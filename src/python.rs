use crate::ast::{Directives, JsonDirective, JsonRepr};
use crate::gen_ast;
use crate::gen_ast::{
    Alias, AliasType, AtomicType, Builtin, Enum, EnumVariant, Field, FieldType, RefType, Struct,
    TopDeclaration, Type, VariantValue,
};
use inflections::Inflect;
use std::collections::{BTreeMap, BTreeSet};
use std::convert::From;
use std::rc::Rc;

pub const SRC_PYTHON_PRELUDE: &'static str = include_str!("prelude.py");

#[allow(dead_code)]
pub fn gen_python(decls: &[gen_ast::TopDeclaration]) -> Vec<PyToken> {
    let mut gen_ctx = GenContext::new(decls);
    let code_tokens: Vec<PyToken> = decls
        .iter()
        .map(|decl| PyToken::Block(gen_python_top_decl(&mut gen_ctx, decl)))
        .collect();

    let imports = py_imports(gather_imports(&code_tokens));
    let mut tokens = vec![];
    tokens.push(imports);
    tokens.push(PyToken::NewLine);
    tokens.push(PyToken::NewLine);
    tokens.extend(intersperce(PyToken::NewLine, code_tokens));

    tokens.push(PyToken::NewLine);
    if let Some(n) = gen_ctx.tuple_arities.clone().iter().max() {
        tokens.push(gen_tuple_typevars(&mut gen_ctx, *n));
        tokens.push(PyToken::NewLine);
    }

    for n in gen_ctx.tuple_arities.iter() {
        tokens.push(gen_tuple_parser(*n));
    }

    tokens.push(PyToken::NewLine);
    for tv in vec!["T", "K", "V"].into_iter() {
        tokens.push(gen_type_var(&mut gen_ctx, tv));
    }

    tokens.push(PyToken::NewLine);
    tokens.push(SRC_PYTHON_PRELUDE.into());

    tokens
}

pub fn render_python(tokens: &[PyToken]) -> String {
    let config = PyConfig { indent_size: 4 };
    let mut ctx = FormatContext::default();
    render(&config, &mut ctx, tokens)
}

/// A struct used to gather various requirements while
/// generating the code.
struct GenContext<'decls> {
    /// The code to parse tuple is very tedious to write manually and thus
    /// instead of putting that in the prelude, track which tuple arities
    /// are required. The parsing code will be generated on the fly
    tuple_arities: BTreeSet<u8>,

    top_declarations: &'decls [TopDeclaration],

    /// Keep track of type variables for mypy so they are not redeclared
    type_variables: BTreeSet<String>,
}

impl<'decls> GenContext<'decls> {
    fn new(decls: &'decls [TopDeclaration]) -> Self {
        GenContext {
            tuple_arities: BTreeSet::new(),
            top_declarations: decls,
            type_variables: BTreeSet::new(),
        }
    }
}

fn gen_python_top_decl(
    mut gen_ctx: &mut GenContext,
    decl: &gen_ast::TopDeclaration,
) -> Vec<PyToken> {
    let mut tokens = Vec::new();

    let decl_tokens = match decl {
        TopDeclaration::Struct(s) => gen_py_struct(&mut gen_ctx, s),
        TopDeclaration::Enum(e) => gen_py_enum(&mut gen_ctx, e),
        TopDeclaration::Alias(a) => gen_py_alias(&mut gen_ctx, a),
    };
    tokens.extend(decl_tokens);
    tokens.push(PyToken::NewLine);

    tokens
}

fn gen_py_struct(mut gen_ctx: &mut GenContext, s: &Struct) -> Vec<PyToken> {
    let mut tokens = Vec::new();
    let struct_td_name = format!("{}JSON", s.name);

    let mut body = vec![];

    // Generate the class attribute with their type
    // Exclude #[typeof] fields
    body.extend(intersperce(
        PyToken::NewLine,
        s.fields
            .iter()
            .filter_map(|f| match &f.typ {
                FieldType::TypeOf(_) => None,
                FieldType::Type(t) => Some((&f.name, t)),
            })
            .map(|(name, typ)| {
                typed_attr(
                    &name.to_snake_case(),
                    vec![typ.get_python_type(&gen_ctx).into()],
                    None,
                )
            })
            .collect(),
    ));

    body.push(PyToken::NewLine);

    for tv in &s.type_parameters {
        tokens.push(gen_type_var(&mut gen_ctx, tv));
    }

    body.push(gen_typevar_dump_fields(&s.type_parameters));

    body.push(PyToken::NewLine);
    body.push(PyToken::NewLine);

    let any = type_import("Any");
    let abc_mapping = PyToken::Import(
        PyImport::Full("collections".to_owned()),
        Some("abc.Mapping".to_owned()),
    );

    let mut from_json_args = vec!["cls".into()];
    from_json_args.extend(s.type_parameters.iter().map(|tv| {
        PyToken::Block(vec![
            format!("json_{}: ", tv).into(),
            gen_generic_parse_dump_hint(tv),
        ])
    }));
    from_json_args.push(typed_attr("data", vec![any], None));

    let mut from_json_body = vec![];

    from_json_body.push(
        If::new(
            PyToken::Block(vec![
                "not isinstance(data, ".into(),
                abc_mapping,
                ")".into(),
            ]),
            "raise ValidationError(message=\"Expected a JSON dictionnary\", data=data)".into(),
        )
        .into(),
    );
    from_json_body.push(PyToken::NewLine);

    // unpack the {to,from}_json functions passed as argument
    from_json_body.extend(s.type_parameters.iter().map(|tv| {
        PyToken::Block(vec![
            format!("parse_{}, dump_{} = json_{}", tv, tv, tv).into(),
            PyToken::NewLine,
        ])
    }));

    from_json_body.push("errors = {}".into());
    from_json_body.push(PyToken::NewLine);
    from_json_body.push(PyToken::NewLine);

    for field in &s.fields {
        // skip #[typeof] fields, generate @properties field for that
        match field.typ {
            FieldType::TypeOf(_) => continue,
            FieldType::Type(_) => (),
        };

        // TODO: perhaps only put the type hint when there are generics involved
        // in this case they are required for mypy not to get confused
        let try_body = PyToken::Block(vec![
            format!("{}: ", field.name).into(),
            field.typ.get_python_type(&gen_ctx),
            format!(
                " = {}",
                field.typ.get_parse_function(
                    &gen_ctx,
                    &format!(r#"data.get("{}")"#, field.name),
                    0
                )
            )
            .into(),
        ]);
        from_json_body.push(py_try(
            try_body,
            vec![(
                "ValidationError as e".into(),
                format!(r#"errors["{}"] = e.messages"#, field.name).into(),
            )],
        ))
    }

    from_json_body.push(PyToken::NewLine);
    from_json_body.push(
        If::new(
            "errors".into(),
            "raise ValidationError(message=errors)".into(),
        )
        .into(),
    );

    from_json_body.push(PyToken::NewLine);
    from_json_body.push("return cls(".into());
    let mut args: Vec<PyToken> = s
        .fields
        .iter()
        .filter_map(|f| match f.typ {
            FieldType::TypeOf(_) => None,
            FieldType::Type(_) => Some(format!("{}", f.name).into()),
        })
        .collect();
    args.extend(
        s.type_parameters
            .iter()
            .map(|tv| format!("dump_{}", tv).into()),
    );
    from_json_body.extend(intersperce(", ".into(), args));
    from_json_body.push(")".into());

    let (from_json_name, from_json_result) = if s.type_parameters.is_empty() {
        ("from_json", format!("\"{}\"", s.name).into())
    } else {
        let type_params = s.type_parameters.join(", ");
        (
            "_from_json",
            format!("\"{}[{}]\"", s.name, type_params).into(),
        )
    };
    let from_json = py_fn(
        from_json_name,
        from_json_args,
        // vec!["cls".into(), typed_attr("data", vec![any], None)],
        from_json_body,
        Some(from_json_result),
    );
    body.push("@classmethod".into());
    body.push(PyToken::NewLine);
    body.extend(from_json);
    body.push(PyToken::NewLine);

    // Generate a @property function for each #[typeof] field
    for (field_name, hint_target) in &s
        .fields
        .iter()
        .filter_map(|f| match &f.typ {
            FieldType::TypeOf(target) => Some((&f.name, target)),
            FieldType::Type(_) => None,
        })
        .collect::<Vec<_>>()
    {
        body.push("@property".into());
        body.push(PyToken::NewLine);
        body.extend(py_fn(
            field_name,
            vec!["self".into()],
            vec![format!("return self.{}.json_type", hint_target).into()],
            Some("str".into()),
        ));
        body.push(PyToken::NewLine)
    }

    let default_dump: Vec<PyToken> = s
        .type_parameters
        .iter()
        .filter_map(|tv| {
            s.fields.iter().find(|f| f.typ.is_typevar(tv)).map(|f| {
                If::new(
                    format!("self._dump_{} is None", tv).into(),
                    format!(
                        "self._dump_{} = get_dump_function(self.{})",
                        tv,
                        f.name.to_snake_case()
                    )
                    .into(),
                )
                .into()
            })
        })
        .collect();

    body.extend(py_fn(
        "to_json",
        vec!["self".into()],
        vec![
            PyToken::Block(default_dump),
            "return {".into(),
            indent(1),
            PyToken::Block(intersperce(
                PyToken::NewLine,
                s.fields
                    .iter()
                    .map(|f| {
                        let name = f.name.to_snake_case();
                        let attr = format!("self.{}", &name.to_snake_case());
                        format!(
                            r#""{}": {},"#,
                            f.name,
                            &f.typ.get_dump_function(&gen_ctx, &attr, 0).unwrap_or(attr)
                        )
                        .into()
                    })
                    .collect(),
            )),
            indent(-1),
            "}".into(),
        ],
        Some(type_import("Any")),
    ));

    tokens.push(PyToken::NewLine);
    let dataclass = PyToken::Import(
        PyImport::Specific("dataclasses".to_owned(), "dataclass".to_owned()),
        None,
    );
    tokens.push("@".into());
    tokens.push(dataclass);
    tokens.push(PyToken::NewLine);

    let class_generics = if s.type_parameters.is_empty() {
        vec![]
    } else {
        vec![PyToken::Block(vec![
            type_import("Generic"),
            "[".into(),
            PyToken::Block(intersperce(
                ", ".into(),
                s.type_parameters
                    .iter()
                    .map(|tv| {
                        gen_ctx.type_variables.insert(tv.to_string());
                        format!("{}", tv).into()
                    })
                    .collect(),
            )),
            "]".into(),
        ])]
    };
    let struct_class = py_class(&s.name, class_generics, body);
    tokens.extend(struct_class);

    tokens
}

fn gen_py_enum(mut gen_ctx: &mut GenContext, e: &Enum) -> Vec<PyToken> {
    let tok = if e.is_simple_enum() {
        gen_simple_enum(&e)
    } else {
        gen_py_sum_type(&mut gen_ctx, &e)
    };

    vec![tok]
}

/// Generate the type hint for the tuple (from_json, to_json) used
/// in the `from_json` class methods
fn gen_generic_parse_dump_hint(tv: &str) -> PyToken {
    let mut tokens = Vec::new();
    tokens.push(type_import("Tuple"));
    tokens.push("[".into());

    tokens.push(type_import("Callable"));
    tokens.push("[[".into());
    tokens.push(type_import("Any"));
    tokens.push(format!("], {}], ", tv).into());

    tokens.push(type_import("Callable"));
    tokens.push(format!("[[{}], ", tv).into());
    tokens.push(type_import("Any"));
    tokens.push("]".into());

    tokens.push("]".into());
    tokens.push(PyToken::NewLine);
    PyToken::Block(tokens)
}

fn gen_py_alias(mut gen_ctx: &mut GenContext, a: &Alias) -> Vec<PyToken> {
    let name = a.name.to_pascal_case();
    match &a.alias {
        AliasType::Atomic(at) => {
            let py_typ = atomic_py_instance(&at).0;
            vec![
                format!("{} = ", name).into(),
                type_import("NewType"),
                format!(r#"("{}", {})"#, name, py_typ).into(),
            ]
        }
        AliasType::Reference(r) => match resolve_final_reference(&r) {
            TopDeclaration::Struct(s) => gen_alias_struct(&mut gen_ctx, &a, &r, &s),
            TopDeclaration::Enum(_) => todo!(),
            TopDeclaration::Alias(_) => unreachable!(),
        },
        AliasType::Builtin(b) => {
            match b {
                Builtin::List(t) => vec![
                    format!("{} = ", name).into(),
                    type_import("NewType"),
                    format!(r#"("{}", "#, name).into(),
                    type_import("List"),
                    "[".into(),
                    t.get_python_type(&gen_ctx),
                    "])".into(),
                ],
                Builtin::Optional(t) => {
                    // Argument 2 to NewType(...) must be subclassable
                    // which is not the case for Optional[x]  :/
                    vec![
                        format!("{} = ", name).into(),
                        type_import("Union"),
                        "[None, ".into(),
                        t.get_python_type(&gen_ctx),
                        "]".into(),
                    ]
                }
                Builtin::Map(_, _) => todo!("map alias"),
            }
        }
    }
}

/// Follow any alias until a non-alias top declaration is found
/// Since dare doesn't support out of order declaration nor recursive
/// types, there cannot be an infinite cycle when resolving references.
fn resolve_final_reference(r: &RefType) -> &TopDeclaration {
    match &*r.target {
        TopDeclaration::Struct(_) | TopDeclaration::Enum(_) => &r.target,
        TopDeclaration::Alias(a) => match &a.alias {
            AliasType::Atomic(_) | AliasType::Builtin(_) => &r.target,
            AliasType::Reference(r2) => resolve_final_reference(&r2),
        },
    }
}

/// Generate a monomorphised variant of the aliased struct (or a dumb copy
/// if the original struct has no type parameter). This is to allow different types, but
/// the same behavior and from/to json functions
fn gen_alias_struct(
    mut gen_ctx: &mut GenContext,
    a: &Alias,
    r: &RefType,
    s: &Struct,
) -> Vec<PyToken> {
    // By that point, all type parameters are already validated, so we
    // can zip and build a hashmap where everything matches.
    let typevar_mapping: BTreeMap<_, _> = s
        .type_parameters
        .iter()
        .zip(r.type_parameters.iter())
        .collect();
    dbg!("typevar mapping: {:#?}", typevar_mapping.clone());
    let fields = s
        .fields
        .iter()
        .map(|f| match &f.typ {
            FieldType::TypeOf(_) => f.clone(),
            FieldType::Type(t) => match t {
                Type::TypeParameter(typevar_name) => {
                    let field_typ = *typevar_mapping.get(&typevar_name).unwrap();
                    Field {
                        typ: FieldType::Type(field_typ.clone()),
                        ..f.clone()
                    }
                }
                _ => f.clone(),
            },
        })
        .collect();

    let s = Struct {
        location: a.location,
        name: a.name.clone(),
        type_parameters: vec![],
        fields,
    };
    gen_py_struct(&mut gen_ctx, &s)
}

fn gather_imports(tokens: &[PyToken]) -> ImportMap {
    let mut import_map = ImportMap::default();

    // include the imports coming from the "stdlib", code not generated
    // but automatically included
    import_map.imports.insert(
        "typing",
        ImportEntry {
            full: false,
            specifics: vec![
                "TypeVar", "Any", "cast", "Dict", "List", "Optional", "Callable", "Union", "Tuple",
            ]
            .into_iter()
            .collect(),
        },
    );

    import_map.imports.insert(
        "base64",
        ImportEntry {
            full: true,
            specifics: BTreeSet::new(),
        },
    );

    import_map.imports.insert(
        "collections",
        ImportEntry {
            full: true,
            specifics: BTreeSet::new(),
        },
    );

    import_map.imports.insert(
        "binascii",
        ImportEntry {
            full: true,
            specifics: BTreeSet::new(),
        },
    );

    for tok in tokens {
        match tok {
            PyToken::Import(imp, _) => match imp {
                PyImport::Full(name) => {
                    let entry = import_map
                        .imports
                        .entry(name.as_str())
                        .or_insert_with(ImportEntry::default);
                    entry.full = true;
                }
                PyImport::Specific(module, name) => {
                    let entry = import_map.imports.entry(module.as_str()).or_default();
                    entry.specifics.insert(name);
                }
                PyImport::Try(prim_module, fallback_module, name) => {
                    import_map.with_fallbacks.insert((
                        prim_module.as_ref(),
                        fallback_module.as_ref(),
                        name.as_ref(),
                    ));
                }
            },
            PyToken::Block(nested_tokens) => {
                import_map.merge(&mut gather_imports(nested_tokens));
            }
            _ => (),
        }
    }
    import_map
}

/// trait to gather the methods for conversion to and from json in python
trait PythonJSON {
    /// Given a `Type`, return a string representing the function to parse json into
    /// that type
    fn get_parse_function(&self, gen_ctx: &GenContext, name: &str, depth: u8) -> String;

    /// Dual of get_parse_function, returns a String representing the function to convert
    /// the given type into json representation
    fn get_dump_function(&self, gen_ctx: &GenContext, name: &str, depth: u8) -> Option<String>;

    /// Returns a python type hint for the type itself
    /// It's usually the name of the type except for sums
    fn get_python_type(&self, gen_ctx: &GenContext) -> PyToken;

    /// Returns a python type hint for the json representation of
    /// this type after serialization.
    fn get_json_type(&self, gen_ctx: &GenContext) -> String;
}

impl PythonJSON for Type {
    fn get_dump_function(&self, gen_ctx: &GenContext, name: &str, depth: u8) -> Option<String> {
        match self {
            Type::Atomic(at) => match at {
                AtomicType::Bytes => {
                    if depth == 0 {
                        Some(format!("dump_bytes({})", name))
                    } else {
                        Some("dump_bytes".to_owned())
                    }
                }
                _ => None,
            },
            Type::Builtin(b) => match b {
                Builtin::List(t) => {
                    match t.get_dump_function(&gen_ctx, name, depth + 1) {
                        // the inner type doesn't need any conversion, so the list
                        // itself also doesn't need conversion
                        None => None,
                        Some(d) => {
                            if depth == 0 {
                                Some(format!("dump_list({}, {})", d, name))
                            } else {
                                Some(format!("lambda x{}: dump_list({}, x{})", depth, d, depth))
                            }
                        }
                    }
                }
                Builtin::Optional(t) => {
                    match t.get_dump_function(&gen_ctx, name, depth + 1) {
                        // the inner type doesn't need any conversion, so the type
                        // itself also doesn't need conversion
                        None => None,
                        Some(d) => {
                            if depth == 0 {
                                Some(format!("dump_optional({}, {})", d, name))
                            } else {
                                Some(format!(
                                    "lambda x{}: dump_optional({}, x{})",
                                    depth, d, depth
                                ))
                            }
                        }
                    }
                }
                Builtin::Map(_k, v) => {
                    // TODO check if the type for the key can be serialised as a string
                    match v.get_dump_function(&gen_ctx, name, depth + 1) {
                        Some(f) => {
                            if depth == 0 {
                                Some(format!("dump_object({}, {})", f, name))
                            } else {
                                Some(format!("lambda x{}: dump_object({}, x{})", depth, f, depth))
                            }
                        }
                        None => None,
                    }
                }
            },
            Type::Reference(r) => {
                // let referenced_decl = &gen_ctx.top_declarations[r.name];
                // TODO check if the referenced top declaration is an alias. In this case
                // simply calling to_json on it will not work
                if depth == 0 {
                    Some(format!("{}.to_json()", name))
                } else {
                    Some("lambda x: x.to_json()".to_owned())
                }
            }
            Type::TypeParameter(tv) => {
                // depth == 0 is used for the .to_json() method
                // while higher depths are used to construct the tuple
                // (from_json, to_json) used in the `from_json` class method
                if depth == 0 {
                    Some(format!("self._dump_{}({})", tv, name))
                } else {
                    Some(format!("lambda x: dump_{}(x)", tv))
                }
            }
        }
    }

    fn get_parse_function(&self, gen_ctx: &GenContext, name: &str, depth: u8) -> String {
        match self {
            Type::Atomic(at) => {
                let (instance, _err) = atomic_py_instance(at);
                if depth == 0 {
                    format!("parse_{}({})", instance, name)
                } else {
                    format!("parse_{}", instance)
                }
            }
            Type::Builtin(builtin_type) => match builtin_type {
                Builtin::Optional(opt_type) => {
                    let nested = opt_type.get_parse_function(&gen_ctx, name, depth + 1);
                    if depth == 0 {
                        format!("parse_optional({}, {})", nested, name)
                    } else {
                        format!("lambda x{}: parse_optional({}, x{})", depth, nested, depth)
                    }
                }
                Builtin::List(inner) => {
                    let nested = inner.get_parse_function(&gen_ctx, name, depth + 1);
                    if depth == 0 {
                        format!("parse_list({}, {})", nested, name)
                    } else {
                        format!("lambda x{}: parse_list({}, x{})", depth, nested, depth)
                    }
                }
                Builtin::Map(key_type, val_type) => {
                    let parse_key_fn =
                        key_type.get_parse_function(&gen_ctx, "key_name?", depth + 1);
                    let parse_val_fn =
                        val_type.get_parse_function(&gen_ctx, "val_name?", depth + 1);
                    format!("parse_map({}, {}, {})", parse_key_fn, parse_val_fn, name)
                }
            },
            Type::Reference(r) => {
                let referenced_decl = r.target.as_ref();
                if r.type_parameters.is_empty() {
                    if depth == 0 {
                        // variant hint only make sense for the first level. It isn't possible to
                        // have #[typeof] refer to things other than Referenced type
                        match &r.variant_hint {
                            Some(hint) => format!(
                                r#"{}.from_json({}, tag=data.get("{}"))"#,
                                referenced_decl.get_name(),
                                name,
                                hint
                            ),
                            None => format!("{}.from_json({})", referenced_decl.get_name(), name),
                        }
                    } else {
                        format!("{}.from_json", referenced_decl.get_name(),)
                    }
                } else {
                    let parse_typevar_tuples = r
                        .type_parameters
                        .iter()
                        .map(|tv| {
                            format!(
                                "({}, {})",
                                // name is irrelevant since the depth is always 1
                                tv.get_parse_function(&gen_ctx, "", 1),
                                tv.get_dump_function(&gen_ctx, "", 1)
                                    .unwrap_or("identity".to_string())
                            )
                        })
                        .collect::<Vec<_>>()
                        .join(", ");

                    if depth == 0 {
                        match &r.variant_hint {
                            Some(hint) => format!(
                                r#"{}._from_json({}, {}, tag=data.get("{}"))"#,
                                referenced_decl.get_name(),
                                parse_typevar_tuples,
                                name,
                                hint
                            ),
                            None => format!(
                                "{}._from_json({}, {})",
                                referenced_decl.get_name(),
                                parse_typevar_tuples,
                                name
                            ),
                        }
                    } else {
                        format!(
                            "lambda x{}: {}._from_json({}, x{})",
                            depth,
                            referenced_decl.get_name(),
                            parse_typevar_tuples,
                            depth
                        )
                    }
                }
            }
            Type::TypeParameter(tv) => {
                if depth == 0 {
                    format!("parse_{}({})", tv, name)
                } else {
                    format!("parse_{}", tv)
                }
            }
        }
    }

    fn get_python_type(&self, gen_ctx: &GenContext) -> PyToken {
        match self {
            Type::Atomic(at) => atomic_py_instance(at).0.into(),
            Type::Reference(r) => {
                if r.type_parameters.is_empty() {
                    r.target.get_py_python_type().into()
                } else {
                    let mut params = vec![r.target.get_py_python_type().into()];
                    params.push("[".into());
                    params.extend(intersperce(
                        ", ".into(),
                        r.type_parameters
                            .iter()
                            .map(|tv| tv.get_python_type(&gen_ctx))
                            .collect(),
                    ));
                    params.push("]".into());
                    PyToken::Block(params)
                }
            }
            Type::Builtin(b) => match b {
                Builtin::List(inner) => {
                    let list = type_import("List");
                    PyToken::Block(vec![
                        list,
                        "[".into(),
                        inner.get_python_type(&gen_ctx),
                        "]".into(),
                    ])
                }
                Builtin::Optional(inner) => {
                    let opt = type_import("Optional");
                    PyToken::Block(vec![
                        opt,
                        "[".into(),
                        inner.get_python_type(&gen_ctx),
                        "]".into(),
                    ])
                }
                Builtin::Map(k, v) => {
                    let dict = type_import("Mapping");
                    PyToken::Block(vec![
                        dict,
                        "[".into(),
                        k.get_python_type(&gen_ctx),
                        ", ".into(),
                        v.get_python_type(&gen_ctx),
                        "]".into(),
                    ])
                }
            },

            Type::TypeParameter(tv) => tv[..].into(),
        }
    }

    fn get_json_type(&self, gen_ctx: &GenContext) -> String {
        match &self {
            Type::Atomic(t) => match t {
                AtomicType::Str => "str",
                AtomicType::UInt => "int",
                AtomicType::Int => "int",
                AtomicType::Int8 => "int",
                AtomicType::Int16 => "int",
                AtomicType::Int32 => "int",
                AtomicType::Int64 => "int",
                AtomicType::UInt8 => "int",
                AtomicType::UInt16 => "int",
                AtomicType::UInt32 => "int",
                AtomicType::UInt64 => "int",
                AtomicType::Float => "float",
                AtomicType::Bool => "bool",
                // bytes get converted to base64
                AtomicType::Bytes => "str",
            }
            .to_owned(),
            Type::Builtin(b) => match b {
                Builtin::List(inner) => format!("List[{}]", inner.get_json_type(&gen_ctx)),
                Builtin::Optional(inner) => format!("Optional[{}]", inner.get_json_type(&gen_ctx)),
                Builtin::Map(k, v) => format!(
                    "Mapping[{}, {}]",
                    k.get_json_type(&gen_ctx),
                    v.get_json_type(&gen_ctx)
                ),
            },
            Type::Reference(r) => r.target.get_py_json_type(),
            Type::TypeParameter(tv) => tv.clone(),
        }
    }
}

impl FieldType {
    /// to test if a FieldType is exactly a given type parameter
    fn is_typevar(&self, tv: &str) -> bool {
        match &self {
            FieldType::Type(Type::TypeParameter(x)) if x == tv => true,
            _ => false,
        }
    }
}

impl PythonJSON for FieldType {
    fn get_parse_function(&self, gen_ctx: &GenContext, name: &str, depth: u8) -> String {
        match self {
            FieldType::TypeOf(_) => {
                Type::Atomic(AtomicType::Str).get_parse_function(&gen_ctx, name, depth)
            }
            FieldType::Type(t) => t.get_parse_function(&gen_ctx, name, depth),
        }
    }

    fn get_dump_function(&self, gen_ctx: &GenContext, name: &str, depth: u8) -> Option<String> {
        match self {
            FieldType::TypeOf(_) => {
                Type::Atomic(AtomicType::Str).get_dump_function(&gen_ctx, name, depth)
            }
            FieldType::Type(t) => t.get_dump_function(&gen_ctx, name, depth),
        }
    }

    fn get_python_type(&self, gen_ctx: &GenContext) -> PyToken {
        match self {
            FieldType::TypeOf(_) => "str".into(),
            FieldType::Type(t) => t.get_python_type(gen_ctx),
        }
    }

    fn get_json_type(&self, gen_ctx: &GenContext) -> String {
        match self {
            FieldType::TypeOf(_) => "str".to_string(),
            FieldType::Type(t) => t.get_json_type(&gen_ctx),
        }
    }
}

fn gen_simple_enum(e: &Enum) -> PyToken {
    let enum_class = PyToken::Import(
        PyImport::Specific("enum".to_owned(), "Enum".to_owned()),
        None,
    );

    let attributes = intersperce(
        PyToken::NewLine,
        e.variants
            .iter()
            .map(|variant| {
                let val = variant.alias.as_ref().unwrap_or(&variant.name);
                format!("{} = \"{}\"", variant.name.to_pascal_case(), val).into()
            })
            .collect(),
    );

    let mut body = Vec::new();
    body.extend(attributes);
    body.push(PyToken::NewLine);
    body.push(PyToken::NewLine);

    let cast = PyImport::Specific("typing".to_owned(), "cast".to_owned());
    body.extend(py_fn(
        "to_json",
        vec!["self".into()],
        vec![
            "return ".into(),
            PyToken::Import(cast, None),
            format!("(str, self.value)").into(),
        ],
        Some(type_import("Any")),
        // Some("str".into()),
    ));
    body.push(PyToken::NewLine);
    body.push(PyToken::NewLine);

    let any = PyToken::Import(
        PyImport::Specific("typing".to_owned(), "Any".to_owned()),
        None,
    );

    let mut from_json_body = vec![];
    for variant in &e.variants {
        let val = variant.alias.as_ref().unwrap_or(&variant.name);
        from_json_body.push(
            If::new(
                format!(r#"raw == "{}""#, val).into(),
                format!("return cls.{}", variant.name.to_pascal_case()).into(),
            )
            .into(),
        );
    }
    from_json_body.push(
        format!(
            r#"raise ValidationError("Invalid {} value: {{}}".format(raw))"#,
            e.name.to_pascal_case()
        )
        .into(),
    );

    body.push("@classmethod".into());
    body.push(PyToken::NewLine);
    body.extend(py_fn(
        "from_json",
        vec!["cls".into(), typed_attr("raw", vec![any], None)],
        from_json_body,
        Some(format!(r#""{}""#, e.name).into()),
    ));

    PyToken::Block(py_class(&e.name.to_pascal_case(), vec![enum_class], body))
}

fn gen_py_sum_type(mut gen_ctx: &mut GenContext, e: &Enum) -> PyToken {
    let mut tokens = vec![];
    // let mut variants_type_names = vec![];

    for tv in &e.type_parameters {
        tokens.push(gen_type_var(&mut gen_ctx, tv));
    }

    for variant in &e.variants {
        let tok = gen_enum_variant(&mut gen_ctx, &variant, &e.directives);
        tokens.push(tok);
        tokens.push(PyToken::NewLine);
        // variants_type_names.push(name);
    }

    // the union of all variants
    tokens.push(format!("{}TypeDef = ", e.name).into());
    tokens.push(typed_union(
        e.variants
            .iter()
            .map(|v| {
                let mut toks = vec![v.name.clone().into()];
                let tvs = v.value.get_type_vars();
                if !tvs.is_empty() {
                    toks.push("[".into());
                    toks.push(tvs.join(", ").into());
                    toks.push("]".into());
                }
                PyToken::Block(toks)
            })
            .collect(),
    ));
    tokens.push(PyToken::NewLine);
    tokens.push(PyToken::NewLine);

    tokens.push(gen_from_json_enum(&mut gen_ctx, &e));

    PyToken::Block(tokens)
}

fn gen_from_json_enum(_gen_ctx: &GenContext, e: &Enum) -> PyToken {
    let mut from_json_body = vec![];

    let json_directive = e
        .directives
        .json
        .as_ref()
        .map(|x| x.1.clone())
        .unwrap_or_default();
    let tag_name = json_directive.tag;
    let content_name = json_directive.content;

    match &json_directive.repr {
        JsonRepr::Object => {
            from_json_body.push(
                If::new(
                    "not isinstance(data, dict)".into(),
                    r#"raise ValidationError(message="Not a dictionnary")"#.into(),
                )
                .into(),
            );
            from_json_body.push(format!(r#"tag = data.get("{}")"#, tag_name).into());
            from_json_body.push(PyToken::NewLine);
            from_json_body.push(format!(r#"contents = data.get("{}")"#, content_name).into());
            from_json_body.push(PyToken::NewLine);
        }
        JsonRepr::Tuple => {
            from_json_body.push(
                If::new(
                    "not isinstance(data, (list, tuple))".into(),
                    r#"raise ValidationError(message="Not a list nor a tuple")"#.into(),
                )
                .into(),
            );
            from_json_body.push(
                If::new(
                    "not data".into(),
                    r#"raise ValidationError(message="Need at least one element to parse")"#.into(),
                )
                .into(),
            );
            from_json_body.push(r#"tag = data[0]"#.into());
            from_json_body.push(PyToken::NewLine);
            from_json_body.push(r#"contents = data[1:]"#.into());
            from_json_body.push(PyToken::NewLine);
        }
        JsonRepr::Union => {
            from_json_body.push("contents = data".into());
            from_json_body.push(PyToken::NewLine);
        }
    }

    for variant in &e.variants {
        let arg = match variant.value {
            VariantValue::OnlyCtor => "",
            _ => "contents",
        };
        from_json_body.push(
            If::new(
                format!(
                    r#"tag == "{}""#,
                    variant.alias.as_ref().unwrap_or(&variant.name)
                )
                .into(),
                if arg.is_empty() {
                    format!("return {}({})", variant.name.to_pascal_case(), arg).into()
                } else {
                    let fn_name = if variant.value.get_type_vars().is_empty() {
                        "from_json"
                    } else {
                        "_from_json"
                    };
                    let mut return_toks = vec![format!(
                        "return {}.{}({}",
                        variant.name.to_pascal_case(),
                        fn_name,
                        arg
                    )
                    .into()];

                    for tv in variant.value.get_type_vars() {
                        return_toks.push(format!(", json_{}", tv).into());
                    }
                    return_toks.push(")".into());
                    PyToken::Block(return_toks)
                },
            )
            .into(),
        );
    }

    match json_directive.repr {
        JsonRepr::Object | JsonRepr::Tuple => {
            from_json_body.push(
                format!(r#"raise ValidationError(message="unknown tag {{}}".format(tag))"#).into(),
            );
        }
        JsonRepr::Union => {
            from_json_body.push(
                If::new(
                    "tag is not None".into(),
                    format!(r#"raise ValidationError(message="unknown tag {{}}".format(tag))"#)
                        .into(),
                )
                .into(),
            );

            from_json_body.push(PyToken::NewLine);
            let (only_ctors, other_ctors): (Vec<_>, Vec<_>) =
                e.variants.iter().partition(|v| v.value.is_only_ctor());
            if !only_ctors.is_empty() {
                from_json_body.push(
                    If::new(
                        "contents is None".into(),
                        format!("return {}()", only_ctors[0].name.to_pascal_case()).into(),
                    )
                    .into(),
                );
            }

            for v in other_ctors.iter() {
                let suppress = PyToken::Import(
                    PyImport::Specific("contextlib".to_string(), "suppress".to_string()),
                    None,
                );
                from_json_body.extend(vec![
                    "with ".into(),
                    suppress,
                    "(ValidationError):".into(),
                    indent(1),
                ]);

                // TODO use get_parse_function instead of that hardcoded stuff
                // from_json_body.push(format!("return {}", v.value));

                let tvs = v.value.get_type_vars();
                if tvs.is_empty() {
                    from_json_body.push(
                        format!("return {}.from_json(contents", v.name.to_pascal_case()).into(),
                    );
                } else {
                    from_json_body
                        .push(format!("return {}._from_json(", v.name.to_pascal_case()).into());
                    for tv in tvs {
                        from_json_body.push(format!("parse_{}, ", tv).into());
                    }
                    from_json_body.push("contents".into());
                };

                from_json_body.push(")".into());
                from_json_body.push(indent(-1));
            }

            from_json_body
                .push(r#"raise ValidationError(message="Could not parse any variant")"#.into());

            from_json_body.push(PyToken::NewLine);
        }
    }

    let mut from_json_args = vec!["cls".into()];

    for tv in &e.type_parameters {
        from_json_args.push(PyToken::Block(vec![
            format!("json_{}: ", tv).into(),
            gen_generic_parse_dump_hint(tv),
        ]));
    }
    from_json_args.push(typed_attr("data", vec!["Any".into()], None));

    match json_directive.repr {
        JsonRepr::Union => from_json_args.push(typed_attr(
            "tag",
            vec!["Optional[str]".into()],
            Some("None".into()),
        )),
        _ => {}
    };

    let (from_json_name, from_json_result) = if e.type_parameters.is_empty() {
        ("from_json", format!("\"{}TypeDef\"", e.name).into())
    } else {
        let type_params = e.type_parameters.join(", ");
        (
            "_from_json",
            format!("\"{}TypeDef[{}]\"", e.name, type_params).into(),
        )
    };

    let super_classes = if e.type_parameters.is_empty() {
        vec![]
    } else {
        let mut toks = vec![type_import("Generic"), "[".into()];
        toks.push(e.type_parameters.join(", ").into());
        toks.push("]".into());
        vec![PyToken::Block(toks)]
    };

    PyToken::Block(py_class(
        &e.name.to_pascal_case(),
        super_classes,
        vec![
            "@classmethod".into(),
            PyToken::NewLine,
            PyToken::Block(py_fn(
                from_json_name,
                from_json_args,
                vec![py_try(
                    PyToken::Block(from_json_body),
                    vec![(
                        "ValidationError as e".into(),
                        PyToken::Block(vec![
                            "e.data = data".into(),
                            PyToken::NewLine,
                            "raise e".into(),
                        ]),
                    )],
                )],
                Some(from_json_result),
            )),
        ],
    ))
}

/// Returns the tokens for the variant
fn gen_enum_variant(
    mut gen_ctx: &mut GenContext,
    variant: &EnumVariant,
    directives: &Directives,
) -> PyToken {
    let mut tokens = Vec::new();
    let dataclass = PyToken::Import(
        PyImport::Specific("dataclasses".to_owned(), "dataclass".to_owned()),
        None,
    );

    tokens.push(PyToken::NewLine);

    tokens.push(PyToken::NewLine);
    tokens.push(PyToken::NewLine);

    let mut class_body = vec![];

    class_body.push(gen_enum_variant_from_json(
        &mut gen_ctx,
        &variant,
        &directives,
    ));

    class_body.push(gen_json_type_enum_variant(variant));
    tokens.push(PyToken::NewLine);

    class_body.push(gen_to_json_enum_variant(&gen_ctx, &variant, &directives));

    tokens.push("@".into());
    tokens.push(dataclass);
    tokens.push(PyToken::NewLine);
    let generic_class_params: Vec<PyToken> = variant
        .value
        .get_type_vars()
        .into_iter()
        .map(|tv| tv.into())
        .collect();

    tokens.extend(py_class(
        &variant.name.to_pascal_case(),
        if generic_class_params.is_empty() {
            vec![]
        } else {
            vec![PyToken::Block(vec![
                type_import("Generic"),
                "[".into(),
                PyToken::Block(intersperce(", ".into(), generic_class_params)),
                "]".into(),
            ])]
        },
        class_body,
    ));

    PyToken::Block(tokens)
}

/// Generate all the _dump_T fields to serialize the corresponding generic type
fn gen_typevar_dump_fields(type_vars: &Vec<String>) -> PyToken {
    PyToken::Block(intersperce(
        PyToken::NewLine,
        type_vars
            .iter()
            .map(|tv| {
                typed_attr(
                    &format!("_dump_{}", tv),
                    vec![PyToken::Block(vec![
                        type_import("Optional"),
                        "[".into(),
                        type_import("Callable"),
                        format!("[[{}], ", tv).into(),
                        type_import("Any"),
                        "]] = ".into(),
                        PyToken::Import(
                            PyImport::Full("dataclasses".to_string()),
                            Some("field".to_string()),
                        ),
                        format!("(repr=False, default=None)").into(),
                    ])],
                    None,
                )
            })
            .collect(),
    ))
}

/// generate the property to get the json tag for this variant
fn gen_json_type_enum_variant(variant: &EnumVariant) -> PyToken {
    let mut tokens = vec!["@property".into(), PyToken::NewLine];

    let tag_val = variant.alias.as_ref().unwrap_or(&variant.name);
    tokens.extend(py_fn(
        "json_type",
        vec!["self".into()],
        vec![
            r#""Returns the json tag for this variant.""#.into(),
            PyToken::NewLine,
            format!(r#"return "{}""#, tag_val).into(),
        ],
        Some("str".into()),
    ));
    tokens.push(PyToken::NewLine);

    PyToken::Block(tokens)
}

/// name and tokens to describe the return type of .to_json()
fn gen_enum_variant_json_type(
    mut gen_ctx: &mut GenContext,
    variant: &EnumVariant,
    directives: &Directives,
) -> (String, PyToken) {
    let x = directives.json.as_ref().map(|x| x.1.repr);
    let repr = x.unwrap_or_default();
    match repr {
        JsonRepr::Object => gen_enum_variant_typedict(&mut gen_ctx, &variant, directives),
        JsonRepr::Tuple => gen_enum_variant_tuple(&mut gen_ctx, &variant),
        JsonRepr::Union => gen_enum_variant_union(&mut gen_ctx, &variant),
    }
}

fn gen_enum_variant_typedict(
    gen_ctx: &mut GenContext,
    variant: &EnumVariant,
    directives: &Directives,
) -> (String, PyToken) {
    let tag_name = directives
        .json
        .as_ref()
        .map_or_else(|| JsonDirective::default().tag, |(_, x)| x.tag.clone());
    let content_name = directives.json.as_ref().map_or_else(
        || JsonDirective::default().content,
        |(_, x)| x.content.clone(),
    );

    let tag_val = variant.alias.as_ref().unwrap_or(&variant.name);
    let mut td_class_body = vec![typed_attr(
        &tag_name,
        vec![type_import("Literal"), format!(r#"["{}"]"#, tag_val).into()],
        None,
    )];

    let td_name = format!("{}JSON", variant.name.to_pascal_case());

    match &variant.value {
        VariantValue::OnlyCtor => (),
        VariantValue::PositionalCtor(refs) => {
            td_class_body.push(PyToken::NewLine);
            if refs.len() == 1 {
                td_class_body.push(typed_attr(
                    &content_name,
                    vec![refs[0].get_json_type(&gen_ctx).into()],
                    None,
                ));
            } else {
                let mut typs = vec![type_import("Tuple")];
                typs.push("[".into());
                typs.extend(intersperce(
                    ", ".into(),
                    refs.iter()
                        .map(|t| t.get_json_type(&gen_ctx).into())
                        .collect(),
                ));
                typs.push("]".into());
                td_class_body.push(typed_attr(&content_name, typs, None));
            }
        }
    }

    let mut tokens = vec![];

    tokens.push(PyToken::NewLine);

    tokens.append(&mut py_class(
        &td_name,
        vec![type_import("TypedDict")],
        td_class_body,
    ));

    (td_name, PyToken::Block(tokens))
}

fn gen_enum_variant_tuple(
    mut gen_ctx: &mut GenContext,
    variant: &EnumVariant,
) -> (String, PyToken) {
    let tuple = type_import("Tuple");
    let tuple_name = format!("{}JSON", variant.name);
    let mut tuple_args = vec!["str".into()];
    match &variant.value {
        VariantValue::OnlyCtor => {}
        VariantValue::PositionalCtor(refs) => {
            for r in refs {
                tuple_args.push(", ".into());
                tuple_args.push(r.get_json_type(&gen_ctx).into());
            }
        }
    }

    let mut tokens = vec![];

    tokens.push(PyToken::NewLine);
    tokens.append(&mut vec![
        format!("{} = ", tuple_name).into(),
        tuple,
        "[".into(),
        PyToken::Block(tuple_args),
        "]".into(),
    ]);

    (tuple_name.clone(), PyToken::Block(tokens))
}

fn gen_enum_variant_union(
    mut gen_ctx: &mut GenContext,
    variant: &EnumVariant,
) -> (String, PyToken) {
    match &variant.value {
        VariantValue::OnlyCtor => ("None".to_string(), PyToken::Block(vec![])),
        VariantValue::PositionalCtor(refs) => {
            let name = format!("{}JSON", variant.name.to_pascal_case());
            if refs.len() == 1 {
                return (
                    name.clone(),
                    format!("{} = {}", name, refs[0].get_json_type(&gen_ctx)).into(),
                );
            }

            let mut tokens = vec![];

            tokens.push(PyToken::NewLine);

            tokens.append(&mut vec![
                format!("{} = ", name.clone()).into(),
                type_import("Tuple"),
            ]);
            tokens.push("[".into());
            tokens.extend(intersperce(
                ", ".into(),
                refs.iter()
                    .map(|t| t.get_json_type(&gen_ctx).into())
                    .collect(),
            ));
            tokens.push("]".into());

            (name, PyToken::Block(tokens))
        }
    }
}

/// Generate the from_json class method for a given enum's variant.
/// Also generate tokens for the `inner` attribute
fn gen_enum_variant_from_json(
    gen_ctx: &mut GenContext,
    variant: &EnumVariant,
    directives: &Directives,
) -> PyToken {
    let mut from_json_body = vec![];
    let parse_generic_functions = variant
        .value
        .get_type_vars()
        .into_iter()
        .map(|tv| format!("parse_{}, dump_{} = json_{}", tv, tv, tv).into())
        .collect();

    let tvs = variant.value.get_type_vars();
    let return_type = if tvs.is_empty() {
        Some(format!(r#""{}""#, variant.name.to_pascal_case()).into())
    } else {
        let mut rt: String = format!("\"{}[", variant.name.to_pascal_case());
        for x in intersperce(", ".to_string(), tvs) {
            rt += &x;
        }
        rt += "]\"";
        Some(rt.into())
    };

    match &variant.value {
        VariantValue::OnlyCtor => {}

        VariantValue::PositionalCtor(refs) => {
            if refs.len() == 1 {
                from_json_body.push(typed_attr(
                    "inner".into(),
                    vec![refs[0].to_py_token(&gen_ctx)],
                    None,
                ));

                from_json_body.push(PyToken::NewLine);
                from_json_body.push(gen_typevar_dump_fields(&variant.value.get_type_vars()));

                from_json_body.push(PyToken::NewLine);

                let json_directive = directives
                    .json
                    .as_ref()
                    .map(|x| x.1.clone())
                    .unwrap_or_default();

                let parse_fn = match &json_directive.repr {
                    JsonRepr::Object | JsonRepr::Union => format!(
                        "inner = {}",
                        (&refs[0]).get_parse_function(&gen_ctx, "data", 0)
                    ),
                    JsonRepr::Tuple => format!(
                        "inner = parse_singleton({}, {})",
                        (&refs[0]).get_parse_function(&gen_ctx, "", 1),
                        "data"
                    ),
                };

                let mut cls_args: Vec<PyToken> = vec!["inner".into()];
                cls_args.append(
                    &mut variant
                        .value
                        .get_type_vars()
                        .into_iter()
                        .map(|tv| format!("dump_{}", tv).into())
                        .collect(),
                );

                let json_args = variant
                    .value
                    .get_type_vars()
                    .iter()
                    .map(|tv| {
                        PyToken::Block(vec![
                            format!("json_{}: ", tv).into(),
                            gen_generic_parse_dump_hint(tv),
                        ])
                    })
                    .collect();

                from_json_body.push("@classmethod".into());
                from_json_body.push(PyToken::NewLine);
                let fn_name = if variant.value.get_type_vars().is_empty() {
                    "from_json"
                } else {
                    "_from_json"
                };

                from_json_body.extend(py_fn(
                    fn_name.into(),
                    vec![
                        "cls".into(),
                        typed_attr("data", vec![type_import("Any")], None),
                        PyToken::Block(intersperce(", ".into(), json_args)),
                    ],
                    vec![
                        PyToken::Block(intersperce(PyToken::NewLine, parse_generic_functions)),
                        PyToken::NewLine,
                        parse_fn.into(),
                        PyToken::NewLine,
                        "return cls(".into(),
                        PyToken::Block(intersperce(", ".into(), cls_args)),
                        ")".into(),
                    ],
                    return_type,
                ));

                from_json_body.push(PyToken::NewLine);
            } else {
                gen_ctx.tuple_arities.insert(refs.len() as u8);
                let tuple = PyToken::Import(
                    PyImport::Specific("typing".to_owned(), "Tuple".to_owned()),
                    None,
                );
                from_json_body.push("inner: ".into());
                from_json_body.push(tuple);
                from_json_body.push("[".into());
                from_json_body.push(PyToken::Block(intersperce(
                    ", ".into(),
                    refs.iter().map(|r| r.to_py_token(&gen_ctx)).collect(),
                )));
                from_json_body.push("]".into());

                from_json_body.push(PyToken::NewLine);
                from_json_body.push(gen_typevar_dump_fields(&variant.value.get_type_vars()));

                from_json_body.push(PyToken::NewLine);

                let mut parse_fns: Vec<_> = refs
                    .iter()
                    .map(|t| t.get_parse_function(&gen_ctx, "", 1))
                    .collect();
                parse_fns.push("data".to_owned());

                let json_args = variant
                    .value
                    .get_type_vars()
                    .iter()
                    .map(|tv| {
                        PyToken::Block(vec![
                            format!("json_{}: ", tv).into(),
                            gen_generic_parse_dump_hint(tv),
                        ])
                    })
                    .collect();

                from_json_body.push("@classmethod".into());
                from_json_body.push(PyToken::NewLine);
                from_json_body.extend(py_fn(
                    "from_json".into(),
                    vec![
                        "cls".into(),
                        typed_attr("data", vec![type_import("Any")], None),
                        PyToken::Block(intersperce(", ".into(), json_args)),
                    ],
                    vec![
                        PyToken::Block(intersperce(PyToken::NewLine, parse_generic_functions)),
                        PyToken::NewLine,
                        format!(
                            r#"inner = parse_tuple_{}({})"#,
                            refs.len(),
                            parse_fns.join(", ")
                        )
                        .into(),
                        PyToken::NewLine,
                        "return cls(inner)".into(),
                    ],
                    return_type,
                ));

                from_json_body.push(PyToken::NewLine);
            }
        }
    }

    PyToken::Block(from_json_body)
}

/// Generate the function to_json(self) -> JSONType for a given enum's variant
fn gen_to_json_enum_variant(
    gen_ctx: &GenContext,
    variant: &EnumVariant,
    directives: &Directives,
) -> PyToken {
    let json_directive = directives
        .json
        .as_ref()
        .map(|x| x.1.clone())
        .unwrap_or_default();
    let tag_name = json_directive.tag;
    let tag_val = variant.alias.as_ref().unwrap_or(&variant.name);
    let content_name = json_directive.content;

    let mut function_body = vec![];

    match &variant.value {
        VariantValue::OnlyCtor => match &json_directive.repr {
            JsonRepr::Object => {
                function_body.push(format!(r#"return {{"{}": "{}"}}"#, tag_name, tag_val).into());
            }
            JsonRepr::Tuple => {
                function_body.push(format!(r#"return ("{}",)"#, tag_val).into());
            }
            JsonRepr::Union => {
                function_body.push("return None".into());
            }
        },

        VariantValue::PositionalCtor(refs) => {
            let to_json_contents = if refs.len() == 1 {
                match &refs[0] {
                    Type::TypeParameter(tv) => {
                        function_body.push(
                            If::new(
                                format!("self._dump_{} is None", tv).into(),
                                format!("self._dump_{} = get_dump_function(self.inner)", tv).into(),
                            )
                            .into(),
                        );
                        function_body.push(PyToken::NewLine);
                    }
                    _ => (),
                };

                let val_name = "self.inner".to_string();
                vec![(&refs[0])
                    .get_dump_function(&gen_ctx, &val_name, 0)
                    .unwrap_or(val_name)
                    .into()]
            } else {
                // In case the self._dump_T functions are null, generate them
                let default_dump = refs
                    .iter()
                    .enumerate()
                    .filter_map(|(idx, r)| match r {
                        Type::TypeParameter(tv) => Some(
                            If::new(
                                format!("self._dump_{} is None", tv).into(),
                                format!(
                                    "self._dump_{} = get_dump_function(self.inner[{}])",
                                    tv, idx
                                )
                                .into(),
                            )
                            .into(),
                        ),
                        _ => None,
                    })
                    .collect();
                function_body.push(PyToken::Block(default_dump));

                refs.iter()
                    .enumerate()
                    .map(|(idx, r)| {
                        let val_name = format!("self.inner[{}]", idx);
                        r.get_dump_function(&gen_ctx, &val_name, 0)
                            .unwrap_or(val_name)
                            .into()
                    })
                    .collect::<Vec<_>>()
            };

            match &json_directive.repr {
                JsonRepr::Object => {
                    function_body.push(
                        format!(
                            r#"return {{"{}": "{}", "{}": ("#,
                            tag_name, tag_val, content_name,
                        )
                        .into(),
                    );
                    function_body.extend(intersperce(", ".into(), to_json_contents));
                    function_body.push(")}".into());
                }
                JsonRepr::Tuple => {
                    function_body.push(format!(r#"return ("{}", "#, tag_val).into());
                    function_body.extend(intersperce(", ".into(), to_json_contents));
                    function_body.push(")".into());
                }
                JsonRepr::Union => {
                    function_body.push("return (".into());
                    function_body.extend(intersperce(", ".into(), to_json_contents));
                    function_body.push(")".into());
                }
            }
        } // VariantValue::StructCtor(_) => todo!("StructCtor not implemented"),
    };

    PyToken::Block(py_fn(
        "to_json",
        vec!["self".into()],
        function_body,
        Some(type_import("Any")),
        // Some(json_type.into()),
    ))
}

fn gen_tuple_typevars(mut gen_ctx: &mut GenContext, n: u8) -> PyToken {
    let mut tokens = vec![];
    // let tv = PyToken::Import(
    //     PyImport::Specific("typing".to_owned(), "TypeVar".to_owned()),
    //     None,
    // );
    for i in 1..=n {
        tokens.push(gen_type_var(&mut gen_ctx, &format!("T{}", i)));
        // tokens.push(format!("T{} = ", i).into());
        // tokens.push(tv.clone());
        // tokens.push(format!(r#"("T{}")"#, i).into());
        tokens.push(PyToken::NewLine);
    }

    tokens.push(PyToken::NewLine);
    PyToken::Block(tokens)
}

/// Generate code to parse a tuple of length `n`
fn gen_tuple_parser(n: u8) -> PyToken {
    let mut tokens = vec![];
    let callable = type_import("Callable");
    let tuple = type_import("Tuple");

    let fn_name = format!("parse_tuple_{}", n);
    let mut fn_args: Vec<PyToken> = (1..=n)
        .into_iter()
        .map(|i| {
            let typ = vec![callable.clone(), format!("[[Any], T{}]", i).into()];
            typed_attr(&format!("parse{}", i), typ, None)
        })
        .collect();
    fn_args.push("v: Any".into());

    let mut return_type = vec![tuple, "[".into()];
    return_type.extend(intersperce(
        ", ".into(),
        (1..=n)
            .into_iter()
            .map(|i| format!("T{}", i).into())
            .collect(),
    ));
    return_type.push("]".into());

    let mut body = vec![];

    body.extend(vec![
        If::new("not isinstance(v, list)".into(),
            r#"raise ValidationError(message="Not a list")"#.into()
        ).into(),
        If::new(
            format!("len(v) != {}", n).into(),
            format!(
                r#"raise ValidationError(message="Expected {} values but got {{}}".format(len(v)))"#, n
            ).into()
        ).into(),
    ]);

    body.push(PyToken::NewLine);
    body.push(PyToken::NewLine);
    body.push("errors = {}".into());
    body.push(PyToken::NewLine);

    for i in 1..=n {
        body.push(py_try(
            format!("v{} = parse{}(v[{}])", i, i, i - 1).into(),
            vec![(
                "ValidationError as e".into(),
                format!(r#"errors["{}"] = e.messages"#, i).into(),
            )],
        ));
        body.push(PyToken::NewLine);
    }

    body.push(PyToken::NewLine);

    body.push(
        If::new(
            "errors".into(),
            "raise ValidationError(message=errors)".into(),
        )
        .into(),
    );

    body.push(PyToken::NewLine);
    body.push("return (".into());
    body.extend(intersperce(
        ", ".into(),
        (1..=n)
            .into_iter()
            .map(|i| format!("v{}", i).into())
            .collect(),
    ));
    body.push(")".into());

    tokens.extend(py_fn(
        &fn_name,
        fn_args,
        body,
        Some(PyToken::Block(return_type)),
    ));

    PyToken::Block(tokens)
}

struct If {
    condition: PyToken,
    body: PyToken,
}

impl If {
    fn new(condition: PyToken, body: PyToken) -> Self {
        If { condition, body }
    }
}

impl From<If> for PyToken {
    fn from(x: If) -> Self {
        PyToken::Block(vec![
            "if ".into(),
            x.condition,
            ":".into(),
            indent(1),
            x.body,
            indent(-1),
        ])
    }
}

fn render(conf: &PyConfig, mut ctx: &mut FormatContext, tokens: &[PyToken]) -> String {
    let mut buf: Vec<String> = Vec::new();

    for tok in tokens.iter() {
        match tok.clone() {
            PyToken::Raw(s) => {
                if ctx.should_indent && ctx.indent_level > 0 {
                    let indent_prefix = " ".repeat(ctx.indent_level * conf.indent_size);
                    buf.push(indent_prefix);
                    ctx.should_indent = false;
                }
                buf.push(s);
            }
            PyToken::Import(imp, inner) => {
                if ctx.should_indent && ctx.indent_level > 0 {
                    let indent_prefix = " ".repeat(ctx.indent_level * conf.indent_size);
                    buf.push(indent_prefix);
                }
                match inner {
                    None => buf.push(imp.sym()),
                    Some(rest) => {
                        buf.push(imp.sym());
                        buf.push(format!(".{}", rest));
                    }
                }
                ctx.should_indent = false;
            }
            PyToken::Space => {
                buf.push(" ".to_owned());
                ctx.should_indent = false;
            }
            PyToken::NewLine => {
                buf.push("\n".to_owned());
                ctx.should_indent = true;
            }
            PyToken::Indent(i) => {
                let il = ctx.indent_level as i8;
                buf.push("\n".to_owned());
                ctx.indent_level = (il + i) as usize;
                ctx.should_indent = true;
            }
            PyToken::Block(toks) => {
                buf.push(render(&conf, &mut ctx, &toks));
            }
            PyToken::Empty => continue,
        }
    }
    buf.join("")
}

#[derive(Clone, Debug)]
pub enum PyImport {
    /// import json
    Full(String),

    /// Specific("typing".to_owned(), "Any".to_owned())
    /// from typing import Any
    Specific(String, String),

    /// when an import could fail, but there's a fallback
    /// clearly not meant for general python import-fu, but
    /// enough for this codegen usecase
    /// Try("typing".to_owned(), "typing_extensions".to_owned(), "TypedDict")
    /// try:
    ///     from typing import TypedDict
    /// except ImportError:
    ///     from typing_extensions import TypedDict
    Try(String, String, String),
}

impl PyImport {
    /// extract the imported String
    fn sym(&self) -> String {
        match self {
            PyImport::Full(s) => s.to_owned(),
            PyImport::Specific(_, s) => s.to_owned(),
            PyImport::Try(_, _, s) => s.to_owned(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum PyToken {
    /// Some plain source code
    Raw(String),

    /// When a symbol is coming from an import
    /// `typing.Union`
    /// would give:
    /// Import(PyImport::Full("typing"), Some("Union".to_owned()))
    /// `from typing import Union`
    /// would give
    /// Import(PyImport::Specific("typing", "Union"), None)
    Import(PyImport, Option<String>),

    Space,
    NewLine,

    /// Control indentation level
    /// Indent(1) would increase the level by one.
    /// The translation into tabs/whitespace is done when turning tokens into string
    Indent(i8),
    Block(Vec<PyToken>),

    /// can be useful when something must produce a token but there is
    /// no code to generate
    Empty,
}

impl From<String> for PyToken {
    fn from(s: String) -> Self {
        PyToken::Raw(s)
    }
}

impl From<&str> for PyToken {
    fn from(s: &str) -> Self {
        PyToken::Raw(s.to_owned())
    }
}

// impl<T: std::fmt::Debug + std::fmt::Display> From<Type<T>> for PyToken {
impl From<Type> for PyToken {
    fn from(t: Type) -> Self {
        match t {
            Type::Atomic(at) => at.into(),
            Type::Reference(r) => {
                if !r.type_parameters.is_empty() {
                    panic!(format!("Type parameters in ref type not supported {:?}", r));
                } else {
                    format!("\"{}\"", r.name).into()
                }
            }
            // Type::Anonymous(_) => todo!(),
            Type::Builtin(b) => match b {
                Builtin::List(lt) => typed_list((*lt).into()),
                Builtin::Optional(ot) => typed_optional((*ot).into()),
                Builtin::Map(kt, vt) => typed_dict((*kt).into(), (*vt).into()),
            },
            Type::TypeParameter(_tv) => todo!("From<Type> for PyToken for type parameter"),
        }
    }
}

impl Type {
    fn to_py_token(&self, gen_ctx: &GenContext) -> PyToken {
        match &self {
            Type::Atomic(at) => at.into(),
            Type::Reference(r) => {
                // let referenced_decl = &gen_ctx.top_declarations[r.name];
                let referenced_decl = r.target.as_ref();
                match referenced_decl {
                    TopDeclaration::Struct(s) => s.name.clone().into(),
                    TopDeclaration::Enum(e) => {
                        if e.is_simple_enum() {
                            e.name.clone().into()
                        } else {
                            format!("{}TypeDef", e.name).into()
                        }
                    }
                    TopDeclaration::Alias(_) => todo!("py token for alias"),
                }
            }
            Type::Builtin(b) => match b {
                Builtin::List(t) => PyToken::Block(vec![
                    type_import("List"),
                    "[".into(),
                    (*t).to_py_token(&gen_ctx),
                    "]".into(),
                ]),
                Builtin::Optional(t) => PyToken::Block(vec![
                    type_import("Optional"),
                    "[".into(),
                    (*t).to_py_token(&gen_ctx),
                    "]".into(),
                ]),
                Builtin::Map(k, v) => {
                    let dict = type_import("Mapping");
                    PyToken::Block(vec![
                        dict,
                        "[".into(),
                        k.to_py_token(&gen_ctx),
                        ", ".into(),
                        v.to_py_token(&gen_ctx),
                        "]".into(),
                    ])
                }
            },
            Type::TypeParameter(tv) => format!("{}", tv).into(),
        }
    }

    /// to test if a FieldType is exactly a given type parameter
    fn is_typevar(&self, tv: &str) -> bool {
        match &self {
            Type::TypeParameter(x) if x == tv => true,
            _ => false,
        }
    }
}

impl From<AtomicType> for PyToken {
    fn from(at: AtomicType) -> Self {
        (&at).into()
    }
}

impl From<&AtomicType> for PyToken {
    fn from(at: &AtomicType) -> Self {
        atomic_py_instance(at).0.into()
    }
}

#[derive(Clone, Debug)]
struct PyConfig {
    indent_size: usize,
}

#[derive(Debug, Default)]
struct FormatContext {
    indent_level: usize,
    should_indent: bool,
}

/// Map of imports generated from the tokens
#[derive(Default, Debug)]
struct ImportMap<'tokens> {
    imports: BTreeMap<&'tokens str, ImportEntry<'tokens>>,
    with_fallbacks: BTreeSet<(&'tokens str, &'tokens str, &'tokens str)>,
}

impl<'tokens> ImportMap<'tokens> {
    fn merge<'tok>(&mut self, other: &'tok mut ImportMap<'tokens>) {
        for (name, mut imp) in other.imports.iter_mut() {
            self.imports
                .entry(name)
                .and_modify(|e| e.merge(&mut imp))
                .or_insert_with(|| imp.clone());
        }
        self.with_fallbacks
            .extend(other.with_fallbacks.iter().cloned());
    }
}

#[derive(Default, Debug, Clone)]
struct ImportEntry<'tokens> {
    full: bool,
    specifics: BTreeSet<&'tokens str>,
}

impl<'tokens> ImportEntry<'tokens> {
    fn merge<'tok>(&mut self, other: &'tok mut ImportEntry<'tokens>) {
        self.full = self.full || other.full;

        let mut s: BTreeSet<&'tokens str> = BTreeSet::new();
        for t in &self.specifics {
            s.insert(t);
        }
        for t in &other.specifics {
            s.insert(t);
        }
        self.specifics = s.into_iter().collect();
    }
}

impl TopDeclaration {
    fn get_py_python_type(&self) -> String {
        match self {
            TopDeclaration::Struct(s) => s.name.to_owned(),
            TopDeclaration::Enum(e) => {
                if e.is_simple_enum() {
                    e.name.to_owned()
                } else {
                    format!("{}TypeDef", e.name)
                }
            }
            TopDeclaration::Alias(_) => todo!(),
        }
    }

    /// returns the type hint for the json object returned by `to_json`
    fn get_py_json_type(&self) -> String {
        match self {
            TopDeclaration::Struct(s) => format!("{}JSON", s.name),
            TopDeclaration::Enum(e) => {
                if e.is_simple_enum() {
                    "str".to_owned()
                } else {
                    format!("{}JSON", e.name)
                }
            }
            TopDeclaration::Alias(_) => todo!(),
        }
    }
}

fn indent(i: i8) -> PyToken {
    PyToken::Indent(i)
}

fn py_class(class_name: &str, super_classes: Vec<PyToken>, body: Vec<PyToken>) -> Vec<PyToken> {
    let mut tokens = Vec::new();
    tokens.push("class".into());
    tokens.push(PyToken::Space);
    tokens.push(class_name.into());
    if !super_classes.is_empty() {
        tokens.push("(".into());
        tokens.extend(intersperce(", ".into(), super_classes));
        tokens.push(")".into());
    }
    tokens.push(":".into());
    tokens.push(indent(1));
    if body.is_empty() {
        tokens.push("pass".into())
    } else {
        tokens.extend(body);
    }
    tokens.push(indent(-1));

    tokens
}

fn py_fn(
    name: &str,
    arguments: Vec<PyToken>,
    body: Vec<PyToken>,
    return_type: Option<PyToken>,
) -> Vec<PyToken> {
    let mut tokens = Vec::new();
    tokens.push("def ".into());
    tokens.push(name.into());

    if arguments.is_empty() {
        tokens.push("()".into());
    } else {
        tokens.push("(".into());
        tokens.push(indent(1));
        tokens.extend(intersperce(", ".into(), arguments));
        tokens.push(indent(-1));
        tokens.push(")".into());
    }

    if let Some(ret_type) = return_type {
        tokens.push(" -> ".into());
        tokens.push(ret_type);
    }
    tokens.push(":".into());

    tokens.push(indent(1));
    if body.is_empty() {
        tokens.push("pass".into());
    } else {
        tokens.extend(body);
    }
    tokens.push(indent(-1));

    tokens
}

fn try_import(sym: &str, primary_source: &str, fallback: &str) -> PyImport {
    PyImport::Try(
        primary_source.to_owned(),
        fallback.to_owned(),
        sym.to_owned(),
    )
}

/// An attribute or function argument declaration
/// foo: int = 3
fn typed_attr(name: &str, typ: Vec<PyToken>, default_val: Option<PyToken>) -> PyToken {
    let mut tokens = Vec::new();
    tokens.push(name.into());
    if !typ.is_empty() {
        tokens.push(": ".into());
        tokens.extend(typ);
    }
    if let Some(dv) = default_val {
        tokens.push(" = ".into());
        tokens.push(dv);
    }
    PyToken::Block(tokens)
}

/// return the same type, with all Optional removed
fn unpack_optional(typ: Type) -> Type {
    match typ {
        Type::Builtin(Builtin::Optional(t)) => *t,
        x => x,
    }
}

fn typed_union(typs: Vec<PyToken>) -> PyToken {
    let union = type_import("Union");
    let mut tokens = vec![union];
    tokens.push("[".into());
    tokens.extend(intersperce(", ".into(), typs));
    tokens.push("]".into());

    PyToken::Block(tokens)
}

fn typed_list(typ: PyToken) -> PyToken {
    let list = type_import("List");
    let mut tokens = vec![list];
    tokens.push("[".into());
    tokens.push(typ);
    tokens.push("]".into());
    PyToken::Block(tokens)
}

fn typed_dict(key_type: PyToken, val_type: PyToken) -> PyToken {
    let dict = type_import("Mapping");
    let mut tokens = vec![dict];
    tokens.push("[".into());
    tokens.push(key_type);
    tokens.push(", ".into());
    tokens.push(val_type);
    tokens.push("]".into());
    PyToken::Block(tokens)
}

fn typed_optional(typ: PyToken) -> PyToken {
    let opt = type_import("Optional");
    let mut tokens = vec![opt];
    tokens.push("[".into());
    tokens.push(typ);
    tokens.push("]".into());
    PyToken::Block(tokens)
}

fn type_import(typ: &str) -> PyToken {
    let typ = PyImport::Specific("typing".to_owned(), typ.to_owned());
    PyToken::Import(typ, None)
}

/// Optionally output a token for a TypeVar definition
fn gen_type_var(gen_ctx: &mut GenContext, tv: &str) -> PyToken {
    if gen_ctx.type_variables.contains(tv) {
        PyToken::Empty
    } else {
        gen_ctx.type_variables.insert(tv.to_string());
        PyToken::Block(vec![
            format!("{} = ", tv).into(),
            type_import("TypeVar"),
            format!("(\"{}\")", tv).into(),
            PyToken::NewLine,
        ])
    }
}

fn py_try(block: PyToken, excepts: Vec<(PyToken, PyToken)>) -> PyToken {
    let mut tokens = vec![];

    tokens.push("try:".into());
    tokens.push(indent(1));
    tokens.push(block);
    tokens.push(indent(-1));
    for (exc, exc_block) in excepts.into_iter() {
        tokens.push("except ".into());
        tokens.push(exc);
        tokens.push(":".into());
        tokens.push(indent(1));
        tokens.push(exc_block);
        tokens.push(indent(-1));
    }

    PyToken::Block(tokens)
}

fn py_imports(im: ImportMap) -> PyToken {
    let mut toks = vec![];
    for (imp_name, imp_map) in im.imports {
        let mut ts = vec![];
        if imp_map.full {
            ts.push(PyToken::Raw(format!("import {}", imp_name)));
            ts.push(PyToken::NewLine);
        }

        if !imp_map.specifics.is_empty() {
            ts.push(PyToken::Raw(format!("from {} import ", imp_name)));
            let imps = intersperce(", ", imp_map.specifics.into_iter().collect());
            ts.extend(imps.into_iter().map(|x| x.into()).collect::<Vec<_>>());
            ts.push(PyToken::NewLine);
        }

        toks.push(PyToken::Block(ts));
    }

    let mut fallbacks_blocks = Vec::new();
    for (prim_module, fallback_module, sym) in im.with_fallbacks {
        fallbacks_blocks.push(PyToken::Block(vec![
            "try:".into(),
            indent(1),
            format!("from {} import {}", prim_module, sym).into(),
            indent(-1),
            "except ImportError:".into(),
            indent(1),
            format!("from {} import {}", fallback_module, sym).into(),
            indent(-1),
        ]));
    }

    toks.push(PyToken::NewLine);
    toks.extend(fallbacks_blocks);
    PyToken::Block(toks)
}

/// returns the corresponding python instance for check with `isinstance`, along
/// with a nice name for error reporting
fn atomic_py_instance(at: &AtomicType) -> (&str, &str) {
    match at {
        AtomicType::Str => ("str", "string"),
        AtomicType::UInt => ("int", "integer"),
        AtomicType::Int => ("int", "integer"),
        AtomicType::Int8 => ("int", "integer"),
        AtomicType::Int16 => ("int", "integer"),
        AtomicType::Int32 => ("int", "integer"),
        AtomicType::Int64 => ("int", "integer"),
        AtomicType::UInt8 => ("int", "integer"),
        AtomicType::UInt16 => ("int", "integer"),
        AtomicType::UInt32 => ("int", "integer"),
        AtomicType::UInt64 => ("int", "integer"),
        AtomicType::Float => ("float", "float"),
        AtomicType::Bool => ("bool", "boolean"),
        AtomicType::Bytes => ("bytes", "bytestring"),
    }
}

// commically inneficient way to do that
fn intersperce<T: Clone>(x: T, xs: Vec<T>) -> Vec<T> {
    xs.into_iter().map(|t| vec![t]).collect::<Vec<_>>().join(&x)
}
