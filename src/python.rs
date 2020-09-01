use crate::ast;
use crate::ast::{
    Alias, AtomicType, Builtin, Enum, Field, ResolvedReference, Struct, TopDeclaration, Type,
    VariantValue,
};
use inflections::Inflect;
use std::collections::{BTreeMap, BTreeSet};
use std::convert::From;

pub const SRC_PYTHON_PRELUDE: &'static str = include_str!("prelude.py");

#[allow(dead_code)]
pub fn gen_python(decls: &[ast::TopDeclaration<ResolvedReference>]) -> Vec<PyToken> {
    let mut gen_ctx = GenCtx::new(decls);
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
    if let Some(n) = gen_ctx.tuple_arities.iter().max() {
        tokens.push(gen_type_vars(*n));
        tokens.push(PyToken::NewLine);
    }
    for n in gen_ctx.tuple_arities {
        tokens.push(gen_tuple_parser(n));
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
struct GenCtx<'decls> {
    /// The code to parse tuple is very tedious to write manually and thus
    /// instead of putting that in the prelude, track which tuple arities
    /// are required. The parsing code will be generated on the fly
    tuple_arities: BTreeSet<u8>,
    top_declarations: &'decls [ast::TopDeclaration<ResolvedReference>],
}

impl<'decls> GenCtx<'decls> {
    fn new(decls: &'decls [ast::TopDeclaration<ResolvedReference>]) -> Self {
        GenCtx {
            tuple_arities: BTreeSet::new(),
            top_declarations: decls,
        }
    }
}

fn gen_python_top_decl(
    mut gen_ctx: &mut GenCtx,
    decl: &ast::TopDeclaration<ResolvedReference>,
) -> Vec<PyToken> {
    let mut tokens = Vec::new();

    let decl_tokens = match decl {
        TopDeclaration::Struct(s) => gen_py_struct(&gen_ctx, s),
        TopDeclaration::Enum(e) => gen_py_enum(&mut gen_ctx, e),
        TopDeclaration::Alias(a) => gen_py_alias(a),
    };
    tokens.extend(decl_tokens);
    tokens.push(PyToken::NewLine);

    tokens
}

fn gen_py_struct(gen_ctx: &GenCtx, s: &Struct<ResolvedReference>) -> Vec<PyToken> {
    let mut tokens = Vec::new();
    let td = PyToken::Import(try_import("TypedDict", "typing", "typing_extensions"), None);

    let struct_td_name = format!("{}TypedDict", s.name);
    let struct_td_attrs = intersperce(
        PyToken::NewLine,
        s.fields
            .iter()
            .map(|f| typed_attr(&f.name, vec![f.typ.get_json_type(&gen_ctx).into()], None))
            .collect(),
    );
    let struct_td = pyclass(&struct_td_name, vec![td], struct_td_attrs);
    tokens.extend(struct_td);

    tokens.push(PyToken::NewLine);

    let mut body = vec![];
    body.extend(intersperce(
        PyToken::NewLine,
        s.fields
            .iter()
            .map(|f| {
                typed_attr(
                    &f.name.to_snake_case(),
                    vec![f.typ.to_py_token(&gen_ctx)],
                    None,
                )
            })
            .collect(),
    ));
    body.push(PyToken::NewLine);
    body.push(PyToken::NewLine);

    let mut init_args = vec!["self".into()];
    init_args.extend(s.fields.iter().map(|f| {
        typed_attr(
            &f.name.to_snake_case(),
            vec![
                Type::Builtin(Builtin::Optional(Box::new(unpack_optional(f.typ.clone()))))
                    .to_py_token(&gen_ctx),
            ],
            None,
        )
    }));

    let mut init_body = vec![];
    init_body.push("errors = {}".into());
    init_body.push(PyToken::NewLine);

    for field in &s.fields {
        let name = &field.name.to_snake_case();
        init_body.push(PyToken::NewLine);

        init_body.push("try:".into());
        init_body.push(indent(1));
        let parse_fn = get_parse_function(&gen_ctx, &field.typ, &name, 0);
        init_body.push(format!("self.{} = {}", name, parse_fn).into());
        init_body.push(indent(-1));
        init_body.push("except ValidationError as e:".into());
        init_body.push(indent(1));
        init_body.push(format!("errors[\"{}\"] = e.messages", name).into());
        init_body.push(indent(-1));
    }

    init_body.push(PyToken::NewLine);
    init_body.push(
        If::new(
            "errors".into(),
            "raise ValidationError(message=errors)".into(),
        )
        .into(),
    );

    let init = py_fn("__init__", init_args, init_body, Some("None".into()));
    body.extend(init);
    body.push(PyToken::NewLine);

    let any = type_import("Any");
    let abc_mapping = PyToken::Import(
        PyImport::Full("collections".to_owned()),
        Some("abc.Mapping".to_owned()),
    );
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

    from_json_body.push("try:".into());
    from_json_body.push(indent(1));

    let args: Vec<PyToken> = s
        .fields
        .iter()
        .map(|f| format!("data.get(\"{}\")", f.name).into())
        .collect();
    let args = intersperce(", ".into(), args);

    from_json_body.extend(vec!["return cls(".into(), PyToken::Block(args), ")".into()]);

    from_json_body.extend(vec![
        indent(-1),
        "except ValidationError as e:".into(),
        indent(1),
        "e.data = data".into(),
        PyToken::NewLine,
        "raise e".into(),
        indent(-1),
    ]);

    let from_json = py_fn(
        "from_json",
        vec!["cls".into(), typed_attr("data", vec![any], None)],
        from_json_body,
        Some(format!("\"{}\"", s.name).into()),
    );
    body.push("@classmethod".into());
    body.push(PyToken::NewLine);
    body.extend(from_json);

    body.extend(py_fn(
        "to_json",
        vec!["self".into()],
        vec![
            "return {".into(),
            indent(1),
            PyToken::Block(intersperce(
                PyToken::NewLine,
                s.fields
                    .iter()
                    .map(|f| {
                        let name = f.name.to_snake_case();
                        format!(
                            r#""{}": self.{},"#,
                            f.name,
                            get_dump_function(&gen_ctx, &f.typ, &name.to_snake_case(), 0)
                                .unwrap_or(name)
                        )
                        .into()
                    })
                    .collect(),
            )),
            indent(-1),
            "}".into(),
        ],
        Some(struct_td_name.into()),
    ));

    tokens.push(PyToken::NewLine);
    let dataclass = PyToken::Import(
        PyImport::Specific("dataclasses".to_owned(), "dataclass".to_owned()),
        None,
    );
    tokens.push("@".into());
    tokens.push(dataclass);
    tokens.push(PyToken::NewLine);

    let struct_class = pyclass(&s.name, vec![], body);
    tokens.extend(struct_class);

    tokens
}

fn gen_py_enum(mut gen_ctx: &mut GenCtx, e: &Enum<ResolvedReference>) -> Vec<PyToken> {
    if !e.type_parameters.is_empty() {
        panic!("Enum with type parameters not supported yet");
    }
    let mut tokens = Vec::new();

    if e.is_simple_enum() {
        tokens.push(gen_simple_enum(&e));
    } else {
        tokens.push(gen_py_sum_type(&mut gen_ctx, &e));
    }

    tokens
}

fn gen_py_alias(_a: &Alias<ResolvedReference>) -> Vec<PyToken> {
    todo!("gen py alias")
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
                "TypeVar", "Any", "cast", "Dict", "List", "Optional", "Callable", "Union",
            ],
        },
    );

    import_map.imports.insert(
        "base64",
        ImportEntry {
            full: true,
            specifics: vec![],
        },
    );

    import_map.imports.insert(
        "binascii",
        ImportEntry {
            full: true,
            specifics: vec![],
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
                    entry.specifics.push(name);
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

/// Given a `Type`, return a string representing the function to parse json into
/// that type
fn get_parse_function(
    gen_ctx: &GenCtx,
    t: &Type<ResolvedReference>,
    name: &str,
    depth: u8,
) -> String {
    match t {
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
                let nested = get_parse_function(&gen_ctx, opt_type, name, depth + 1);
                if depth == 0 {
                    format!("parse_optional({}, {})", nested, name)
                } else {
                    format!("lambda x{}: parse_optional({}, x{})", depth, nested, depth)
                }
            }
            Builtin::List(inner) => {
                let nested = get_parse_function(&gen_ctx, inner, name, depth + 1);
                if depth == 0 {
                    format!("parse_list({}, {})", nested, name)
                } else {
                    format!("lambda x{}: parse_list({}, x{})", depth, nested, depth)
                }
            }
            Builtin::Map(key_type, val_type) => {
                let parse_key_fn = get_parse_function(&gen_ctx, key_type, "key_name?", depth + 1);
                let parse_val_fn = get_parse_function(&gen_ctx, val_type, "val_name?", depth + 1);
                format!("parse_map({}, {}, {})", parse_key_fn, parse_val_fn, name)
            }
        },
        Type::Reference(r) => {
            let referenced_decl = &gen_ctx.top_declarations[r.name];
            if depth == 0 {
                format!("{}.from_json({})", referenced_decl.get_name(), name)
            } else {
                format!("{}.from_json", referenced_decl.get_name(),)
            }
        }
    }
}

/// Dual of get_parse_function, returns a String representing the function to convert
/// the given type into json representation
fn get_dump_function(
    _gen_ctx: &GenCtx,
    t: &Type<ResolvedReference>,
    name: &str,
    depth: u8,
) -> Option<String> {
    match t {
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
                match get_dump_function(&_gen_ctx, t, name, depth + 1) {
                    // the inner type doesn't need any conversion, so the list
                    // itself also doesn't need conversion
                    None => None,
                    Some(d) => {
                        if depth == 0 {
                            Some(format!("dump_list({}, {})", d, name))
                        } else {
                            Some(format!("lambda x: dump_list({}, x)", d))
                        }
                    }
                }
            }
            Builtin::Optional(t) => {
                match get_dump_function(&_gen_ctx, t, name, depth + 1) {
                    // the inner type doesn't need any conversion, so the type
                    // itself also doesn't need conversion
                    None => None,
                    Some(d) => {
                        if depth == 0 {
                            Some(format!("dump_optional({}, {})", d, name))
                        } else {
                            Some(format!("lambda x: dump_optional({}, x)", d))
                        }
                    }
                }
            }
            Builtin::Map(_, _) => todo!(),
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
    }
}

fn gen_simple_enum(e: &Enum<ResolvedReference>) -> PyToken {
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
        Some("str".into()),
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

    PyToken::Block(pyclass(&e.name.to_pascal_case(), vec![enum_class], body))
}

fn gen_py_sum_type(mut gen_ctx: &mut GenCtx, e: &Enum<ResolvedReference>) -> PyToken {
    let mut tokens = vec![];
    for variant in &e.variants {
        tokens.push(gen_enum_variant(&mut gen_ctx, &variant));
        tokens.push(PyToken::NewLine);
    }

    tokens.push(format!("{}TypeDef = ", e.name).into());
    tokens.push(typed_union(
        e.variants.iter().map(|v| v.name.clone().into()).collect(),
    ));
    tokens.push(PyToken::NewLine);
    tokens.push(PyToken::NewLine);

    let mut from_json_body = vec![];
    from_json_body.push(
        If::new(
            "not isinstance(data, dict)".into(),
            r#"raise ValidationError(message="Not a dictionnary")"#.into(),
        )
        .into(),
    );
    from_json_body.push(r#"tag = data.get("tag")"#.into());
    from_json_body.push(PyToken::NewLine);
    from_json_body.push(r#"contents = data.get("contents")"#.into());
    from_json_body.push(PyToken::NewLine);

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
                format!("return {}({})", variant.name.to_pascal_case(), arg).into(),
            )
            .into(),
        );
    }

    from_json_body
        .push(format!(r#"raise ValidationError(message="unknown tag {{}}".format(tag))"#).into());

    tokens.extend(pyclass(
        &e.name.to_pascal_case(),
        vec![],
        vec![
            "@classmethod".into(),
            PyToken::NewLine,
            PyToken::Block(py_fn(
                "from_json",
                vec!["cls".into(), typed_attr("data", vec!["Any".into()], None)],
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
                Some(format!("{}TypeDef", e.name).into()),
            )),
        ],
    ));

    PyToken::Block(tokens)
}

fn gen_enum_variant(
    gen_ctx: &mut GenCtx,
    variant: &ast::EnumVariant<ResolvedReference>,
) -> PyToken {
    let mut tokens = Vec::new();
    let dataclass = PyToken::Import(
        PyImport::Specific("dataclasses".to_owned(), "dataclass".to_owned()),
        None,
    );

    tokens.push(PyToken::NewLine);

    let mut td_class_body = vec![typed_attr(
        "tag",
        vec![
            type_import("Literal"),
            format!(r#"["{}"]"#, variant.name).into(),
        ],
        None,
    )];

    let td_name = format!("{}TypedDict", variant.name.to_pascal_case());

    match &variant.value {
        VariantValue::OnlyCtor => (),
        VariantValue::PositionalCtor(refs) => {
            td_class_body.push(PyToken::NewLine);
            if refs.len() == 1 {
                td_class_body.push(typed_attr(
                    "contents",
                    vec![refs[0].get_json_type(&gen_ctx).into()],
                    // vec![refs[0].to_py_token(&gen_ctx)],
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
                td_class_body.push(typed_attr("contents", typs, None));
            }
        }
        VariantValue::StructCtor(_) => todo!("anon struct in sum type"),
    }

    tokens.extend(pyclass(
        &td_name,
        vec![type_import("TypedDict")],
        td_class_body,
    ));

    tokens.push(PyToken::NewLine);
    tokens.push(PyToken::NewLine);

    let mut class_body = vec![];

    match &variant.value {
        VariantValue::OnlyCtor => {
            let tag_val = variant.alias.as_ref().unwrap_or(&variant.name);
            class_body.extend(py_fn(
                "to_json",
                vec!["self".into()],
                vec![format!(r#"return {{"tag": "{}"}}"#, tag_val).into()],
                Some("Any".into()),
            ));
        }

        VariantValue::PositionalCtor(refs) => {
            if refs.len() == 1 {
                class_body.push(typed_attr(
                    "inner".into(),
                    vec![refs[0].to_py_token(&gen_ctx)],
                    None,
                ));

                class_body.push(PyToken::NewLine);
                class_body.push(PyToken::NewLine);

                class_body.extend(py_fn(
                    "__init__".into(),
                    vec!["self".into(), typed_attr("inner", vec!["Any".into()], None)],
                    vec![format!(
                        "self.inner = {}",
                        get_parse_function(&gen_ctx, &refs[0], "inner", 0)
                    )
                    .into()],
                    Some("None".into()),
                ));

                class_body.push(PyToken::NewLine);

                class_body.extend(py_fn(
                    "to_json",
                    vec!["self".into()],
                    vec![format!(
                        r#"return {{"tag": "{}", "contents": {}}}"#,
                        variant.name.to_pascal_case(),
                        get_dump_function(&gen_ctx, &refs[0], "self.inner", 0)
                            .unwrap_or("self.inner".to_owned())
                    )
                    .into()],
                    Some(td_name.into()),
                ));
            } else {
                gen_ctx.tuple_arities.insert(refs.len() as u8);
                let tuple = PyToken::Import(
                    PyImport::Specific("typing".to_owned(), "Tuple".to_owned()),
                    None,
                );
                class_body.push("inner: ".into());
                class_body.push(tuple);
                class_body.push("[".into());
                class_body.push(PyToken::Block(intersperce(
                    ", ".into(),
                    refs.iter().map(|r| r.to_py_token(&gen_ctx)).collect(),
                )));
                class_body.push("]".into());

                class_body.push(PyToken::NewLine);
                class_body.push(PyToken::NewLine);

                let mut parse_fns: Vec<_> = refs
                    .iter()
                    .map(|t| get_parse_function(&gen_ctx, t, "", 1))
                    .collect();
                parse_fns.push("inner".to_owned());

                class_body.extend(py_fn(
                    "__init__".into(),
                    vec!["self".into(), typed_attr("inner", vec!["Any".into()], None)],
                    vec![format!(
                        r#"self.inner = parse_tuple_{}({})"#,
                        refs.len(),
                        parse_fns.join(", ")
                    )
                    .into()],
                    Some("None".into()),
                ));

                class_body.push(PyToken::NewLine);

                let to_json_contents = intersperce(
                    ", ".into(),
                    refs.iter()
                        .enumerate()
                        .map(|(idx, r)| {
                            let val_name = format!("self.inner[{}]", idx);
                            get_dump_function(&gen_ctx, r, &val_name, 0)
                                .unwrap_or(val_name)
                                .into()
                        })
                        .collect(),
                );

                class_body.extend(py_fn(
                    "to_json",
                    vec!["self".into()],
                    vec![
                        format!(
                            r#"return {{"tag": "{}", "contents": ("#,
                            variant.name.to_pascal_case(),
                        )
                        .into(),
                        PyToken::Block(to_json_contents),
                        ")}".into(),
                    ],
                    Some(td_name.into()),
                ));
            }
        }
        VariantValue::StructCtor(_) => todo!("StructCtor not implemented"),
    }

    tokens.push("@".into());
    tokens.push(dataclass);
    tokens.push(PyToken::NewLine);
    tokens.extend(pyclass(&variant.name.to_pascal_case(), vec![], class_body));

    PyToken::Block(tokens)
}

// /// We may need to summon a new name from the given constructor
// fn gen_variant_name(variant: &ast::EnumVariant) -> String {
//     match &variant.value {
//         VariantValue::StructCtor(_) => format!("{}Struct", variant.name),
//         _ => variant.name.to_owned(),
//     }
// }

fn gen_type_vars(n: u8) -> PyToken {
    let mut tokens = vec![];
    let tv = PyToken::Import(
        PyImport::Specific("typing".to_owned(), "TypeVar".to_owned()),
        None,
    );
    for i in 1..=n {
        tokens.push(format!("T{} = ", i).into());
        tokens.push(tv.clone());
        tokens.push(format!(r#"("T{}")"#, i).into());
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
        }
    }
    buf.join("")
}

impl Type<ResolvedReference> {
    /// Returns a python type hint for the json representation of
    /// this type after serialization.
    fn get_json_type(&self, gen_ctx: &GenCtx) -> String {
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
                Builtin::Map(_k, _v) => todo!(),
            },
            Type::Reference(r) => {
                let referenced_decl = &gen_ctx.top_declarations[r.name];
                format!("{}TypedDict", referenced_decl.get_name())
            }
        }
    }
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
impl From<Type<String>> for PyToken {
    fn from(t: Type<String>) -> Self {
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
        }
    }
}

// // TODO: remove that. Only done to silence the compiler while I'm fixing other stuff
// impl From<Type<usize>> for PyToken {
//     fn from(t: Type<usize>) -> Self {
//         todo!("can't implement From, need to have a function which resolve reference")
//     }
// }

impl Type<usize> {
    fn to_py_token(&self, gen_ctx: &GenCtx) -> PyToken {
        match &self {
            Type::Atomic(at) => at.into(),
            Type::Reference(r) => {
                let referenced_decl = &gen_ctx.top_declarations[r.name];
                referenced_decl.get_name().into()
            }
            Type::Builtin(b) => match b {
                Builtin::List(t) => {
                    PyToken::Block(vec!["List[".into(), (*t).to_py_token(&gen_ctx), "]".into()])
                }
                Builtin::Optional(t) => PyToken::Block(vec![
                    "Optional[".into(),
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
    specifics: Vec<&'tokens str>,
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

fn indent(i: i8) -> PyToken {
    PyToken::Indent(i)
}

fn pyclass(class_name: &str, super_classes: Vec<PyToken>, body: Vec<PyToken>) -> Vec<PyToken> {
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

/// Turn an ast::Field into a field declaration like:
/// foo: int
fn typed_field(gen_ctx: &GenCtx, f: &Field<ResolvedReference>) -> PyToken {
    typed_attr(&f.name, vec![f.typ.to_py_token(&gen_ctx)], None)
}

/// return the same type, with all Optional
fn unpack_optional(typ: Type<ResolvedReference>) -> Type<ResolvedReference> {
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
            let imps = intersperce(", ", imp_map.specifics);
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
