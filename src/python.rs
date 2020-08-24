use crate::ast;
use crate::ast::{
    Alias, AtomicType, Builtin, Enum, Field, Struct, TopDeclaration, Type, VariantValue, ResolvedReference
};
use inflections::Inflect;
use std::collections::{BTreeMap, BTreeSet};
use std::convert::From;

pub const SRC_PYTHON_PRELUDE: &'static str = include_str!("prelude.py");

#[allow(dead_code)]
pub fn gen_python(decls: &[ast::TopDeclaration<ResolvedReference>]) -> Vec<PyToken> {
    let mut gen_ctx = GenCtx::new();
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

    // tokens.push(PyToken::NewLine);
    // tokens.push(SRC_PYTHON_PRELUDE.into());

    tokens
}

pub fn render_python(tokens: &[PyToken]) -> String {
    let config = PyConfig { indent_size: 4 };
    let mut ctx = FormatContext::default();
    render(&config, &mut ctx, tokens)
}

/// A struct used to gather various requirements while
/// generating the code. Some construct may require addition
/// to the prelude (like parsing tuples)
struct GenCtx {
    tuple_arities: BTreeSet<u8>,
}

impl GenCtx {
    fn new() -> Self {
        GenCtx {
            tuple_arities: BTreeSet::new(),
        }
    }
}

fn gen_python_top_decl(mut gen_ctx: &mut GenCtx, decl: &ast::TopDeclaration<ResolvedReference>) -> Vec<PyToken> {
    let mut tokens = Vec::new();

    let decl_tokens = match decl {
        TopDeclaration::Struct(s) => gen_py_struct(s),
        TopDeclaration::Enum(e) => gen_py_enum(&mut gen_ctx, e),
        TopDeclaration::Alias(a) => gen_py_alias(a),
    };
    tokens.extend(decl_tokens);
    tokens.push(PyToken::NewLine);

    tokens
}

fn gen_py_struct(s: &Struct<ResolvedReference>) -> Vec<PyToken> {
    let mut tokens = Vec::new();
    let td = PyToken::Import(try_import("TypedDict", "typing", "typing_extensions"), None);

    let dict_attributes = intersperce(
        PyToken::NewLine,
        s.fields.iter().map(|f| typed_field(f)).collect(),
    );

    let struct_td_name = format!("{}TypedDict", s.name);
    let struct_td = pyclass(&struct_td_name, vec![td], dict_attributes.clone());
    tokens.extend(struct_td);

    tokens.push(PyToken::NewLine);

    let mut body = vec![];
    body.extend(intersperce(
        PyToken::NewLine,
        s.fields
            .iter()
            .map(|f| typed_attr(&f.name.to_snake_case(), vec![f.typ.clone().into()], None))
            .collect(),
    ));
    body.push(PyToken::NewLine);
    body.push(PyToken::NewLine);

    let mut init_args = vec!["self".into()];
    init_args.extend(s.fields.iter().map(|f| {
        typed_attr(
            &f.name.to_snake_case(),
            vec![Type::Builtin(Builtin::Optional(Box::new(unpack_optional(f.typ.clone())))).into()],
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
        let parse_fn = parse_function(&field.typ, &name, 0);
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

    let any = PyToken::Import(
        PyImport::Specific("typing".to_string(), "Any".to_string()),
        None,
    );
    let abc_mapping = PyToken::Import(
        PyImport::Full("collections".to_string()),
        Some("abc.Mapping".to_string()),
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
                    .map(|f| format!("\"{}\": self.{},", f.name, f.name.to_snake_case()).into())
                    .collect(),
            )),
            indent(-1),
            "}".into(),
        ],
        Some(struct_td_name.into()),
    ));

    tokens.push(PyToken::NewLine);
    let dataclass = PyToken::Import(
        PyImport::Specific("dataclasses".to_string(), "dataclass".to_string()),
        None,
    );
    tokens.push("@".into());
    tokens.push(dataclass);
    tokens.push(PyToken::NewLine);

    let struct_class = pyclass(&s.name, vec![], body);
    tokens.extend(struct_class);
    // let struct_td = pyclass(format!("{}TypedDict", s.name).as_ref(), vec![td], body);
    // tokens.extend(struct_td);

    tokens
}

fn gen_py_enum(mut gen_ctx: &mut GenCtx, e: &Enum<ResolvedReference>) -> Vec<PyToken> {
    if !e.type_parameters.is_empty() {
        panic!("Enum with type parameters not supported yet");
    }
    // println!("{:#?}", e);
    let mut tokens = Vec::new();
    for variant in &e.variants {
        tokens.push(gen_enum_variant(&mut gen_ctx, &variant));
        tokens.push(PyToken::NewLine);
    }

    tokens.push(format!("{}TypeDef = ", e.name).into());
    tokens.push(typed_union(
        e.variants.iter().map(|v| v.name.clone().into()).collect(),
    ));
    tokens.push(PyToken::NewLine);

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
                "TypeVar", "Any", "cast", "Dict", "List", "Optional", "Callable", "Union", "Tuple",
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

fn parse_function(t: &Type<ResolvedReference>, name: &str, depth: u8) -> String {
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
                let nested = parse_function(opt_type, name, depth + 1);
                if depth == 0 {
                    format!("parse_optional({}, {})", nested, name)
                } else {
                    format!("lambda x{}: parse_optional({}, x{})", depth, nested, depth)
                }
            }
            Builtin::List(inner) => {
                let nested = parse_function(inner, name, depth + 1);
                if depth == 0 {
                    format!("parse_list({}, {})", nested, name)
                } else {
                    format!("lambda x{}: parse_list({}, x{})", depth, nested, depth)
                }
            }
            Builtin::Map(key_type, val_type) => {
                let parse_key_fn = parse_function(key_type, "key_name?", depth + 1);
                let parse_val_fn = parse_function(val_type, "val_name?", depth + 1);
                format!("parse_map({}, {}, {})", parse_key_fn, parse_val_fn, name)
            }
        },
        Type::Reference(_) => todo!(),
        // Type::Anonymous(_) => todo!(),
    }
}

fn gen_enum_variant(mut gen_ctx: &mut GenCtx, variant: &ast::EnumVariant<ResolvedReference>) -> PyToken {
    let mut tokens = Vec::new();
    let dataclass = PyToken::Import(
        PyImport::Specific("dataclasses".to_string(), "dataclass".to_string()),
        None,
    );
    tokens.push("@".into());
    tokens.push(dataclass);
    tokens.push(PyToken::NewLine);
    tokens.extend(pyclass(&variant.name, vec![], vec![]));
    PyToken::Block(tokens)
}

// /// We may need to summon a new name from the given constructor
// fn gen_variant_name(variant: &ast::EnumVariant) -> String {
//     match &variant.value {
//         VariantValue::StructCtor(_) => format!("{}Struct", variant.name),
//         _ => variant.name.to_string(),
//     }
// }

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
                buf.push(" ".to_string());
                ctx.should_indent = false;
            }
            PyToken::NewLine => {
                buf.push("\n".to_string());
                ctx.should_indent = true;
            }
            PyToken::Indent(i) => {
                let il = ctx.indent_level as i8;
                buf.push("\n".to_string());
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

#[derive(Clone, Debug)]
pub enum PyImport {
    /// import json
    Full(String),

    /// Specific("typing".to_string(), "Any".to_string())
    /// from typing import Any
    Specific(String, String),

    /// when an import could fail, but there's a fallback
    /// clearly not meant for general python import-fu, but
    /// enough for this codegen usecase
    /// Try("typing".to_string(), "typing_extensions".to_string(), "TypedDict")
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
            PyImport::Full(s) => s.to_string(),
            PyImport::Specific(_, s) => s.to_string(),
            PyImport::Try(_, _, s) => s.to_string(),
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
    /// Import(PyImport::Full("typing"), Some("Union".to_string()))
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
        PyToken::Raw(s.to_string())
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

impl From<AtomicType> for PyToken {
    fn from(at: AtomicType) -> Self {
        atomic_py_instance(&at).0.into()
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
        primary_source.to_string(),
        fallback.to_string(),
        sym.to_string(),
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
fn typed_field(f: &Field<ResolvedReference>) -> PyToken {
    typed_attr(&f.name, vec![f.typ.clone().into()], None)
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
    let typ = PyImport::Specific("typing".to_string(), typ.to_string());
    PyToken::Import(typ, None)
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
