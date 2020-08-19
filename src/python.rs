use crate::ast;
use crate::ast::{Alias, AtomicType, Builtin, Enum, Field, Struct, TopDeclaration, Type};
use std::collections::{BTreeMap, BTreeSet};
use std::convert::From;

pub const SRC_PYTHON_PRELUDE: &'static str = include_str!("prelude.py");

#[allow(dead_code)]
pub fn gen_python(decls: &Vec<ast::TopDeclaration>) -> Vec<PyToken> {
    let code_tokens: Vec<PyToken> = decls.iter().map(|decl| PyToken::Block(gen_python_top_decl(decl))).collect();
    let imports = py_import(gather_imports(&code_tokens));
    let mut tokens = vec![];
    tokens.push(imports);
    tokens.extend(code_tokens);
    tokens
}

pub fn render_python(tokens: &Vec<PyToken>) -> String {
    let config = PyConfig { indent_size: 4 };
    let mut ctx = FormatContext::default();
    render(&config, &mut ctx, tokens)
}

fn gen_python_top_decl(decl: &ast::TopDeclaration) -> Vec<PyToken> {
    let mut tokens = Vec::new();

    let decl_tokens = match decl {
        TopDeclaration::Struct(s) => gen_py_struct(s),
        TopDeclaration::Enum(e) => gen_py_enum(e),
        TopDeclaration::Alias(a) => gen_py_alias(a),
    };
    tokens.extend(decl_tokens);
    tokens.push(PyToken::NewLine);

    tokens.push(SRC_PYTHON_PRELUDE.into());

    tokens
}

fn gen_py_struct(s: &Struct) -> Vec<PyToken> {
    let mut tokens = Vec::new();
    let td = PyToken::Import(
        PyImport::Specific("typing_extensions".to_string(), "TypedDict".to_string()),
        None,
    );

    let attributes = intersperce(
        PyToken::NewLine,
        s.fields.iter().map(|f| typed_field(f)).collect(),
    );

    let struct_td_name = format!("{}TypedDict", s.name);
    let struct_td = pyclass(&struct_td_name, vec![td], attributes.clone());
    tokens.extend(struct_td);

    tokens.push(PyToken::NewLine);

    let mut body = vec![];
    body.extend(attributes);
    body.push(PyToken::NewLine);
    body.push(PyToken::NewLine);

    let mut init_args = vec!["self".into()];
    init_args.extend(s.fields.iter().map(|f| {
        typed_attr(
            &f.name,
            vec![Type::Builtin(Builtin::Optional(Box::new(unpack_optional(f.typ.clone())))).into()],
            None,
        )
    }));

    let mut init_body = vec![];
    init_body.push("errors = {}".into());
    init_body.push(PyToken::NewLine);

    for field in &s.fields {
        init_body.push(PyToken::NewLine);

        init_body.push("try:".into());
        init_body.push(indent(1));
        let parse_fn = parse_function(&field.typ, &field.name, 0);
        init_body.push(format!("self.{} = {}", field.name, parse_fn).into());
        init_body.push(indent(-1));
        init_body.push("except ValidationError as e:".into());
        init_body.push(indent(1));
        init_body.push(format!("errors[\"{}\"] = e.messages", field.name).into());
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
    // return cls(data.get("name"), data.get("email"), data.get("age"),)

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
        vec!["cls".into(), typed_attr("data", vec![any.clone()], None)],
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
                    .map(|f| format!("\"{}\": self.{},", f.name, f.name).into())
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

fn gen_py_enum(_e: &Enum) -> Vec<PyToken> {
    todo!("gen py enum")
}

fn gen_py_alias(_a: &Alias) -> Vec<PyToken> {
    todo!("gen py alias")
}

fn gather_imports(tokens: &Vec<PyToken>) -> BTreeMap<&str, ImportMap> {
    let mut imports = BTreeMap::new();

    // include the imports coming from the "stdlib", code not generated
    // but automatically included
    imports.insert(
        "typing",
        ImportMap {
            full: false,
            specifics: vec![
                "TypeVar", "Any", "cast", "Dict", "List", "Optional", "Callable", "Union",
            ],
            aliased: vec![],
        },
    );

    imports.insert(
        "base64",
        ImportMap {
            full: true,
            specifics: vec![],
            aliased: vec![],
        },
    );

    imports.insert(
        "binascii",
        ImportMap {
            full: true,
            specifics: vec![],
            aliased: vec![],
        },
    );

    for tok in tokens {
        match tok {
            PyToken::Import(imp, _) => match imp {
                PyImport::Full(name) => {
                    let entry = imports.entry(name.as_str()).or_insert(ImportMap::default());
                    entry.full = true;
                }
                PyImport::Specific(module, name) => {
                    let entry = imports.entry(module.as_str()).or_default();
                    entry.specifics.push(name);
                }
                PyImport::Alias(name, alias) => {
                    let entry = imports.entry(name.as_str()).or_default();
                    entry.aliased.push(alias);
                }
            },
            PyToken::Block(nested_tokens) => {
                for (name, mut imp) in gather_imports(nested_tokens) {
                    imports
                        .entry(name)
                        .and_modify(|e| e.merge(&mut imp))
                        .or_insert(imp);
                }
            }
            _ => (),
        }
    }
    imports
}

fn parse_function(t: &Type, name: &str, depth: u8) -> String {
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
            Builtin::Map(_, _) => todo!(),
        },
        Type::Reference(_) => todo!(),
        Type::Anonymous(_) => todo!(),
    }
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

fn render(conf: &PyConfig, mut ctx: &mut FormatContext, tokens: &Vec<PyToken>) -> String {
    let mut buf: Vec<String> = Vec::new();

    for tok in tokens.into_iter() {
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
    /// from typing import Any
    Specific(String, String),
    /// import typing as t
    #[allow(dead_code)]
    Alias(String, String),
    // Try(Vec<String>),
}

impl PyImport {
    /// extract the imported String
    fn sym(&self) -> String {
        match self {
            PyImport::Full(s) => s.to_string(),
            PyImport::Specific(_, s) => s.to_string(),
            PyImport::Alias(_, s) => s.to_string(),
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
            Type::Anonymous(_) => todo!(),
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
    full: bool,
    specifics: Vec<&'tokens str>,
    aliased: Vec<&'tokens str>,
}

impl<'tokens> ImportMap<'tokens> {
    fn merge<'tok>(&mut self, other: &'tok mut ImportMap<'tokens>) {
        self.full = self.full || other.full;

        let mut s: BTreeSet<&'tokens str> = BTreeSet::new();
        for t in &self.specifics {
            s.insert(t);
        }
        for t in &other.specifics {
            s.insert(t);
        }
        self.specifics = s.into_iter().collect();

        s = BTreeSet::new();
        for t in &self.aliased {
            s.insert(t);
        }
        for t in &other.aliased {
            s.insert(t);
        }
        self.aliased = s.into_iter().collect();
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
fn typed_field(f: &Field) -> PyToken {
    typed_attr(&f.name, vec![f.typ.clone().into()], None)
}

/// return the same type, with all Optional
fn unpack_optional(typ: Type) -> Type {
    match typ {
        Type::Builtin(Builtin::Optional(t)) => *t,
        x => x,
    }
}

// fn typed_union(typs: Vec<PyToken>) -> PyToken {
//     let union = type_import("Union");
//     let mut tokens = vec![union];
//     tokens.push("[".into());
//     tokens.extend(intersperce(", ".into(), typs));
//     tokens.push("]".into());
//
//     PyToken::Block(tokens)
// }

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

fn py_import(imports: BTreeMap<&str, ImportMap>) -> PyToken {
    let mut toks = vec![];
    for (imp_name, imp_map) in imports {
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

        for alias in imp_map.aliased {
            ts.push(PyToken::Raw(format!("import {} as {}", imp_name, alias)));
            ts.push(PyToken::NewLine);
        }

        toks.push(PyToken::Block(ts));
    }
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
