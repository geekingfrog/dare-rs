pub mod ast;
pub mod dare;

#[macro_use]
extern crate anyhow;
use anyhow::Result;

use std::collections::{BTreeMap, BTreeSet};
use std::convert::From;
use std::fs::File;
use std::io::{prelude::*, BufReader};

use ast::{Alias, AtomicType, Builtin, Enum, Field, Struct, TopDeclaration, Type};

pub const SRC_PYTHON_PRELUDE: &'static str = include_str!("prelude.py");

fn main() {
    let f = File::open("coucou.dare").unwrap();
    let result = parse(&f).unwrap();

    let toks = gen_python(&result);
    let imports = gather_imports(&toks);
    let conf = PyConfig { indent_size: 4 };
    let mut ctx = FormatContext::default();
    println!("{}", render(&conf, &mut ctx, vec![py_import(imports)]));
    println!("{}", render(&conf, &mut ctx, toks));
}

#[allow(dead_code)]
fn parse(f: &File) -> Result<ast::TopDeclaration> {
    let mut buf_reader = BufReader::new(f);
    let mut content = String::new();
    buf_reader.read_to_string(&mut content)?;
    dare::TopDeclarationParser::new()
        .parse(&content)
        .map_err(|e| anyhow!("Parse error {}", e))
}

fn gen_python(decl: &ast::TopDeclaration) -> Vec<PyToken> {
    let mut tokens = Vec::new();
    // tokens.extend(gen_python_preamble());

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

// fn gen_python_preamble() -> Vec<PyToken> {
//     let mut tokens = Vec::new();
//
//     let any = PyToken::Import(
//         PyImport::Specific("typing".to_string(), "Any".to_string()),
//         None,
//     );
//     let mut args = vec![tok_str("self")];
//     args.push(typed_attr(
//         "message",
//         vec![typed_union(vec![
//             tok_str("str"),
//             typed_list(any.clone()),
//             typed_dict(tok_str("str"), any.clone()),
//         ])],
//         None,
//     ));
//
//     args.push(typed_attr("data", vec![typed_optional(any)], None));
//
//     let mut body = vec![];
//     body.push(tok_str(
//         "self.messages = [message] if isinstance(message, (str, bytes)) else message",
//     ));
//     body.push(PyToken::NewLine);
//     body.push(tok_str("self.data = data"));
//     body.push(PyToken::NewLine);
//     body.push(tok_str("super().__init__(message)"));
//     body.push(PyToken::NewLine);
//
//     let init = py_fn("__init__", args, body, None);
//     tokens.extend(pyclass("ValidationError", vec![tok_str("Exception")], init));
//
//     tokens
// }

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
    init_body.push(tok_str("errors = {}"));
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

    let init = py_fn("__init__", init_args, init_body, Some(tok_str("None")));
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
    tokens.push(tok_str("@"));
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
    // fn py_elif(self, condition: PyToken, body: PyToken) -> Elif {
    //     Elif {
    //         prev: vec![self.into()],
    //         condition,
    //         body,
    //     }
    // }
    //
    // fn py_else(self, body: PyToken) -> Else {
    //     Else {
    //         prev: self.into(),
    //         body,
    //     }
    // }

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

// struct Elif {
//     prev: Vec<PyToken>,
//     condition: PyToken,
//     body: PyToken,
// }
//
// impl From<Elif> for PyToken {
//     fn from(x: Elif) -> Self {
//         PyToken::Block(vec![
//             PyToken::Block(x.prev),
//             "elif ".into(),
//             x.condition,
//             ":".into(),
//             indent(1),
//             x.body,
//             indent(-1),
//         ])
//     }
// }
//
// impl Elif {
//     fn py_elif(self, condition: PyToken, body: PyToken) -> Elif {
//         Elif {
//             prev: self.prev,
//             condition,
//             body,
//         }
//     }
//
//     fn py_else(self, body: PyToken) -> Else {
//         Else {
//             prev: self.into(),
//             body,
//         }
//     }
// }
//
// struct Else {
//     prev: PyToken,
//     body: PyToken,
// }
//
// impl From<Else> for PyToken {
//     fn from(x: Else) -> Self {
//         PyToken::Block(vec![x.prev, "else:".into(), indent(1), x.body, indent(-1)])
//     }
// }

fn render(conf: &PyConfig, mut ctx: &mut FormatContext, tokens: Vec<PyToken>) -> String {
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
                buf.push(render(&conf, &mut ctx, toks));
            }
        }
    }
    buf.join("")
}

#[derive(Clone, Debug)]
enum PyImport {
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
enum PyToken {
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

fn tok_str(s: &str) -> PyToken {
    PyToken::Raw(s.to_string())
}

fn indent(i: i8) -> PyToken {
    PyToken::Indent(i)
}

fn pyclass(class_name: &str, super_classes: Vec<PyToken>, body: Vec<PyToken>) -> Vec<PyToken> {
    let mut tokens = Vec::new();
    tokens.push(tok_str("class"));
    tokens.push(PyToken::Space);
    tokens.push(tok_str(class_name));
    if !super_classes.is_empty() {
        tokens.push(tok_str("("));
        tokens.extend(intersperce(tok_str(", "), super_classes));
        tokens.push(tok_str(")"));
    }
    tokens.push(tok_str(":"));
    tokens.push(indent(1));
    if body.is_empty() {
        tokens.push(tok_str("pass"))
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
    tokens.push(tok_str(name));
    if !typ.is_empty() {
        tokens.push(tok_str(": "));
        tokens.extend(typ);
    }
    if let Some(dv) = default_val {
        tokens.push(tok_str(" = "));
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

fn typed_union(typs: Vec<PyToken>) -> PyToken {
    let union = type_import("Union");
    let mut tokens = vec![union];
    tokens.push(tok_str("["));
    tokens.extend(intersperce(tok_str(", "), typs));
    tokens.push(tok_str("]"));

    PyToken::Block(tokens)
}

fn typed_list(typ: PyToken) -> PyToken {
    let list = type_import("List");
    let mut tokens = vec![list];
    tokens.push(tok_str("["));
    tokens.push(typ);
    tokens.push(tok_str("]"));
    PyToken::Block(tokens)
}

fn typed_dict(key_type: PyToken, val_type: PyToken) -> PyToken {
    let dict = type_import("Mapping");
    let mut tokens = vec![dict];
    tokens.push(tok_str("["));
    tokens.push(key_type);
    tokens.push(tok_str(", "));
    tokens.push(val_type);
    tokens.push(tok_str("]"));
    PyToken::Block(tokens)
}

fn typed_optional(typ: PyToken) -> PyToken {
    let opt = type_import("Optional");
    let mut tokens = vec![opt];
    tokens.push(tok_str("["));
    tokens.push(typ);
    tokens.push(tok_str("]"));
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
            ts.extend(imps.into_iter().map(tok_str).collect::<Vec<_>>());
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

#[cfg(test)]
mod test {
    use super::*;
    use ast::{
        location, Alias, AtomicType, Builtin, Enum, EnumVariant, Field, RefType, SrcSpan, Struct,
        Type, VariantValue,
    };

    #[test]
    fn atomic_type() {
        let expr = dare::AtomicTypeParser::new().parse("String").unwrap();
        assert_eq!(expr, AtomicType::Str);
    }

    #[test]
    fn ref_type() {
        let expr = dare::RefTypeParser::new().parse("Foo").unwrap();
        assert_eq!(
            expr,
            RefType {
                location: location(0, 3),
                name: "Foo".to_string(),
                type_parameters: vec![]
            }
        );
    }

    #[test]
    fn parametrised_ref_type() {
        let expr = dare::RefTypeParser::new()
            .parse("Foo<T1, Bar<T1>>")
            .unwrap();
        assert_eq!(
            expr,
            RefType {
                location: location(0, 16),
                name: "Foo".to_string(),
                type_parameters: vec![
                    make_generic("T1", vec![], location(4, 6)),
                    Type::Reference(RefType {
                        location: location(8, 15),
                        name: "Bar".to_string(),
                        type_parameters: vec![make_generic("T1", vec![], location(12, 14))],
                    }),
                ]
            }
        );
    }

    #[test]
    fn atomic_ref_type() {
        let expr = dare::RefTypeParser::new().parse("Foo<Int>").unwrap();
        assert_eq!(
            expr,
            RefType {
                location: location(0, 8),
                name: "Foo".to_string(),
                type_parameters: vec![Type::Atomic(AtomicType::Int),]
            }
        );
    }

    #[test]
    fn simple_struct() {
        let expr = dare::StructParser::new()
            .parse(
                "struct MyStruct {
            field1: Int,
            field2: Bool,
        }",
            )
            .unwrap();
        assert_eq!(
            expr,
            Struct {
                location: location(0, 78),
                name: "MyStruct".to_string(),
                type_parameters: vec![],
                fields: vec![
                    Field {
                        location: location(30, 41),
                        name: "field1".to_string(),
                        typ: Type::Atomic(AtomicType::Int)
                    },
                    Field {
                        location: location(55, 67),
                        name: "field2".to_string(),
                        typ: Type::Atomic(AtomicType::Bool)
                    }
                ],
            }
        );
    }

    #[test]
    fn parametrised_struct() {
        let expr = dare::StructParser::new()
            .parse("struct MyStruct<Foo, Bar> {field1: Foo}")
            .unwrap();

        assert_eq!(
            expr,
            Struct {
                location: location(0, 39),
                name: "MyStruct".to_string(),
                type_parameters: vec!["Foo".to_string(), "Bar".to_string()],
                fields: vec![Field {
                    location: location(27, 38),
                    name: "field1".to_string(),
                    typ: make_generic("Foo", vec![], location(35, 38)),
                },]
            }
        )
    }

    #[test]
    fn simple_enum() {
        let expr = dare::EnumParser::new().parse("enum Base {A, G}").unwrap();

        assert_eq!(
            expr,
            Enum {
                location: location(0, 16),
                name: "Base".to_string(),
                type_parameters: vec![],
                variants: vec![
                    EnumVariant {
                        location: location(11, 12),
                        name: "A".to_string(),
                        value: VariantValue::Nothing,
                    },
                    EnumVariant {
                        location: location(14, 15),
                        name: "G".to_string(),
                        value: VariantValue::Nothing,
                    }
                ],
            }
        )
    }

    #[test]
    fn enum_with_atomic_type() {
        let expr = dare::EnumParser::new()
            .parse("enum MaybeInt {Nothing, Just<Int>}")
            .unwrap();

        assert_eq!(
            expr,
            Enum {
                location: location(0, 34),
                name: "MaybeInt".to_string(),
                type_parameters: vec![],
                variants: vec![
                    EnumVariant {
                        location: location(15, 22),
                        name: "Nothing".to_string(),
                        value: VariantValue::Nothing,
                    },
                    EnumVariant {
                        location: location(24, 33),
                        name: "Just".to_string(),
                        value: VariantValue::Type(vec![Type::Atomic(AtomicType::Int)]),
                    }
                ],
            }
        )
    }

    #[test]
    fn enum_with_polymorphic_variant() {
        let expr = dare::EnumParser::new()
            .parse("enum Maybe<T> {Nothing, Just<T>}")
            .unwrap();

        assert_eq!(
            expr,
            Enum {
                location: location(0, 32),
                name: "Maybe".to_string(),
                type_parameters: vec!["T".to_string()],
                variants: vec![
                    EnumVariant {
                        location: location(15, 22),
                        name: "Nothing".to_string(),
                        value: VariantValue::Nothing,
                    },
                    EnumVariant {
                        location: location(24, 31),
                        name: "Just".to_string(),
                        value: VariantValue::Type(vec![make_generic(
                            "T",
                            vec![],
                            location(29, 30)
                        )]),
                    }
                ],
            }
        )
    }

    #[test]
    fn type_alias_with_parameters() {
        let expr = dare::AliasParser::new()
            .parse("type MyResult = Result<String, Int>")
            .unwrap();
        assert_eq!(
            expr,
            Alias {
                location: location(0, 35),
                name: "MyResult".to_string(),
                alias: RefType {
                    location: location(16, 35),
                    name: "Result".to_string(),
                    type_parameters: vec![
                        Type::Atomic(AtomicType::Str),
                        Type::Atomic(AtomicType::Int),
                    ],
                }
            }
        );
    }

    #[test]
    fn list_with_atomic() {
        let expr = dare::TypeParser::new().parse("List<Int>").unwrap();
        assert_eq!(
            expr,
            Type::Builtin(Builtin::List(Box::new(Type::Atomic(AtomicType::Int)))),
        );
    }

    #[test]
    fn list_with_ref_type() {
        let expr = dare::TypeParser::new().parse("List<Foo>").unwrap();
        assert_eq!(
            expr,
            Type::Builtin(Builtin::List(Box::new(make_generic(
                "Foo",
                vec![],
                location(5, 8)
            )))),
        );
    }

    #[test]
    fn list_with_parameter() {
        let expr = dare::TypeParser::new().parse("List<Foo<Int>>").unwrap();
        assert_eq!(
            expr,
            Type::Builtin(Builtin::List(Box::new(Type::Reference(RefType {
                location: location(5, 13),
                name: "Foo".to_string(),
                type_parameters: vec![Type::Atomic(AtomicType::Int)]
            })))),
        );
    }

    #[test]
    fn optional_full() {
        let expr_full = dare::TypeParser::new().parse("Optional<String>").unwrap();
        let expected = Type::Builtin(Builtin::Optional(Box::new(Type::Atomic(AtomicType::Str))));
        assert_eq!(expr_full, expected);
    }

    #[test]
    fn optional_short() {
        let expr_short = dare::TypeParser::new().parse("String ?").unwrap();
        let expected = Type::Builtin(Builtin::Optional(Box::new(Type::Atomic(AtomicType::Str))));
        assert_eq!(expr_short, expected);
    }

    #[test]
    fn optional_complex_type() {
        let expr_short = dare::TypeParser::new().parse("Foo<Int>?").unwrap();
        let expected = Type::Builtin(Builtin::Optional(Box::new(make_generic(
            "Foo",
            vec![Type::Atomic(AtomicType::Int)],
            location(0, 8),
        ))));
        assert_eq!(expr_short, expected);
    }

    #[test]
    fn map_simple() {
        let expr = dare::TypeParser::new().parse("Map<String, Foo>").unwrap();
        let expected = Type::Builtin(Builtin::Map(
            Box::new(Type::Atomic(AtomicType::Str)),
            Box::new(make_generic("Foo", vec![], location(12, 15))),
        ));
        assert_eq!(expr, expected)
    }

    #[test]
    fn anon_struct() {
        let expr = dare::TypeParser::new()
            .parse("Optional<{foo: Int}>")
            .unwrap();
        let expected = Type::Builtin(Builtin::Optional(Box::new(Type::Anonymous(vec![Field {
            location: location(10, 18),
            name: "foo".to_string(),
            typ: Type::Atomic(AtomicType::Int),
        }]))));
        assert_eq!(expr, expected)
    }

    #[test]
    fn anon_struct_in_enum() {
        let expr = dare::EnumParser::new()
            .parse("enum Foo {Stuff{f1: Int, f2: String}}")
            .unwrap();
        let expected = Enum {
            location: location(0, 37),
            name: "Foo".to_string(),
            type_parameters: vec![],
            variants: vec![EnumVariant {
                location: location(10, 36),
                name: "Stuff".to_string(),
                value: VariantValue::Type(vec![Type::Anonymous(vec![
                    Field {
                        location: location(16, 23),
                        name: "f1".to_string(),
                        typ: Type::Atomic(AtomicType::Int),
                    },
                    Field {
                        location: location(25, 35),
                        name: "f2".to_string(),
                        typ: Type::Atomic(AtomicType::Str),
                    },
                ])]),
            }],
        };
        assert_eq!(expr, expected)
    }

    #[test]
    fn anon_struct_in_struct() {
        let expr = dare::StructParser::new()
            .parse("struct Foo {base: {f1: Int}}")
            .unwrap();
        let expected = Struct {
            location: location(0, 28),
            name: "Foo".to_string(),
            type_parameters: vec![],
            fields: vec![Field {
                location: location(12, 27),
                name: "base".to_string(),
                typ: Type::Anonymous(vec![Field {
                    location: location(19, 26),
                    name: "f1".to_string(),
                    typ: Type::Atomic(AtomicType::Int),
                }]),
            }],
        };
        assert_eq!(expr, expected)
    }

    fn make_generic(name: &str, params: Vec<Type>, loc: SrcSpan) -> Type {
        Type::Reference(RefType {
            location: loc,
            name: name.to_string(),
            type_parameters: params,
        })
    }
}
