pub mod ast;
pub mod dare;

#[macro_use]
extern crate anyhow;
use anyhow::Result;

use std::collections::{BTreeMap, BTreeSet};
use std::fs::File;
use std::io::{prelude::*, BufReader};

fn main() {
    let f = File::open("coucou.dare").unwrap();
    let result = parse(&f).unwrap();
    println!("{:?}", result);

    println!("");

    let toks = gen_python(&result);
    let imports = gather_imports(&toks);
    // println!("imports: {:?}", imports);
    let conf = PyConfig { indent_size: 4 };
    let mut ctx = FormatContext { indent_level: 0 };
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

fn gen_python(_decl: &ast::TopDeclaration) -> Vec<PyToken> {
    let mut tokens = Vec::new();

    let any = PyToken::Import(
        PyImport::Specific("typing".to_string(), "Any".to_string()),
        vec![],
    );
    let mut args = vec![tok_str("self")];
    args.push(typed_attr(
        "message",
        vec![typed_union(vec![
            tok_str("str"),
            typed_list(any.clone()),
            typed_dict(tok_str("str"), any.clone()),
        ])],
        None,
    ));

    args.push(typed_attr("data", vec![typed_optional(any)], None));

    let mut body = vec![];
    body.push(tok_str("self.messages = [message] if isinstance(message, (str, bytes)) else message"));
    body.push(PyToken::NewLine);
    body.push(tok_str("self.data = data"));
    body.push(PyToken::NewLine);
    body.push(tok_str("super().__init__(message)"));
    body.push(PyToken::NewLine);

    let init = pyfn(
        "__init__",
        args,
        body,
        None,
    );
    tokens.extend(pyclass("ValidationError", vec![tok_str("Exception")], init));

    tokens
}

fn gather_imports(tokens: &Vec<PyToken>) -> BTreeMap<&str, ImportMap> {
    let mut imports = BTreeMap::new();
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

fn render(conf: &PyConfig, mut ctx: &mut FormatContext, tokens: Vec<PyToken>) -> String {
    let mut buf: Vec<String> = Vec::new();
    let mut indent_prefix = "".to_string();
    let mut should_indent = false;

    for tok in tokens.into_iter() {
        match tok.clone() {
            PyToken::Raw(s) => {
                if should_indent && !indent_prefix.is_empty() {
                    buf.push(indent_prefix.clone());
                }
                buf.push(s);
                should_indent = false;
            }
            PyToken::Import(imp, inner) => {
                buf.push(imp.sym());
                should_indent = false;
            }
            PyToken::Space => {
                buf.push(" ".to_string());
                should_indent = false;
            },
            PyToken::NewLine => {
                buf.push("\n".to_string());
                should_indent = true;
            }
            PyToken::Indent(i) => {
                let il = ctx.indent_level as i8;
                ctx.indent_level = (il + i) as usize;
                indent_prefix = " ".repeat(ctx.indent_level * conf.indent_size);
                should_indent = true;
            }
            PyToken::Block(toks) => {
                buf.push(render(&conf, &mut ctx, toks));
                should_indent = false;
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
    /// Import(PyImport::Full("typing"), vec![PyToken::Raw("Union".to_string())])
    /// `from typing import Union`
    /// would give
    /// Import(PyImport::Specific("typing", "Union"), vec![])
    Import(PyImport, Vec<PyToken>),

    Space,
    NewLine,

    /// Control indentation level
    /// Indent(1) would increase the level by one.
    /// The translation into tabs/whitespace is done when turning tokens into string
    Indent(i8),
    Block(Vec<PyToken>),
}

#[derive(Clone, Debug)]
struct PyConfig {
    indent_size: usize,
}

struct FormatContext {
    indent_level: usize,
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
    tokens.push(PyToken::NewLine);
    tokens.push(indent(1));
    tokens.extend(body);
    tokens.push(indent(-1));

    tokens
}

fn pyfn(
    name: &str,
    arguments: Vec<PyToken>,
    body: Vec<PyToken>,
    return_type: Option<Vec<PyToken>>,
) -> Vec<PyToken> {
    let mut tokens = Vec::new();
    tokens.push(tok_str("def "));
    tokens.push(tok_str(name));

    if arguments.is_empty() {
        tokens.push(tok_str("()"));
    } else {
        tokens.push(tok_str("("));
        tokens.push(PyToken::NewLine);
        tokens.push(indent(1));
        tokens.extend(intersperce(tok_str(", "), arguments));
        tokens.push(PyToken::NewLine);
        tokens.push(indent(-1));
        tokens.push(tok_str(")"));
    }

    if let Some(ret_type) = return_type {
        tokens.push(tok_str(" -> "));
        tokens.extend(ret_type);
    }
    tokens.push(tok_str(":"));

    tokens.push(PyToken::NewLine);
    tokens.push(indent(1));
    if body.is_empty() {
        tokens.push(tok_str("pass"));
    } else {
        tokens.extend(body);
    }
    tokens.push(PyToken::NewLine);
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
    let dict = type_import("Dict");
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
    PyToken::Import(typ, vec![])
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
