#[macro_use]
extern crate anyhow;
use anyhow::Result;

#[macro_use]
extern crate strum_macros;
extern crate strum;

use std::fs::File;
use std::io::{prelude::*, BufReader};
use std::path::PathBuf;
use structopt::StructOpt;

mod ast;
mod dare;
mod lexer;
mod python;

use lexer::Lexer;

#[derive(StructOpt, Debug)]
// #[structopt(name = "basic")]
struct Opt {
    #[structopt(long)]
    schema: PathBuf,

    #[structopt(long)]
    target_dir: Option<PathBuf>,
}

fn main() {
    let opt = Opt::from_args();

    let f = File::open(&opt.schema).unwrap();
    let raw_ast = parse(&f).unwrap();
    let validated_ast = ast::validate_ast(raw_ast).unwrap();

    let py_tokens = python::gen_python(&validated_ast[..]);

    println!("{}", python::render_python(&py_tokens));

    let mut dest_path_default = opt.schema.clone();
    dest_path_default.pop();
    let mut dest_path = opt.target_dir.unwrap_or(dest_path_default);
    dest_path.push(&opt.schema.file_stem().unwrap());
    dest_path.set_extension("py");
    let mut dest = File::create(&dest_path).unwrap();
    println!("writing result to {:?}", dest_path);
    dest.write_all("# File generated by the dare compiler, do not manually edit !\n\n".as_bytes())
        .unwrap();
    dest.write_all(&python::render_python(&py_tokens).as_bytes())
        .unwrap();
}

fn parse(f: &File) -> Result<Vec<ast::TopDeclaration<String>>> {
    let mut buf_reader = BufReader::new(f);
    let mut content = String::new();
    buf_reader.read_to_string(&mut content)?;
    dare::TopDeclarationsParser::new()
        .parse(Lexer::new(&content))
        .map_err(|e| anyhow!("Parse error {}", e))
}

#[cfg(test)]
mod test {
    use super::*;
    use ast::{
        Alias, AtomicType, Builtin, Directives, Enum, EnumVariant, Field, JsonDirective, JsonRepr,
        RefType, Struct, Type, VariantValue,
    };
    use lexer::{Loc, SrcSpan};
    use pretty_assertions::assert_eq;

    fn loc(s: (usize, usize), e: (usize, usize)) -> SrcSpan {
        SrcSpan {
            start: Loc {
                line: s.0,
                column: s.1,
            },
            end: Loc {
                line: e.0,
                column: e.1,
            },
        }
    }

    #[test]
    fn atomic_type() {
        let expr = dare::AtomicTypeParser::new()
            .parse(Lexer::new("String"))
            .unwrap();
        assert_eq!(expr, AtomicType::Str);
    }

    #[test]
    fn ref_type() {
        let expr = dare::RefTypeParser::new().parse(Lexer::new("Foo")).unwrap();
        assert_eq!(
            expr,
            RefType {
                location: loc((1, 1), (1, 4)),
                name: "Foo".to_string(),
                type_parameters: vec![]
            }
        );
    }

    #[test]
    fn parametrised_ref_type() {
        let expr = dare::RefTypeParser::new()
            .parse(Lexer::new("Foo<T1, Bar<T1>>"))
            .unwrap();
        assert_eq!(
            expr,
            RefType {
                location: loc((1, 1), (1, 17)),
                name: "Foo".to_string(),
                type_parameters: vec![
                    make_generic("T1", vec![], loc((1, 5), (1, 7))),
                    Type::Reference(RefType {
                        location: loc((1, 9), (1, 16)),
                        name: "Bar".to_string(),
                        type_parameters: vec![make_generic("T1", vec![], loc((1, 13), (1, 15)))],
                    }),
                ]
            }
        );
    }

    #[test]
    fn atomic_ref_type() {
        let expr = dare::RefTypeParser::new()
            .parse(Lexer::new("Foo<Int>"))
            .unwrap();
        assert_eq!(
            expr,
            RefType {
                location: loc((1, 1), (1, 9)),
                name: "Foo".to_string(),
                type_parameters: vec![Type::Atomic(AtomicType::Int),]
            }
        );
    }

    #[test]
    fn simple_struct() {
        let expr = dare::StructParser::new()
            .parse(Lexer::new(
                "struct MyStruct {
            field1: Int,
            field2: Bool,
        }",
            ))
            .unwrap();
        assert_eq!(
            expr,
            Struct {
                location: loc((1, 1), (4, 10)),
                name: "MyStruct".to_string(),
                type_parameters: vec![],
                fields: vec![
                    Field {
                        location: loc((2, 13), (2, 24)),
                        name: "field1".to_string(),
                        typ: Type::Atomic(AtomicType::Int)
                    },
                    Field {
                        location: loc((3, 13), (3, 25)),
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
            .parse(Lexer::new("struct MyStruct<Foo, Bar> {field1: Foo}"))
            .unwrap();

        assert_eq!(
            expr,
            Struct {
                location: loc((1, 1), (1, 40)),
                name: "MyStruct".to_string(),
                type_parameters: vec!["Foo".to_string(), "Bar".to_string()],
                fields: vec![Field {
                    location: loc((1, 28), (1, 39)),
                    name: "field1".to_string(),
                    typ: make_generic("Foo", vec![], loc((1, 36), (1, 39))),
                },]
            }
        )
    }

    #[test]
    fn simple_enum() {
        let expr = dare::EnumParser::new()
            .parse(Lexer::new("enum Base {A, G}"))
            .unwrap();

        assert_eq!(
            expr,
            Enum {
                location: loc((1, 1), (1, 17)),
                name: "Base".to_string(),
                type_parameters: vec![],
                variants: vec![
                    EnumVariant {
                        location: loc((1, 12), (1, 13)),
                        name: "A".to_string(),
                        alias: None,
                        value: VariantValue::OnlyCtor,
                    },
                    EnumVariant {
                        location: loc((1, 15), (1, 16)),
                        name: "G".to_string(),
                        alias: None,
                        value: VariantValue::OnlyCtor,
                    }
                ],
                directives: Directives::default(),
            }
        )
    }

    #[test]
    fn enum_with_atomic_type() {
        let expr = dare::EnumParser::new()
            .parse(Lexer::new("enum MaybeInt {Nothing, Just(Int)}"))
            .unwrap();

        assert_eq!(
            expr,
            Enum {
                location: loc((1, 1), (1, 35)),
                name: "MaybeInt".to_string(),
                type_parameters: vec![],
                variants: vec![
                    EnumVariant {
                        location: loc((1, 16), (1, 23)),
                        name: "Nothing".to_string(),
                        alias: None,
                        value: VariantValue::OnlyCtor,
                    },
                    EnumVariant {
                        location: loc((1, 25), (1, 34)),
                        name: "Just".to_string(),
                        alias: None,
                        value: VariantValue::PositionalCtor(vec![Type::Atomic(AtomicType::Int)]),
                    }
                ],
                directives: Directives::default(),
            }
        )
    }

    #[test]
    fn enum_with_polymorphic_variant() {
        let expr = dare::EnumParser::new()
            .parse(Lexer::new("enum Maybe<T> {Nothing, Just(T)}"))
            .unwrap();

        assert_eq!(
            expr,
            Enum {
                location: loc((1, 1), (1, 33)),
                name: "Maybe".to_string(),
                type_parameters: vec!["T".to_string()],
                variants: vec![
                    EnumVariant {
                        location: loc((1, 16), (1, 23)),
                        name: "Nothing".to_string(),
                        alias: None,
                        value: VariantValue::OnlyCtor,
                    },
                    EnumVariant {
                        location: loc((1, 25), (1, 32)),
                        name: "Just".to_string(),
                        alias: None,
                        value: VariantValue::PositionalCtor(vec![make_generic(
                            "T",
                            vec![],
                            loc((1, 30), (1, 31))
                        )]),
                    }
                ],
                directives: Directives::default(),
            }
        )
    }

    #[test]
    fn type_alias_with_parameters() {
        let expr = dare::AliasParser::new()
            .parse(Lexer::new("type MyResult = Result<String, Int>"))
            .unwrap();
        assert_eq!(
            expr,
            Alias {
                location: loc((1, 1), (1, 36)),
                name: "MyResult".to_string(),
                alias: RefType {
                    location: loc((1, 17), (1, 36)),
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
        let expr = dare::TypeParser::new()
            .parse(Lexer::new("List<Int>"))
            .unwrap();
        assert_eq!(
            expr,
            Type::Builtin(Builtin::List(Box::new(Type::Atomic(AtomicType::Int)))),
        );
    }

    #[test]
    fn list_with_ref_type() {
        let expr = dare::TypeParser::new()
            .parse(Lexer::new("List<Foo>"))
            .unwrap();
        assert_eq!(
            expr,
            Type::Builtin(Builtin::List(Box::new(make_generic(
                "Foo",
                vec![],
                loc((1, 6), (1, 9))
            )))),
        );
    }

    #[test]
    fn list_with_parameter() {
        let expr = dare::TypeParser::new()
            .parse(Lexer::new("List<Foo<Int>>"))
            .unwrap();
        assert_eq!(
            expr,
            Type::Builtin(Builtin::List(Box::new(Type::Reference(RefType {
                location: loc((1, 6), (1, 14)),
                name: "Foo".to_string(),
                type_parameters: vec![Type::Atomic(AtomicType::Int)]
            })))),
        );
    }

    #[test]
    fn optional_full() {
        let expr_full = dare::TypeParser::new()
            .parse(Lexer::new("Optional<String>"))
            .unwrap();
        let expected = Type::Builtin(Builtin::Optional(Box::new(Type::Atomic(AtomicType::Str))));
        assert_eq!(expr_full, expected);
    }

    #[test]
    fn optional_short() {
        let expr_short = dare::TypeParser::new()
            .parse(Lexer::new("String ?"))
            .unwrap();
        let expected = Type::Builtin(Builtin::Optional(Box::new(Type::Atomic(AtomicType::Str))));
        assert_eq!(expr_short, expected);
    }

    #[test]
    fn optional_complex_type() {
        let expr_short = dare::TypeParser::new()
            .parse(Lexer::new("Foo<Int>?"))
            .unwrap();
        let expected = Type::Builtin(Builtin::Optional(Box::new(make_generic(
            "Foo",
            vec![Type::Atomic(AtomicType::Int)],
            loc((1, 1), (1, 9)),
        ))));
        assert_eq!(expr_short, expected);
    }

    #[test]
    fn map_simple() {
        let expr = dare::TypeParser::new()
            .parse(Lexer::new("Map<String, Foo>"))
            .unwrap();
        let expected = Type::Builtin(Builtin::Map(
            Box::new(Type::Atomic(AtomicType::Str)),
            Box::new(make_generic("Foo", vec![], loc((1, 13), (1, 16)))),
        ));
        assert_eq!(expr, expected)
    }

    #[test]
    fn anon_struct_in_enum() {
        let expr = dare::EnumParser::new()
            .parse(Lexer::new("enum Foo {Stuff{f1: Int, f2: String}}"))
            .unwrap();
        let expected = Enum {
            location: loc((1, 1), (1, 38)),
            name: "Foo".to_string(),
            type_parameters: vec![],
            variants: vec![EnumVariant {
                location: loc((1, 11), (1, 37)),
                name: "Stuff".to_string(),
                alias: None,
                value: VariantValue::StructCtor(vec![
                    Field {
                        location: loc((1, 17), (1, 24)),
                        name: "f1".to_string(),
                        typ: Type::Atomic(AtomicType::Int),
                    },
                    Field {
                        location: loc((1, 26), (1, 36)),
                        name: "f2".to_string(),
                        typ: Type::Atomic(AtomicType::Str),
                    },
                ]),
            }],
            directives: Directives::default(),
        };
        assert_eq!(expr, expected)
    }

    #[test]
    fn sum_with_alias() {
        let expr = dare::EnumParser::new()
            .parse(Lexer::new(r#"enum TestEnum {Simple1 as "one"}"#))
            .unwrap();
        let expected = Enum {
            location: loc((1, 1), (1, 33)),
            name: "TestEnum".to_string(),
            type_parameters: vec![],
            variants: vec![EnumVariant {
                location: loc((1, 16), (1, 32)),
                name: "Simple1".to_string(),
                alias: Some("one".to_string()),
                value: VariantValue::OnlyCtor,
            }],
            directives: Directives::default(),
        };

        assert_eq!(expr, expected);
    }

    #[test]
    fn test_json_directive_tuple() {
        let expr = dare::EnumParser::new()
            .parse(Lexer::new("#[json(tag=\"customTag\", content=\"customContent\", repr = \"tuple\")]\nenum TestEnum {}"))
            .unwrap();
        let expected = Enum {
            location: loc((2, 1), (2, 17)),
            name: "TestEnum".to_string(),
            type_parameters: vec![],
            variants: vec![],
            directives: Directives {
                json: Some((
                    loc((1, 1), (1, 66)),
                    JsonDirective {
                        repr: JsonRepr::Tuple,
                        tag: "customTag".to_string(),
                        content: "customContent".to_string(),
                    },
                )),
            },
        };

        assert_eq!(expr, expected);
    }

    #[test]
    fn test_json_directive_union() {
        let expr = dare::EnumParser::new()
            .parse(Lexer::new("#[json(repr = \"union\")]\nenum TestEnum {}"))
            .unwrap();
        let expected = Enum {
            location: loc((2, 1), (2, 17)),
            name: "TestEnum".to_string(),
            type_parameters: vec![],
            variants: vec![],
            directives: Directives {
                json: Some((
                    loc((1, 1), (1, 24)),
                    JsonDirective {
                        repr: JsonRepr::Union,
                        tag: "tag".to_string(),
                        content: "contents".to_string(),
                    },
                )),
            },
        };

        assert_eq!(expr, expected);
    }

    fn make_generic(name: &str, params: Vec<Type<String>>, loc: SrcSpan) -> Type<String> {
        Type::Reference(RefType {
            location: loc,
            name: name.to_string(),
            type_parameters: params,
        })
    }
}
