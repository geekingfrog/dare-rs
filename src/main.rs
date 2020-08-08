// #[macro_use] extern crate lalrpop_util;

// lalrpop_mod!(pub dare);
pub mod ast;
pub mod dare;

fn main() {
    println!("Hello, world!");
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
