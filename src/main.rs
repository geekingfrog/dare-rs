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
        Alias, AtomicType, Builtin, Enum, EnumVariant, Field, RefType, Struct, Type, VariantValue,
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
                name: "Foo".to_string(),
                type_parameters: vec![
                    make_generic("T1", vec![]),
                    Type::Reference(RefType {
                        name: "Bar".to_string(),
                        type_parameters: vec![make_generic("T1", vec![])],
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
                name: "MyStruct".to_string(),
                type_parameters: vec![],
                fields: vec![
                    Field {
                        name: "field1".to_string(),
                        typ: Type::Atomic(AtomicType::Int)
                    },
                    Field {
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
                name: "MyStruct".to_string(),
                type_parameters: vec!["Foo".to_string(), "Bar".to_string()],
                fields: vec![Field {
                    name: "field1".to_string(),
                    typ: make_generic("Foo", vec![]),
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
                name: "Base".to_string(),
                type_parameters: vec![],
                variants: vec![
                    EnumVariant {
                        name: "A".to_string(),
                        value: VariantValue::Nothing,
                    },
                    EnumVariant {
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
                name: "MaybeInt".to_string(),
                type_parameters: vec![],
                variants: vec![
                    EnumVariant {
                        name: "Nothing".to_string(),
                        value: VariantValue::Nothing,
                    },
                    EnumVariant {
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
                name: "Maybe".to_string(),
                type_parameters: vec!["T".to_string()],
                variants: vec![
                    EnumVariant {
                        name: "Nothing".to_string(),
                        value: VariantValue::Nothing,
                    },
                    EnumVariant {
                        name: "Just".to_string(),
                        value: VariantValue::Type(vec![make_generic("T", vec![])]),
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
                name: "MyResult".to_string(),
                alias: RefType {
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
            Type::Builtin(Builtin::List(Box::new(make_generic("Foo", vec![])))),
        );
    }

    #[test]
    fn list_with_parameter() {
        let expr = dare::TypeParser::new().parse("List<Foo<Int>>").unwrap();
        assert_eq!(
            expr,
            Type::Builtin(Builtin::List(Box::new(Type::Reference(RefType {
                name: "Foo".to_string(),
                type_parameters: vec![Type::Atomic(AtomicType::Int)]
            })))),
        );
    }

    #[test]
    fn optional_full() {
        let expr_full = dare::TypeParser::new().parse("Option<String>").unwrap();
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
        ))));
        assert_eq!(expr_short, expected);
    }

    #[test]
    fn map_simple() {
        let expr = dare::TypeParser::new().parse("Map<String, Foo>").unwrap();
        let expected = Type::Builtin(Builtin::Map(
            Box::new(Type::Atomic(AtomicType::Str)),
            Box::new(make_generic("Foo", vec![])),
        ));
        assert_eq!(expr, expected)
    }

    fn make_generic(name: &str, params: Vec<Type>) -> Type {
        Type::Reference(RefType {
            name: name.to_string(),
            type_parameters: params,
        })
    }
}
