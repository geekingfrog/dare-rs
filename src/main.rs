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
    use ast::{Alias, AtomicType, Enum, EnumVariant, Field, RefType, Struct, Type, VariantValue};

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
        let expr = dare::RefTypeParser::new().parse("Foo<T1, T2>").unwrap();
        assert_eq!(
            expr,
            RefType {
                name: "Foo".to_string(),
                type_parameters: vec![
                    Type::Generic("T1".to_string()),
                    Type::Generic("T2".to_string())
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
                    typ: Type::Generic("Foo".to_string()),
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
                        value: VariantValue::Type(vec![Type::Generic("T".to_string())]),
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
}
