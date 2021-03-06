use crate::lexer::{LexicalError, Loc, SrcSpan};
use anyhow::Result;
use std::{collections::BTreeMap, vec::Vec};
use std::{convert::TryFrom, fmt::Debug};

#[derive(Debug, PartialEq, Eq)]
pub enum TopDeclaration {
    Struct(Struct),
    Enum(Enum),
    Alias(Alias),
}

impl TopDeclaration {
    pub(crate) fn get_name(&self) -> &str {
        match self {
            TopDeclaration::Struct(s) => &s.name,
            TopDeclaration::Enum(e) => &e.name,
            TopDeclaration::Alias(a) => &a.name,
        }
    }
}

/// struct Foo {…}
/// struct Bar<A, B> {…}
#[derive(Debug, PartialEq, Eq)]
pub struct Struct {
    pub location: SrcSpan,
    pub name: String,
    // only generic types are allowed in definition
    pub type_parameters: Vec<String>,
    pub fields: Vec<Field>,
}

/// field1: Int,
/// field2: Foo,
/// field3: Bar<T>,
/// field4: Bar<Int>,
/// field5: Bar<Map<String, V>>
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Field {
    pub location: SrcSpan,
    pub name: String,
    pub typ: FieldType,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum FieldType {
    TypeOf(String),
    Type(Type),
}

/// enum Foo = {A, B, C, D}
/// enum Bar = {String, Int}
/// enum Maybe<T> = {Nothing, Just(T)}
/// enum Detailed<A,B> = {Nothing, Something(A,B)}
/// enum WithFields = {A, B{b_val: Int}}
/// the last example, when monomorphised would give:
/// ["Detailed", "val_A", val_B""]
///  or {"type": "Detailed", "value": ["val_A", "val_B"]}
#[derive(Debug, PartialEq, Eq)]
pub struct Enum {
    pub location: SrcSpan,
    pub name: String,
    // only generic types are allowed in definition
    pub type_parameters: Vec<String>,
    pub variants: Vec<EnumVariant>,
    pub directives: Directives,
}

/// type MaybeInt = Maybe<Int>
#[derive(Debug, PartialEq, Eq)]
pub struct Alias {
    pub location: SrcSpan,
    pub name: String,
    pub alias: Type,
}

#[derive(Debug, PartialEq, Eq)]
pub struct EnumVariant {
    pub location: SrcSpan,
    pub name: String,
    pub alias: Option<String>,
    pub value: VariantValue,
}

#[derive(Debug, PartialEq, Eq)]
pub enum VariantValue {
    OnlyCtor,
    PositionalCtor(Vec<Type>),
    StructCtor(Vec<Field>),
}

// impl VariantValue {
//     pub fn is_only_ctor(&self) -> bool {
//         match &self {
//             VariantValue::OnlyCtor => true,
//             _ => false,
//         }
//     }
// }

/// A type can be atomic (String, Bool, Int…), a reference to another type
/// (Foo, Bar<T>, Map<String, Int>), or a generic type like `T` or `ErrorType`.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    /// One of the basic atomic type
    Atomic(AtomicType),
    /// Generic type or reference to existing type
    /// like Barf<Foo, T, Bar<Int>>
    Reference(RefType),
    // /// Anonymous structure like List<{foo: Int}>
    // Anonymous(Vec<Field>),
    /// Builtin type like List, Map and Optional
    Builtin(Builtin),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum AtomicType {
    Str,
    UInt,
    Int,
    Int8,
    Int16,
    Int32,
    Int64,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Float,
    Bool,
    Bytes,
}

/// A reference to another type. Like Foo, or Bar<T>
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct RefType {
    pub location: SrcSpan,
    pub name: String,
    pub type_parameters: Vec<Type>,
}

/// Index for the reference with regard to the list of TopDeclaration
/// Cannot directly use a Box<TopDeclaration<Reference>> because of cyclic declaration
pub type ResolvedReference = usize;

// /// Used after AST validation, where the generic type is resolved
// /// to a primitive type, or another declared type
// pub struct ResolvedRef {
//     pub location: ScrSpan,
//     pub target: ?,
//     pub type_parameters: Vec<Type>,
// }

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Builtin {
    List(Box<Type>),
    Optional(Box<Type>),
    Map(Box<Type>, Box<Type>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct JsonDirective {
    pub repr: JsonRepr,
    pub tag: String,
    pub content: String,
}

impl Default for JsonDirective {
    fn default() -> Self {
        JsonDirective {
            repr: JsonRepr::default(),
            tag: "tag".to_string(),
            content: "contents".to_string(),
        }
    }
}

impl JsonDirective {
    fn parse(args: Vec<DirectiveArg>) -> Result<Self, LexicalError> {
        let mut result = JsonDirective::default();

        for arg in args {
            if arg.key == "repr" {
                match JsonRepr::try_from(arg.value) {
                    Ok(r) => result.repr = r,
                    Err(err) => return Err(LexicalError::ParseError(arg.start, arg.end, err)),
                }
            } else if arg.key == "tag" {
                if arg.value.is_empty() {
                    return Err(LexicalError::ParseError(
                        arg.start,
                        arg.end,
                        "tag cannot be empty".to_string(),
                    ));
                }
                result.tag = arg.value;
            } else if arg.key == "content" {
                if arg.value.is_empty() {
                    return Err(LexicalError::ParseError(
                        arg.start,
                        arg.end,
                        "content cannot be empty".to_string(),
                    ));
                }
                result.content = arg.value;
            } else {
                return Err(LexicalError::ParseError(
                    arg.start,
                    arg.end,
                    format!("Unknown json directive: {}", arg.key),
                ));
            }
        }

        Ok(result)
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum JsonRepr {
    Object,
    Tuple,
    Union,
}

impl Default for JsonRepr {
    fn default() -> Self {
        JsonRepr::Object
    }
}

impl TryFrom<String> for JsonRepr {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        if value == "nested" {
            Ok(JsonRepr::Object)
        } else if value == "tuple" {
            Ok(JsonRepr::Tuple)
        } else if value == "union" {
            Ok(JsonRepr::Union)
        } else {
            Err(format!("Unknown json directive argument: {}", value))
        }
    }
}

#[derive(Debug, PartialEq, Eq, Default, Clone)]
pub struct Directives {
    pub json: Option<(SrcSpan, JsonDirective)>,
}

impl Directives {
    pub(crate) fn parse(
        s: Loc,
        e: Loc,
        dir_type: String,
        args: Vec<DirectiveArg>,
    ) -> Result<Self, LexicalError> {
        if dir_type == "json" {
            JsonDirective::parse(args).map(|json| Directives {
                json: Some((SrcSpan { start: s, end: e }, json)),
            })
        } else {
            Err(LexicalError::ParseError(
                s,
                e,
                format!("Unknown directive: {}", dir_type),
            ))
        }
    }
}

#[derive(Debug)]
pub struct DirectiveArg {
    pub start: Loc,
    pub end: Loc,
    pub key: String,
    pub value: String,
}

// type Mappings = BTreeMap<String, usize>;
//
// pub(crate) fn validate_ast(
//     decls: Vec<TopDeclaration<String>>,
// ) -> Result<Vec<TopDeclaration<ResolvedReference>>> {
//     let mappings: Mappings = decls
//         .iter()
//         .enumerate()
//         .map(|(idx, d)| (d.get_name().to_owned(), idx))
//         .collect();
//
//     // let mut f: Vec<TopDeclaration<ResolvedReference>> = vec![];
//     decls
//         .into_iter()
//         .map(|decl| match decl {
//             TopDeclaration::Struct(s) => {
//                 validate_struct(&mappings, s).map(|x| TopDeclaration::Struct(x))
//             }
//             TopDeclaration::Enum(e) => validate_enum(&mappings, e).map(|x| TopDeclaration::Enum(x)),
//             TopDeclaration::Alias(_) => Err(anyhow!("boom")),
//         })
//         .collect()
// }
//
// fn validate_struct(mappings: &Mappings, s: Struct<String>) -> Result<Struct<usize>> {
//     let fields = s
//         .fields
//         .into_iter()
//         .map(|f| validate_field(&mappings, f))
//         .collect::<Result<Vec<_>>>();
//
//     Ok(Struct {
//         location: s.location,
//         name: s.name.clone(),
//         type_parameters: s.type_parameters.clone(),
//         fields: fields?,
//     })
// }
//
// fn validate_field(mappings: &Mappings, f: Field<String>) -> Result<Field<usize>> {
//     Ok(Field {
//         location: f.location,
//         name: f.name,
//         typ: validate_field_type(&mappings, f.typ)?,
//     })
// }
//
// fn validate_field_type(mappings: &Mappings, f: FieldType<String>) -> Result<FieldType<usize>> {
//     match f {
//         FieldType::TypeOf(x) => Ok(FieldType::TypeOf(x)),
//         FieldType::Type(t) => {
//             let validated = validate_type(&mappings, t)?;
//             Ok(FieldType::Type(validated))
//         }
//     }
// }
//
// fn validate_enum(mappings: &Mappings, e: Enum<String>) -> Result<Enum<usize>> {
//     let variants = e
//         .variants
//         .into_iter()
//         .map(|v| validate_variant(&mappings, v))
//         .collect::<Result<Vec<_>>>();
//     Ok(Enum {
//         location: e.location,
//         name: e.name,
//         type_parameters: e.type_parameters,
//         variants: variants?,
//         directives: e.directives, // TODO validate directives
//     })
// }
//
// fn validate_variant(mappings: &Mappings, ev: EnumVariant<String>) -> Result<EnumVariant<usize>> {
//     Ok(EnumVariant {
//         location: ev.location,
//         name: ev.name,
//         alias: ev.alias,
//         value: validate_variant_value(&mappings, ev.value)?,
//     })
// }
//
// fn validate_variant_value(
//     mappings: &Mappings,
//     v: VariantValue<String>,
// ) -> Result<VariantValue<usize>> {
//     match v {
//         VariantValue::OnlyCtor => Ok(VariantValue::OnlyCtor),
//         VariantValue::PositionalCtor(typs) => typs
//             .into_iter()
//             .map(|t| validate_type(&mappings, t))
//             .collect::<Result<Vec<_>>>()
//             .map(|x| VariantValue::PositionalCtor(x)),
//         VariantValue::StructCtor(fields) => fields
//             .into_iter()
//             .map(|f| validate_field(&mappings, f))
//             .collect::<Result<Vec<_>>>()
//             .map(|x| VariantValue::StructCtor(x)),
//     }
// }
//
// fn validate_type(mappings: &Mappings, t: Type<String>) -> Result<Type<usize>> {
//     match t {
//         Type::Atomic(t) => Ok(Type::Atomic(t)),
//         Type::Reference(r) => mappings
//             .get(&r.name)
//             .ok_or(anyhow!("Reference {} not found", r.name).into())
//             .and_then(|i| {
//                 let params = r
//                     .type_parameters
//                     .into_iter()
//                     .map(|t| validate_type(&mappings, t))
//                     .collect::<Result<Vec<_>>>();
//
//                 Ok(Type::Reference(RefType {
//                     location: r.location,
//                     name: *i,
//                     type_parameters: params?,
//                 }))
//             }),
//         Type::Builtin(b) => {
//             let b2 = match b {
//                 Builtin::List(inner) => Builtin::List(Box::new(validate_type(mappings, *inner)?)),
//                 Builtin::Optional(inner) => {
//                     Builtin::Optional(Box::new(validate_type(mappings, *inner)?))
//                 }
//                 Builtin::Map(k, v) => Builtin::Map(
//                     Box::new(validate_type(mappings, *k)?),
//                     Box::new(validate_type(mappings, *v)?),
//                 ),
//             };
//             Ok(Type::Builtin(b2))
//         }
//     }
// }
