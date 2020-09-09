use anyhow::Result;
use std::fmt::Debug;
use std::{collections::BTreeMap, vec::Vec};
use crate::lexer;

#[derive(Debug, PartialEq, Eq)]
pub enum TopDeclaration<Reference> {
    Struct(Struct<Reference>),
    Enum(Enum<Reference>),
    // TODO: type alias
    Alias(Alias<Reference>),
}

impl<T> TopDeclaration<T> {
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
pub struct Struct<Reference> {
    pub location: SrcSpan,
    pub name: String,
    // only generic types are allowed in definition
    pub type_parameters: Vec<String>,
    pub fields: Vec<Field<Reference>>,
}

/// field1: Int,
/// field2: Foo,
/// field3: Bar<T>,
/// field4: Bar<Int>,
/// field5: Bar<Map<String, V>>
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Field<Reference> {
    pub location: SrcSpan,
    pub name: String,
    pub typ: Type<Reference>,
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
pub struct Enum<Reference> {
    pub location: SrcSpan,
    pub name: String,
    // only generic types are allowed in definition
    pub type_parameters: Vec<String>,
    pub variants: Vec<EnumVariant<Reference>>,
}

impl<T> Enum<T> {
    /// returns true if the sum type is actually a simple enum,
    /// that is, only has variants with only constructor and no
    /// data attached to them
    pub fn is_simple_enum(&self) -> bool {
        for v in self.variants.iter() {
            match v.value {
                VariantValue::OnlyCtor => continue,
                _ => return false,
            }
        }
        true
    }
}

/// type MaybeInt = Maybe<Int>
#[derive(Debug, PartialEq, Eq)]
pub struct Alias<Reference> {
    pub location: SrcSpan,
    pub name: String,
    pub alias: RefType<Reference>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct EnumVariant<Reference> {
    pub location: SrcSpan,
    pub name: String,
    pub alias: Option<String>,
    pub value: VariantValue<Reference>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum VariantValue<Reference> {
    OnlyCtor,
    PositionalCtor(Vec<Type<Reference>>),
    StructCtor(Vec<Field<Reference>>),
}

/// A type can be atomic (String, Bool, Int…), a reference to another type
/// (Foo, Bar<T>, Map<String, Int>), or a generic type like `T` or `errorType`.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type<Reference> {
    /// One of the basic atomic type
    Atomic(AtomicType),
    /// Generic type or reference to existing type
    /// like Barf<Foo, T, Bar<Int>>
    Reference(RefType<Reference>),
    // /// Anonymous structure like List<{foo: Int}>
    // Anonymous(Vec<Field>),
    /// Builtin type like List, Map and Optional
    Builtin(Builtin<Reference>),
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
pub struct RefType<Reference> {
    pub location: SrcSpan,
    pub name: Reference,
    pub type_parameters: Vec<Type<Reference>>,
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
pub enum Builtin<Reference> {
    List(Box<Type<Reference>>),
    Optional(Box<Type<Reference>>),
    Map(Box<Type<Reference>>, Box<Type<Reference>>),
}

#[derive(Debug, PartialEq, Eq, Default, Clone, Copy)]
pub struct SrcSpan {
    pub start: lexer::Loc,
    pub end: lexer::Loc,
}

pub fn location(start: lexer::Loc, end: lexer::Loc) -> SrcSpan {
    SrcSpan { start, end }
}

type Mappings = BTreeMap<String, usize>;

pub(crate) fn validate_ast(
    decls: Vec<TopDeclaration<String>>,
) -> Result<Vec<TopDeclaration<ResolvedReference>>> {
    let mappings: Mappings = decls
        .iter()
        .enumerate()
        .map(|(idx, d)| (d.get_name().to_owned(), idx))
        .collect();

    // let mut f: Vec<TopDeclaration<ResolvedReference>> = vec![];
    decls
        .into_iter()
        .map(|decl| match decl {
            TopDeclaration::Struct(s) => {
                validate_struct(&mappings, s).map(|x| TopDeclaration::Struct(x))
            }
            TopDeclaration::Enum(e) => validate_enum(&mappings, e).map(|x| TopDeclaration::Enum(x)),
            TopDeclaration::Alias(_) => Err(anyhow!("boom")),
        })
        .collect()
}

fn validate_struct(mappings: &Mappings, s: Struct<String>) -> Result<Struct<usize>> {
    let fields = s
        .fields
        .into_iter()
        .map(|f| validate_field(&mappings, f))
        .collect::<Result<Vec<_>>>();

    Ok(Struct {
        location: s.location,
        name: s.name.clone(),
        type_parameters: s.type_parameters.clone(),
        fields: fields?,
    })
}

fn validate_field(mappings: &Mappings, f: Field<String>) -> Result<Field<usize>> {
    Ok(Field {
        location: f.location,
        name: f.name,
        typ: validate_type(&mappings, f.typ)?,
    })
}

fn validate_enum(mappings: &Mappings, e: Enum<String>) -> Result<Enum<usize>> {
    let variants = e
        .variants
        .into_iter()
        .map(|v| validate_variant(&mappings, v))
        .collect::<Result<Vec<_>>>();
    Ok(Enum {
        location: e.location,
        name: e.name,
        type_parameters: e.type_parameters,
        variants: variants?,
    })
}

fn validate_variant(mappings: &Mappings, ev: EnumVariant<String>) -> Result<EnumVariant<usize>> {
    Ok(EnumVariant {
        location: ev.location,
        name: ev.name,
        alias: ev.alias,
        value: validate_variant_value(&mappings, ev.value)?,
    })
}

fn validate_variant_value(
    mappings: &Mappings,
    v: VariantValue<String>,
) -> Result<VariantValue<usize>> {
    match v {
        VariantValue::OnlyCtor => Ok(VariantValue::OnlyCtor),
        VariantValue::PositionalCtor(typs) => typs
            .into_iter()
            .map(|t| validate_type(&mappings, t))
            .collect::<Result<Vec<_>>>()
            .map(|x| VariantValue::PositionalCtor(x)),
        VariantValue::StructCtor(fields) => fields
            .into_iter()
            .map(|f| validate_field(&mappings, f))
            .collect::<Result<Vec<_>>>()
            .map(|x| VariantValue::StructCtor(x)),
    }
}

fn validate_type(mappings: &Mappings, t: Type<String>) -> Result<Type<usize>> {
    match t {
        Type::Atomic(t) => Ok(Type::Atomic(t)),
        Type::Reference(r) => mappings
            .get(&r.name)
            .ok_or(anyhow!("Reference {} not found", r.name).into())
            .and_then(|i| {
                let params = r
                    .type_parameters
                    .into_iter()
                    .map(|t| validate_type(&mappings, t))
                    .collect::<Result<Vec<_>>>();

                Ok(Type::Reference(RefType {
                    location: r.location,
                    name: *i,
                    type_parameters: params?,
                }))
            }),
        Type::Builtin(b) => {
            let b2 = match b {
                Builtin::List(inner) => Builtin::List(Box::new(validate_type(mappings, *inner)?)),
                Builtin::Optional(inner) => {
                    Builtin::Optional(Box::new(validate_type(mappings, *inner)?))
                }
                Builtin::Map(k, v) => Builtin::Map(
                    Box::new(validate_type(mappings, *k)?),
                    Box::new(validate_type(mappings, *v)?),
                ),
            };
            Ok(Type::Builtin(b2))
        }
    }
}
