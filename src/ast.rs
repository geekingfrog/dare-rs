use std::fmt::Debug;
use std::vec::Vec;

#[derive(Debug, PartialEq, Eq)]
pub enum TopDeclaration<Reference> {
    Struct(Struct<Reference>),
    Enum(Enum<Reference>),
    // TODO: type alias
    Alias(Alias<Reference>),
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
                _ => return false
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

// TODO
pub type ResolvedReference = String;

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

#[derive(Debug, PartialEq, Eq, Default, Clone)]
pub struct SrcSpan {
    pub start: usize,
    pub end: usize,
}

pub fn location(start: usize, end: usize) -> SrcSpan {
    SrcSpan { start, end }
}

/*
 * struct Foo<A,B> {
 *   foo_a: A,
 *   foo_b: B,
 * };
 *
 * struct Big<A,B,C,D> {
 *   big1: Int,
 *   big2: Option<A>,
 *   big3: Foo<B,C>,
 *   big4: Foo<String, D>,
 *   big5: Foo<Foo<Bool, Int>, String>,
 *   big6: List<{nested1: Int, nested2: A}>
 *
 * }
 *
 * type Test = Big<Bool, Int, Int, String>
 *
 * don't allow type synonyms with type parameter
 * type Incomplete<T> = Big<Bool, T, T, T>
 */
