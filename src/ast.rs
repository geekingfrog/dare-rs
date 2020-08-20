use std::fmt::Debug;
use std::vec::Vec;

#[derive(Debug, PartialEq, Eq)]
pub enum TopDeclaration {
    Struct(Struct),
    Enum(Enum),
    // TODO: type alias
    Alias(Alias),
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
    pub typ: Type,
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
}

/// type MaybeInt = Maybe<Int>
#[derive(Debug, PartialEq, Eq)]
pub struct Alias {
    pub location: SrcSpan,
    pub name: String,
    pub alias: RefType,
}

#[derive(Debug, PartialEq, Eq)]
pub struct EnumVariant {
    pub location: SrcSpan,
    pub name: String,
    pub value: VariantValue,
}

#[derive(Debug, PartialEq, Eq)]
pub enum VariantValue {
    OnlyCtor,
    PositionalCtor(Vec<Type>),
    StructCtor(Vec<Field>),
}

/// A type can be atomic (String, Bool, Int…), a reference to another type
/// (Foo, Bar<T>, Map<String, Int>), or a generic type like `T` or `errorType`.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    /// One of the basic atomic type
    Atomic(AtomicType),
    /// Generic type or reference to existing type
    /// like Barf<Foo, T, Bar<Int>>
    Reference(RefType),
    /// Anonymous structure like List<{foo: Int}>
    Anonymous(Vec<Field>),
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Builtin {
    List(Box<Type>),
    Optional(Box<Type>),
    Map(Box<Type>, Box<Type>),
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
