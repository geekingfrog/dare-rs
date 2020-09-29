use anyhow::Result;
/// AST used for code generation, as opposed to the parsing AST
use std::{collections::BTreeMap, rc::Rc};

use crate::ast;
use crate::lexer::SrcSpan;
use std::fmt::Debug;

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Struct {
    pub location: SrcSpan,
    pub name: String,
    // only generic types are allowed in definition
    pub type_parameters: Vec<String>,
    pub fields: Vec<Field>,
}

/// enum Foo = {A, B, C, D}
/// enum Bar = {String, Int}
/// enum Maybe<T> = {Nothing, Just(T)}
/// enum Detailed<A,B> = {Nothing, Something(A,B)}
/// enum WithFields = {A, B{b_val: Int}}
/// the last example, when monomorphised would give:
/// ["Detailed", "val_A", val_B""]
///  or {"type": "Detailed", "value": ["val_A", "val_B"]}
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Enum {
    pub location: SrcSpan,
    pub name: String,
    // only generic types are allowed in definition
    pub type_parameters: Vec<String>,
    pub variants: Vec<EnumVariant>,
    pub directives: ast::Directives,
}

/// type MaybeInt = Maybe<Int>
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Alias {
    pub location: SrcSpan,
    pub name: String,
    pub alias: RefType,
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
    /// name of the field holding the referenced type
    TypeOf(String),
    Type(Type),
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
    /// Builtin type like List, Map and Optional
    Builtin(Builtin),
}

impl Enum {
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
    pub target: Rc<TopDeclaration>,

    // Populated from a #[typeof(…)] field when an Enum
    // is embedded into a Struct
    // Used for efficient parsing
    pub variant_hint: Option<String>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Builtin {
    List(Box<Type>),
    Optional(Box<Type>),
    Map(Box<Type>, Box<Type>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct EnumVariant {
    pub location: SrcSpan,
    pub name: String,
    pub alias: Option<String>,
    pub value: VariantValue,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum VariantValue {
    OnlyCtor,
    PositionalCtor(Vec<Type>),
    StructCtor(Vec<Field>),
}

impl VariantValue {
    pub fn is_only_ctor(&self) -> bool {
        match &self {
            VariantValue::OnlyCtor => true,
            _ => false,
        }
    }
}

type Mappings<'a> = BTreeMap<String, Rc<TopDeclaration>>;

// used to propagate information about #[typeof] fields
// (field_name, target_name)
// foo: #[typeof(bar)] => (foo, bar)
type TypeMappings<'a> = Vec<(&'a str, &'a str)>;

/// Turn a Vec<ast::TopDeclaration> into a Vec<TopDeclaration> ready to be used
/// for code generation
pub fn from_parse_ast(decls: Vec<ast::TopDeclaration>) -> Result<Vec<TopDeclaration>> {
    let mut mappings = BTreeMap::new();
    decls.iter().map(|d| d.to_gen_ast(&mut mappings)).collect()
}

impl ast::TopDeclaration {
    fn to_gen_ast(&self, mappings: &mut Mappings) -> Result<TopDeclaration> {
        let result = match self {
            ast::TopDeclaration::Struct(s) => s.to_gen_ast(&mappings).map(TopDeclaration::Struct),
            ast::TopDeclaration::Enum(e) => e.to_gen_ast(&mappings).map(TopDeclaration::Enum),
            ast::TopDeclaration::Alias(_) => todo!("alias not supported yet"),
        }?;

        // would be nice to avoid the result.clone()
        mappings.insert(self.get_name().to_string(), Rc::new(result.clone()));
        Ok(result)
    }
}

impl ast::Struct {
    fn to_gen_ast(&self, mappings: &Mappings) -> Result<Struct> {
        let type_mappings: TypeMappings = self.fields.iter().filter_map(|f| match &f.typ {
            ast::FieldType::TypeOf(s) => Some((f.name.as_ref(), s.as_ref())),
            ast::FieldType::Type(_) => None,
        }).collect();

        for (type_of_name, target_field_name) in type_mappings.iter() {
            let found = self.fields.iter().find(|f| &f.name == target_field_name);
            if found.is_none() {
                return Err(anyhow!(
                    "{} references a field named {} but it is not present in the struct",
                    type_of_name,
                    target_field_name
                ));
            }
        }

        let fields = self
            .fields
            .iter()
            .map(|f| f.to_gen_ast(&mappings, &type_mappings))
            .collect::<Result<_>>();

        Ok(Struct {
            location: self.location,
            name: self.name.clone(),
            type_parameters: self.type_parameters.clone(),
            fields: fields?,
        })
    }
}

impl ast::Enum {
    fn to_gen_ast(&self, mappings: &Mappings) -> Result<Enum> {
        let variants = self
            .variants
            .iter()
            .map(|v| v.to_gen_ast(&mappings))
            .collect::<Result<_>>();
        Ok(Enum {
            location: self.location,
            name: self.name.clone(),
            type_parameters: self.type_parameters.clone(),
            variants: variants?,
            directives: self.directives.clone(),
        })
    }
}

impl ast::Field {
    fn to_gen_ast(&self, mappings: &Mappings, type_mappings: &TypeMappings) -> Result<Field> {
        let variant_hint = type_mappings.iter().find(|(mapping_name, _hint)| {
            **mapping_name == self.name
        }).map(|(_, hint)| (*hint).to_string());

        Ok(Field {
            location: self.location,
            name: self.name.clone(),
            typ: self.typ.to_gen_ast(&mappings, variant_hint)?,
        })
    }
}

impl ast::FieldType {
    fn to_gen_ast(&self, mappings: &Mappings, variant_hint: Option<String>) -> Result<FieldType> {
        match self {
            ast::FieldType::TypeOf(s) => Ok(FieldType::TypeOf(s.clone())),
            ast::FieldType::Type(t) => t.to_gen_ast(&mappings, variant_hint).map(FieldType::Type),
        }
    }
}

impl ast::EnumVariant {
    fn to_gen_ast(&self, mappings: &Mappings) -> Result<EnumVariant> {
        Ok(EnumVariant {
            location: self.location,
            name: self.name.clone(),
            alias: self.alias.clone(),
            value: self.value.to_gen_ast(&mappings)?,
        })
    }
}

impl ast::VariantValue {
    fn to_gen_ast(&self, mappings: &Mappings) -> Result<VariantValue> {
        match self {
            ast::VariantValue::OnlyCtor => Ok(VariantValue::OnlyCtor),
            ast::VariantValue::PositionalCtor(ctors) => {
                let result = ctors
                    .iter()
                    .map(|t| t.to_gen_ast(&mappings, None))
                    .collect::<Result<_>>();
                Ok(VariantValue::PositionalCtor(result?))
            }
            ast::VariantValue::StructCtor(_) => todo!("anon struct in enum not supported"),
        }
    }
}

impl ast::Type {
    fn to_gen_ast(&self, mappings: &Mappings, variant_hint: Option<String>) -> Result<Type> {
        match self {
            ast::Type::Atomic(t) => Ok(Type::Atomic(t.into())),
            ast::Type::Reference(r) => mappings
                .get(&r.name[..])
                .ok_or(anyhow!("Reference {} not found", r.name).into())
                .and_then(|target| {
                    let params = r
                        .type_parameters
                        .iter()
                        // no variant hints for type params
                        .map(|t| t.to_gen_ast(&mappings, None))
                        .collect::<Result<_>>();

                    Ok(Type::Reference(RefType {
                        location: r.location,
                        name: r.name.clone(),
                        type_parameters: params?,
                        target: (*target).clone(),
                        variant_hint: None, // TODO
                    }))
                }),
            ast::Type::Builtin(b) => {
                let b2 = match b {
                    ast::Builtin::List(inner) => {
                        Builtin::List(Box::new(inner.to_gen_ast(mappings, None)?))
                    }
                    ast::Builtin::Optional(inner) => {
                        Builtin::Optional(Box::new(inner.to_gen_ast(mappings, None)?))
                    }
                    ast::Builtin::Map(k, v) => Builtin::Map(
                        Box::new(k.to_gen_ast(mappings, None)?),
                        Box::new(v.to_gen_ast(mappings, None)?),
                    ),
                };
                Ok(Type::Builtin(b2))
            }
        }
    }
}

impl From<&ast::AtomicType> for AtomicType {
    fn from(t: &ast::AtomicType) -> Self {
        match t {
            ast::AtomicType::Str => AtomicType::Str,
            ast::AtomicType::UInt => AtomicType::UInt,
            ast::AtomicType::Int => AtomicType::Int,
            ast::AtomicType::Int8 => AtomicType::Int8,
            ast::AtomicType::Int16 => AtomicType::Int16,
            ast::AtomicType::Int32 => AtomicType::Int32,
            ast::AtomicType::Int64 => AtomicType::Int64,
            ast::AtomicType::UInt8 => AtomicType::UInt8,
            ast::AtomicType::UInt16 => AtomicType::UInt16,
            ast::AtomicType::UInt32 => AtomicType::UInt32,
            ast::AtomicType::UInt64 => AtomicType::UInt64,
            ast::AtomicType::Float => AtomicType::Float,
            ast::AtomicType::Bool => AtomicType::Bool,
            ast::AtomicType::Bytes => AtomicType::Bytes,
        }
    }
}
