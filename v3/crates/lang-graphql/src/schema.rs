use crate::ast::common as ast;
use crate::ast::common::TypeName;
use crate::ast::value as gql;
use crate::mk_name;

use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::collections::{HashMap, HashSet};
use std::fmt::Display;

pub mod build;
pub mod sdl;

// A simple wrapper on top of ast::TypeName so that we can track the construction
// of TypeNames during the schema building phase.
#[derive(Serialize, Deserialize, PartialEq, Debug, Eq, Clone, Hash, PartialOrd, Ord)]
pub struct RegisteredTypeName(pub(super) ast::TypeName);

impl RegisteredTypeName {
    pub fn type_name(&self) -> &ast::TypeName {
        &self.0
    }
}

impl RegisteredTypeName {
    pub fn new(name: ast::Name) -> RegisteredTypeName {
        RegisteredTypeName(TypeName(name))
    }
    pub fn string() -> RegisteredTypeName {
        RegisteredTypeName(TypeName(mk_name!("String")))
    }
    pub fn int() -> RegisteredTypeName {
        RegisteredTypeName(TypeName(mk_name!("Int")))
    }
    pub fn float() -> RegisteredTypeName {
        RegisteredTypeName(TypeName(mk_name!("Float")))
    }
    pub fn boolean() -> RegisteredTypeName {
        RegisteredTypeName(TypeName(mk_name!("Boolean")))
    }
    pub fn id() -> RegisteredTypeName {
        RegisteredTypeName(TypeName(mk_name!("ID")))
    }
}

pub type RegisteredType = ast::TypeContainer<RegisteredTypeName>;

pub struct EntryPoint<S: SchemaContext> {
    pub query: S::TypeId,
    pub mutation: Option<S::TypeId>,
    pub subscription: Option<S::TypeId>,
}

// The requirement of the PartialEQ, Clone, Serialize super traits here seem to be some
// kind of a limitation with Rust's derive macros for types including associated types
pub trait SchemaContext: std::fmt::Debug + Clone + PartialEq + Serialize {
    // TODO: Rename it to Scope/Role
    type Namespace: std::fmt::Debug
        + std::cmp::Eq
        + std::hash::Hash
        + ToString
        + Clone
        + Serialize
        + DeserializeOwned;

    // Normalized AST is annotated with this information. This information isn't copied to the
    // normalized AST but are references to data in 'Schema'. To avoid duplication of data that
    // would be same across all roles, this is split into GenericNodeInfo and NamespacedNodeInfo.
    type GenericNodeInfo: std::cmp::Eq
        + std::fmt::Debug
        + PartialEq
        + Clone
        + Serialize
        + DeserializeOwned;
    type NamespacedNodeInfo: std::cmp::Eq
        + std::fmt::Debug
        + PartialEq
        + Clone
        + Serialize
        + DeserializeOwned;

    // used for __typename fields
    // fn capture_typename(type_name: &ast::TypeName) -> Self::GenericNodeInfo;

    // This is to attach empty information to introspection fields. Maybe a better design could've
    // been to not have these functions and the annotated information's type changes to
    // Option<&GenericNodeInfo>. We can then use None to store empty information about
    // introspection fields.
    fn introspection_node() -> Self::GenericNodeInfo;

    // Types and functions related to schema generation.

    // A TypeId is a unique identifier for each generated type in the schema.
    type TypeId: std::fmt::Debug + std::cmp::Eq + std::hash::Hash + ToString + Clone + Serialize;

    // Translates a schema specific 'TypeId' to a GraphQL TypeName
    fn to_type_name(type_id: &Self::TypeId) -> ast::TypeName;

    type SchemaError: std::fmt::Debug + From<build::Error>;
    // Builds the schema / 'TypeInfo' for the specified TypeId
    fn build_type_info(
        &self,
        builder: &mut Builder<Self>,
        type_id: &Self::TypeId,
    ) -> std::result::Result<TypeInfo<Self>, Self::SchemaError>;

    fn get_schema_entry_point(&self) -> EntryPoint<Self>;

    // type ScalarValue: std::fmt::Debug;
}

// Builder tracks all the references to a type during the construction of any TypeInfo. This
// combined with `RegisteredTypeName` and the `new` constructors on various `TypeInfo` objects,
// offers a low-key solution to safely build a GraphQL schema.
pub struct Builder<S: SchemaContext> {
    // either a schema specified type_id or a built-in type_name
    registered_types: HashSet<S::TypeId>,
    registered_namespaces: HashSet<S::Namespace>,
}

impl<S: SchemaContext> Builder<S> {
    pub fn register_type(&mut self, type_id: S::TypeId) -> RegisteredTypeName {
        let type_name = S::to_type_name(&type_id);
        self.registered_types.insert(type_id);
        RegisteredTypeName(type_name)
    }

    pub fn allow_all_namespaced<C>(&mut self, data: C) -> Namespaced<S, C> {
        Namespaced {
            namespaced: NamespacedData::AllowAll,
            data,
        }
    }

    pub fn conditional_namespaced<C>(
        &mut self,
        data: C,
        map: HashMap<S::Namespace, S::NamespacedNodeInfo>,
    ) -> Namespaced<S, C> {
        self.registered_namespaces.extend(map.keys().cloned());
        Namespaced {
            namespaced: NamespacedData::Conditional(map),
            data,
        }
    }
}

#[derive(Serialize, Debug, PartialEq, Clone)]
pub struct NodeInfo<'s, S: SchemaContext> {
    pub generic: &'s S::GenericNodeInfo,
    pub namespaced: &'s S::NamespacedNodeInfo,
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
pub struct Namespaced<S: SchemaContext, C> {
    pub namespaced: NamespacedData<S>,
    pub data: C,
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
pub enum NamespacedData<S: SchemaContext> {
    AllowAll,
    Conditional(HashMap<S::Namespace, S::NamespacedNodeInfo>),
}

/// A 'NamespacedGetter' is a function that interprets what it means to extract a
/// `NamespacedNodeInfo` from a `Namespaced` value.
pub trait NamespacedGetter<S: SchemaContext> {
    fn get<'s, C>(
        &self,
        namespaced: &'s Namespaced<S, C>,
    ) -> Option<(&'s C, &'s S::NamespacedNodeInfo)>;
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone, Default)]
pub enum DeprecationStatus {
    #[default]
    NotDeprecated,
    Deprecated {
        reason: Option<String>,
    },
}

impl DeprecationStatus {
    pub fn new_deprecated(description: Option<&str>) -> Self {
        DeprecationStatus::Deprecated {
            reason: description.map(ToString::to_string),
        }
    }

    pub fn is_deprecated(&self) -> bool {
        matches!(self, DeprecationStatus::Deprecated { .. })
    }

    pub fn reason(&self) -> Option<&str> {
        match self {
            DeprecationStatus::NotDeprecated => None,
            DeprecationStatus::Deprecated { reason } => reason.as_deref(),
        }
    }
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
pub struct Directive {
    pub name: ast::Name,
    pub arguments: BTreeMap<ast::Name, gql::ConstValue>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
pub struct Object<S: SchemaContext> {
    pub name: ast::TypeName,
    pub description: Option<String>,
    pub fields: BTreeMap<ast::Name, Namespaced<S, Field<S>>>,
    /// The set of interfaces that this object type implements
    pub interfaces: BTreeMap<ast::TypeName, Namespaced<S, ()>>,
    pub directives: Vec<Directive>,
}

fn build_typename_field<S: SchemaContext>(builder: &mut Builder<S>) -> Namespaced<S, Field<S>> {
    builder.allow_all_namespaced(Field {
        name: mk_name!("__typename"),
        description: None,
        info: S::introspection_node(),
        field_type: ast::Type {
            base: ast::BaseType::Named(TypeName(mk_name!("String"))),
            nullable: false,
        },
        arguments: BTreeMap::new(),
        deprecation_status: DeprecationStatus::NotDeprecated,
    })
}

impl<S: SchemaContext> Object<S> {
    pub fn new(
        builder: &mut Builder<S>,
        name: ast::TypeName,
        description: Option<String>,
        fields: BTreeMap<ast::Name, Namespaced<S, Field<S>>>,
        interfaces: BTreeMap<RegisteredTypeName, Namespaced<S, ()>>,
        directives: Vec<Directive>,
    ) -> Self {
        let interfaces = interfaces.into_iter().map(|(k, v)| (k.0, v)).collect();
        let mut definition = Object {
            name,
            description,
            fields,
            interfaces,
            directives,
        };
        let typename_field = build_typename_field(builder);
        definition
            .fields
            .insert(typename_field.data.name.clone(), typename_field);
        definition
    }
}

impl<S: SchemaContext> Object<S> {
    pub fn possible_types(&self) -> HashSet<&ast::TypeName> {
        HashSet::from([&self.name])
    }
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
pub struct Field<S: SchemaContext> {
    pub name: ast::Name,
    pub description: Option<String>,
    pub info: S::GenericNodeInfo,
    pub field_type: ast::Type,
    pub arguments: BTreeMap<ast::Name, Namespaced<S, InputField<S>>>,
    pub deprecation_status: DeprecationStatus,
}

impl<S: SchemaContext> Field<S> {
    pub fn new(
        name: ast::Name,
        description: Option<String>,
        info: S::GenericNodeInfo,
        field_type: RegisteredType,
        arguments: BTreeMap<ast::Name, Namespaced<S, InputField<S>>>,
        deprecation_status: DeprecationStatus,
    ) -> Self {
        Field {
            name,
            description,
            info,
            field_type: field_type.map(|v| v.0),
            arguments,
            deprecation_status,
        }
    }
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
pub struct InputObject<S: SchemaContext> {
    pub name: ast::TypeName,
    pub description: Option<String>,
    pub fields: BTreeMap<ast::Name, Namespaced<S, InputField<S>>>,
    pub directives: Vec<Directive>,
}

impl<S: SchemaContext> InputObject<S> {
    pub fn new(
        name: ast::TypeName,
        description: Option<String>,
        fields: BTreeMap<ast::Name, Namespaced<S, InputField<S>>>,
        directives: Vec<Directive>,
    ) -> Self {
        InputObject {
            name,
            description,
            fields,
            directives,
        }
    }
    // TODO: we'll probably have to pre-compute this if required
    pub fn required_field_count(&self) -> i32 {
        self.fields.values().fold(0, |accum, field| {
            accum + i32::from(!field.data.field_type.nullable)
        })
    }
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
pub struct InputField<S: SchemaContext> {
    pub name: ast::Name,
    pub description: Option<String>,
    pub info: S::GenericNodeInfo,
    pub field_type: ast::Type,
    pub default_value: Option<gql::ConstValue>,
    pub deprecation_status: DeprecationStatus,
}

impl<S: SchemaContext> InputField<S> {
    pub fn new(
        name: ast::Name,
        description: Option<String>,
        info: S::GenericNodeInfo,
        field_type: RegisteredType,
        default_value: Option<gql::ConstValue>,
        deprecation_status: DeprecationStatus,
    ) -> Self {
        InputField {
            name,
            description,
            info,
            field_type: field_type.map(|v| v.0),
            default_value,
            deprecation_status,
        }
    }
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
pub struct Scalar {
    pub name: ast::TypeName,
    pub description: Option<String>,
    pub directives: Vec<Directive>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
pub struct EnumValue<S: SchemaContext> {
    pub value: ast::Name,
    pub description: Option<String>,
    pub deprecation_status: DeprecationStatus,
    pub info: S::GenericNodeInfo,
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
pub struct Enum<S: SchemaContext> {
    pub name: ast::TypeName,
    pub description: Option<String>,
    pub values: BTreeMap<ast::Name, Namespaced<S, EnumValue<S>>>,
    pub directives: Vec<Directive>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
pub struct Union<S: SchemaContext> {
    pub name: ast::TypeName,
    pub description: Option<String>,
    fields: BTreeMap<ast::Name, Namespaced<S, Field<S>>>,
    pub members: BTreeMap<ast::TypeName, Namespaced<S, ()>>,
    pub directives: Vec<Directive>,
}

impl<S: SchemaContext> Union<S> {
    pub fn new(
        builder: &mut Builder<S>,
        name: ast::TypeName,
        description: Option<String>,
        members: BTreeMap<RegisteredTypeName, Namespaced<S, ()>>,
        directives: Vec<Directive>,
    ) -> Self {
        let typename_field = build_typename_field(builder);
        Union {
            name,
            description,
            fields: BTreeMap::from_iter([(typename_field.data.name.clone(), typename_field)]),
            members: members.into_iter().map(|(k, v)| (k.0, v)).collect(),
            directives,
        }
    }

    pub fn possible_types(&self) -> HashSet<&ast::TypeName> {
        // Note Clone of Name is constant
        self.members.keys().collect()
    }

    pub fn get_fields(&self) -> &BTreeMap<ast::Name, Namespaced<S, Field<S>>> {
        &self.fields
    }
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
pub struct Interface<S: SchemaContext> {
    pub name: ast::TypeName,
    pub description: Option<String>,
    pub fields: BTreeMap<ast::Name, Namespaced<S, Field<S>>>,
    pub interfaces: BTreeMap<ast::TypeName, Namespaced<S, ()>>,
    pub implemented_by: BTreeMap<ast::TypeName, Namespaced<S, ()>>,
    pub directives: Vec<Directive>,
}

impl<S: SchemaContext> Interface<S> {
    pub fn new(
        builder: &mut Builder<S>,
        name: ast::TypeName,
        description: Option<String>,
        fields: BTreeMap<ast::Name, Namespaced<S, Field<S>>>,
        interfaces: BTreeMap<RegisteredTypeName, Namespaced<S, ()>>,
        implemented_by: BTreeMap<RegisteredTypeName, Namespaced<S, ()>>,
        directives: Vec<Directive>,
    ) -> Self {
        let mut definition = Interface {
            name,
            description,
            fields,
            interfaces: interfaces.into_iter().map(|(k, v)| (k.0, v)).collect(),
            implemented_by: implemented_by.into_iter().map(|(k, v)| (k.0, v)).collect(),
            directives,
        };
        let typename_field = build_typename_field(builder);
        definition
            .fields
            .insert(typename_field.data.name.clone(), typename_field);
        definition
    }

    pub fn possible_types(&self) -> HashSet<&ast::TypeName> {
        self.implemented_by.keys().collect()
    }
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
pub enum TypeInfo<S: SchemaContext> {
    Scalar(Scalar),
    Enum(Enum<S>),
    Object(Object<S>),
    Interface(Interface<S>),
    Union(Union<S>),
    InputObject(InputObject<S>),
}

impl<S: SchemaContext> TypeInfo<S> {
    pub fn kind(&self) -> &'static str {
        match self {
            TypeInfo::Scalar(_) => "SCALAR",
            TypeInfo::Enum(_) => "ENUM",
            TypeInfo::Interface(_) => "INTERFACE",
            TypeInfo::Object(_) => "OBJECT",
            TypeInfo::Union(_) => "UNION",
            TypeInfo::InputObject(_) => "INPUT_OBJECT",
        }
    }
    pub fn as_input_type(&self) -> Option<InputType<'_, S>> {
        match self {
            TypeInfo::Scalar(info) => Some(InputType::Scalar(info)),
            TypeInfo::Enum(info) => Some(InputType::Enum(info)),
            TypeInfo::InputObject(info) => Some(InputType::InputObject(info)),
            TypeInfo::Interface(_) | TypeInfo::Object(_) | TypeInfo::Union(_) => None,
        }
    }

    fn type_name(&self) -> &ast::TypeName {
        match self {
            TypeInfo::Scalar(d) => &d.name,
            TypeInfo::Enum(d) => &d.name,
            TypeInfo::Object(d) => &d.name,
            TypeInfo::Interface(d) => &d.name,
            TypeInfo::Union(d) => &d.name,
            TypeInfo::InputObject(d) => &d.name,
        }
    }
}

#[derive(Serialize, Debug, Clone, Copy)]
pub enum InputType<'s, S: SchemaContext> {
    Scalar(&'s Scalar),
    Enum(&'s Enum<S>),
    InputObject(&'s InputObject<S>),
}

impl<S: SchemaContext> TypeInfo<S> {
    pub fn name(&self) -> &ast::TypeName {
        match self {
            TypeInfo::Scalar(scalar) => &scalar.name,
            TypeInfo::Enum(e) => &e.name,
            TypeInfo::Interface(interface) => &interface.name,
            TypeInfo::Object(object) => &object.name,
            TypeInfo::Union(union) => &union.name,
            TypeInfo::InputObject(input_object) => &input_object.name,
        }
    }
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
pub struct Schema<S: SchemaContext> {
    pub types: BTreeMap<ast::TypeName, TypeInfo<S>>,
    pub query_type: ast::TypeName,
    pub mutation_type: Option<ast::TypeName>,
    pub subscription_type: Option<ast::TypeName>,
    pub namespaces: HashSet<S::Namespace>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub enum SchemaWithVersion<S: SchemaContext> {
    V0(Schema<S>),
}

impl<S: SchemaContext> Schema<S> {
    pub fn get_type(&self, type_name: &ast::TypeName) -> Option<&TypeInfo<S>> {
        self.types.get(type_name)
    }
}

impl<S: SchemaContext> SchemaWithVersion<S> {
    pub fn upgrade(self) -> Schema<S> {
        match self {
            SchemaWithVersion::V0(s) => s,
        }
    }
}
