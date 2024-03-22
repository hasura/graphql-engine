use super::common::*;
use super::spanning::*;
use super::value::*;

/// A GraphQL file or request string defining a GraphQL service.
///
/// [Reference](https://spec.graphql.org/October2021/#Document).
#[derive(Debug, Clone, PartialEq)]
pub struct SchemaDocument {
    /// The definitions of this document.
    pub definitions: Vec<Spanning<TypeSystemDefinition>>,
}

/// A definition concerning the type system of a GraphQL service.
///
/// [Reference](https://spec.graphql.org/October2021/#TypeSystemDefinition). This enum also covers
/// [extensions](https://spec.graphql.org/October2021/#TypeSystemExtension).
#[derive(Debug, Clone, PartialEq)]
pub enum TypeSystemDefinition {
    /// The definition of the schema of the service.
    Schema(SchemaDefinition),
    /// The definition of a type in the service.
    Type(TypeDefinition),
    /// The definition of a directive in the service.
    Directive(DirectiveDefinition),
}

/// A GraphQL directive, such as `@deprecated(reason: "Use the other field")`.
///
/// [Reference](https://spec.graphql.org/June2018/#Directive).
#[derive(Debug, Clone, PartialEq)]
pub struct ConstDirective {
    /// The name of the directive.
    pub name: Spanning<Name>,
    /// The arguments to the directive.
    pub arguments: Option<Spanning<Vec<ConstArgument>>>,
}

/// A const argument
pub type ConstArgument = Spanning<KeyValue<ConstValue>>;

/// The definition of the schema in a GraphQL service.
///
/// [Reference](https://spec.graphql.org/October2021/#SchemaDefinition). This also covers
/// [extensions](https://spec.graphql.org/October2021/#SchemaExtension).
#[derive(Debug, Clone, PartialEq)]
pub struct SchemaDefinition {
    /// Whether the schema is an extension of another schema.
    pub extend: bool,
    /// The description of the schema, if present. This is never present on an
    /// extension schema.
    pub description: Option<Spanning<String>>,
    /// The directives of the schema definition.
    pub directives: Vec<Spanning<ConstDirective>>,

    pub operation_types: Vec<Spanning<(Spanning<OperationType>, Spanning<Name>)>>,
}

/// The definition of a type in a GraphQL service.
///
/// [Reference](https://spec.graphql.org/October2021/#TypeDefinition). This also covers
/// [extensions](https://spec.graphql.org/October2021/#TypeExtension).
#[derive(Debug, Clone, PartialEq)]
pub enum TypeDefinition {
    /// A scalar type.
    Scalar(ScalarTypeDefinition),
    /// An enum type.
    Enum(EnumTypeDefinition),
    /// An object type.
    Object(ObjectTypeDefinition),
    /// An interface type.
    Interface(InterfaceTypeDefinition),
    /// A union type.
    Union(UnionTypeDefinition),
    /// An input object type.
    InputObject(InputObjectTypeDefinition),
}

impl TypeDefinition {
    pub fn name(&self) -> &Spanning<Name> {
        match self {
            TypeDefinition::Scalar(definition) => &definition.name,
            TypeDefinition::Enum(definition) => &definition.name,
            TypeDefinition::Object(definition) => &definition.name,
            TypeDefinition::Interface(definition) => &definition.name,
            TypeDefinition::Union(definition) => &definition.name,
            TypeDefinition::InputObject(definition) => &definition.name,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ScalarTypeDefinition {
    /// Whether the type is an extension of another type.
    pub extend: bool,
    /// The description of the type, if present. This is never present on an
    /// extension type.
    pub description: Option<Spanning<String>>,
    /// The name of the type.
    pub name: Spanning<Name>,
    /// The directives of type definition.
    pub directives: Vec<Spanning<ConstDirective>>,
}

/// The definition of a value inside an enum.
///
/// [Reference](https://spec.graphql.org/October2021/#EnumValueDefinition).
#[derive(Debug, Clone, PartialEq)]
pub struct EnumValueDefinition {
    /// The description of the argument.
    pub description: Option<Spanning<String>>,
    /// The value name.
    pub value: Spanning<Name>,
    /// The directives of the enum value.
    pub directives: Vec<Spanning<ConstDirective>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumTypeDefinition {
    /// Whether the type is an extension of another type.
    pub extend: bool,
    /// The description of the type, if present. This is never present on an
    /// extension type.
    pub description: Option<Spanning<String>>,
    /// The name of the type.
    pub name: Spanning<Name>,
    /// The directives of type definition.
    pub directives: Vec<Spanning<ConstDirective>>,
    /// The possible values of the enum.
    pub values: Vec<Spanning<EnumValueDefinition>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectTypeDefinition {
    /// Whether the type is an extension of another type.
    pub extend: bool,
    /// The description of the type, if present. This is never present on an
    /// extension type.
    pub description: Option<Spanning<String>>,
    /// The name of the type.
    pub name: Spanning<Name>,
    /// The directives of type definition.
    pub directives: Vec<Spanning<ConstDirective>>,
    /// The interfaces implemented by the object.
    pub implements: Vec<Spanning<Name>>,
    /// The fields of the object type.
    pub fields: Vec<Spanning<FieldDefinition>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceTypeDefinition {
    /// Whether the type is an extension of another type.
    pub extend: bool,
    /// The description of the type, if present. This is never present on an
    /// extension type.
    pub description: Option<Spanning<String>>,
    /// The name of the type.
    pub name: Spanning<Name>,
    /// The directives of type definition.
    pub directives: Vec<Spanning<ConstDirective>>,
    /// The interfaces implemented by the object.
    pub implements: Vec<Spanning<Name>>,
    /// The fields of the object type.
    pub fields: Vec<Spanning<FieldDefinition>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnionTypeDefinition {
    /// Whether the type is an extension of another type.
    pub extend: bool,
    /// The description of the type, if present. This is never present on an
    /// extension type.
    pub description: Option<Spanning<String>>,
    /// The name of the type.
    pub name: Spanning<Name>,
    /// The directives of type definition.
    pub directives: Vec<Spanning<ConstDirective>>,
    /// The interfaces implemented by the object.
    pub members: Vec<Spanning<Name>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InputObjectTypeDefinition {
    /// Whether the type is an extension of another type.
    pub extend: bool,
    /// The description of the type, if present. This is never present on an
    /// extension type.
    pub description: Option<Spanning<String>>,
    /// The name of the type.
    pub name: Spanning<Name>,
    /// The directives of type definition.
    pub directives: Vec<Spanning<ConstDirective>>,
    /// The fields of the input object.
    pub fields: Vec<Spanning<InputValueDefinition>>,
}

/// The definition of a field inside an object or interface.
///
/// [Reference](https://spec.graphql.org/October2021/#FieldDefinition).
#[derive(Debug, Clone, PartialEq)]
pub struct FieldDefinition {
    /// The description of the field.
    pub description: Option<Spanning<String>>,
    /// The name of the field.
    pub name: Spanning<Name>,
    /// The arguments of the field.
    pub arguments: Vec<Spanning<InputValueDefinition>>,
    /// The type of the field.
    pub ty: Spanning<Type>,
    /// The directives of the field.
    pub directives: Vec<Spanning<ConstDirective>>,
}

/// The definition of an input value inside the arguments of a field.
///
/// [Reference](https://spec.graphql.org/October2021/#InputValueDefinition).
#[derive(Debug, Clone, PartialEq)]
pub struct InputValueDefinition {
    /// The description of the argument.
    pub description: Option<Spanning<String>>,
    /// The name of the argument.
    pub name: Spanning<Name>,
    /// The type of the argument.
    pub ty: Spanning<Type>,
    /// The default value of the argument, if there is one.
    pub default_value: Option<Spanning<ConstValue>>,
    /// The directives of the input value.
    pub directives: Vec<Spanning<ConstDirective>>,
}

/// The definition of a directive in a service.
///
/// [Reference](https://spec.graphql.org/October2021/#DirectiveDefinition).
#[derive(Debug, Clone, PartialEq)]
pub struct DirectiveDefinition {
    /// The description of the directive.
    pub description: Option<Spanning<String>>,
    /// The name of the directive.
    pub name: Spanning<Name>,
    /// The arguments of the directive.
    pub arguments: Vec<Spanning<InputValueDefinition>>,
    /// The locations the directive applies to.
    pub locations: Vec<Spanning<DirectiveLocation>>,
}

/// Where a directive can apply to.
///
/// [Reference](https://spec.graphql.org/October2021/#DirectiveLocation).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DirectiveLocation {
    /// A [query](enum.OperationType.html#variant.Query)
    /// [operation](struct.OperationDefinition.html).
    Query,
    /// A [mutation](enum.OperationType.html#variant.Mutation)
    /// [operation](struct.OperationDefinition.html).
    Mutation,
    /// A [subscription](enum.OperationType.html#variant.Subscription)
    /// [operation](struct.OperationDefinition.html).
    Subscription,
    /// A [field](struct.Field.html).
    Field,
    /// A [fragment definition](struct.FragmentDefinition.html).
    FragmentDefinition,
    /// A [fragment spread](struct.FragmentSpread.html).
    FragmentSpread,
    /// An [inline fragment](struct.InlineFragment.html).
    InlineFragment,
    /// A [schema](struct.Schema.html).
    Schema,
    /// A [scalar](enum.TypeKind.html#variant.Scalar).
    Scalar,
    /// An [object](struct.ObjectType.html).
    Object,
    /// A [field definition](struct.FieldDefinition.html).
    FieldDefinition,
    /// An [input value definition](struct.InputFieldDefinition.html) as the
    /// arguments of a field but not an input object.
    ArgumentDefinition,
    /// An [interface](struct.InterfaceType.html).
    Interface,
    /// A [union](struct.UnionType.html).
    Union,
    /// An [enum](struct.EnumType.html).
    Enum,
    /// A [value on an enum](struct.EnumValueDefinition.html).
    EnumValue,
    /// An [input object](struct.InputObjectType.html).
    InputObject,
    /// An [input value definition](struct.InputValueDefinition.html) on an
    /// input object but not a field.
    InputFieldDefinition,
    /// An [variable definition](struct.VariableDefinition.html).
    VariableDefinition,
}
