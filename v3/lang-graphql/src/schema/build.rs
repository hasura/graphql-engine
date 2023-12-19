use super::*;

use thiserror::Error;

use crate::ast::common as ast;
use crate::ast::schema as sdl;
use crate::ast::spanning::Positioned;
use crate::parser;

#[derive(Error, Debug, Clone)]
pub enum Error {
    #[error("internal error when parsing introspection schema : {0:}")]
    InternalParseError(Positioned<parser::Error>),

    #[error("internal error when building schema: {0:}")]
    Internal(String),

    #[error("multiple definitions of graphql type: {0:}")]
    ConflictingGraphQlType(ast::TypeName),
}

pub type Result<T> = core::result::Result<T, Error>;

pub fn build_schema<S>(s: &S) -> std::result::Result<Schema<S>, S::SchemaError>
where
    S: SchemaContext,
{
    let introspection_schema = include_str!("introspection.graphql");
    let introspection_document = parser::Parser::new(introspection_schema)
        .parse_schema_document()
        .map_err(Error::InternalParseError)?;
    let mut types = BTreeMap::new();
    let mut introspection_root_fields = HashMap::new();
    let mut builder = Builder {
        registered_types: HashSet::new(),
        registered_namespaces: HashSet::new(),
    };
    for definition in introspection_document.definitions {
        match definition.item {
            sdl::TypeSystemDefinition::Type(type_definition) => {
                let normalized_definition = convert_type_definition(
                    &mut builder,
                    |_, t| RegisteredTypeName(t),
                    &type_definition,
                )?;
                if normalized_definition.name().as_str() == "Query" {
                    if let TypeInfo::Object(query_root) = normalized_definition {
                        introspection_root_fields = query_root.fields;
                    }
                } else {
                    types.insert(
                        normalized_definition.type_name().clone(),
                        normalized_definition,
                    );
                }
            }
            sdl::TypeSystemDefinition::Schema(_) => {}
            sdl::TypeSystemDefinition::Directive(_) => {}
        }
    }

    let mut generated_type_ids: HashSet<S::TypeId> = HashSet::new();
    let schema_entry_point = s.get_schema_entry_point();

    // register all the root types
    let query_root_name = builder.register_type(schema_entry_point.query);
    let mutation_root_name = schema_entry_point
        .mutation
        .map(|type_id| builder.register_type(type_id));
    let subscription_root_name = schema_entry_point
        .subscription
        .map(|type_id| builder.register_type(type_id));

    while !builder.registered_types.is_empty() {
        let types_to_be_generated = builder
            .registered_types
            .drain()
            .filter(|type_id| !generated_type_ids.contains(type_id))
            .collect::<Vec<_>>();
        for type_id in types_to_be_generated {
            let type_definition = s.build_type_info(&mut builder, &type_id)?;
            if let Some(old_type_definition) =
                types.insert(type_definition.type_name().clone(), type_definition)
            {
                return Err(
                    Error::ConflictingGraphQlType(old_type_definition.type_name().clone()).into(),
                );
            }
            generated_type_ids.insert(type_id.clone());
        }
    }

    // add introspection root fields
    match types.get_mut(&query_root_name.0) {
        Some(TypeInfo::Object(object)) => object.fields.extend(introspection_root_fields),
        _ => {
            return Err(Error::Internal(
                "failed to add introspection root fields".to_string(),
            ))?
        }
    }

    Ok(Schema {
        types,
        query_type: query_root_name.0,
        mutation_type: mutation_root_name.map(|v| v.0),
        subscription_type: subscription_root_name.map(|v| v.0),
        namespaces: builder.registered_namespaces,
    })
}

pub fn convert_type_definition<S, F>(
    builder: &mut Builder<S>,
    register_type_name: F,
    definition: &sdl::TypeDefinition,
) -> Result<TypeInfo<S>>
where
    S: SchemaContext,
    F: FnMut(&mut Builder<S>, ast::TypeName) -> RegisteredTypeName,
{
    match definition {
        sdl::TypeDefinition::Scalar(definition) => {
            convert_scalar_type_definition(definition).map(TypeInfo::Scalar)
        }
        sdl::TypeDefinition::Object(definition) => {
            convert_object_type_definition(builder, register_type_name, definition)
                .map(TypeInfo::Object)
        }
        sdl::TypeDefinition::Interface(definition) => {
            convert_interface_type_definition(builder, register_type_name, definition)
                .map(TypeInfo::Interface)
        }
        sdl::TypeDefinition::Union(definition) => {
            convert_union_type_definition(builder, register_type_name, definition)
                .map(TypeInfo::Union)
        }
        sdl::TypeDefinition::Enum(definition) => {
            convert_enum_type_definition(builder, definition).map(TypeInfo::Enum)
        }
        sdl::TypeDefinition::InputObject(definition) => {
            convert_input_object_type_definition(builder, register_type_name, definition)
                .map(TypeInfo::InputObject)
        }
    }
}

fn convert_scalar_type_definition(definition: &sdl::ScalarTypeDefinition) -> Result<Scalar> {
    Ok(Scalar {
        name: ast::TypeName(definition.name.item.clone()),
        description: definition
            .description
            .as_ref()
            .map(|description| description.item.clone()),
    })
}

fn convert_enum_type_definition<S: SchemaContext>(
    builder: &mut Builder<S>,
    definition: &sdl::EnumTypeDefinition,
) -> Result<Enum<S>> {
    let mut values = HashMap::new();
    for enum_value_definition in &definition.values {
        let enum_value_definition = &enum_value_definition.item;
        let enum_value = &enum_value_definition.value.item;
        let normalized_enum_value = EnumValue {
            value: enum_value_definition.value.item.clone(),
            description: enum_value_definition
                .description
                .as_ref()
                .map(|d| d.item.clone()),
            deprecation_status: DeprecationStatus::default(),
            info: S::introspection_node(),
        };
        // TODO: throw error
        if values
            .insert(
                enum_value.clone(),
                builder
                    .allow_all_namespaced(normalized_enum_value, S::introspection_namespace_node()),
            )
            .is_some()
        {}
    }
    Ok(Enum {
        name: ast::TypeName(definition.name.item.clone()),
        description: definition
            .description
            .as_ref()
            .map(|description| description.item.clone()),
        values,
    })
}

fn convert_field_definition<S, F>(
    builder: &mut Builder<S>,
    mut register_type_name: F,
    definition: &sdl::FieldDefinition,
) -> Result<Field<S>>
where
    S: SchemaContext,
    F: FnMut(&mut Builder<S>, ast::TypeName) -> RegisteredTypeName,
{
    let mut arguments = HashMap::new();
    for field_definition in &definition.arguments {
        let argument_name = &field_definition.item.name.item;
        let normalized_argument_definition = convert_input_value_definition(
            builder,
            &mut register_type_name,
            &field_definition.item,
        )?;
        if arguments
            .insert(
                argument_name.clone(),
                builder.allow_all_namespaced(
                    normalized_argument_definition,
                    S::introspection_namespace_node(),
                ),
            )
            .is_some()
        {
            // TODO, throw an error
        }
    }

    Ok(Field::new(
        definition.name.item.clone(),
        definition
            .description
            .as_ref()
            .map(|description| description.item.clone()),
        S::introspection_node(),
        definition
            .ty
            .item
            .clone()
            .map(|t| register_type_name(builder, t)),
        arguments,
        DeprecationStatus::default(),
    ))
}

fn convert_object_type_definition<S, F>(
    builder: &mut Builder<S>,
    mut register_type_name: F,
    definition: &sdl::ObjectTypeDefinition,
) -> Result<Object<S>>
where
    S: SchemaContext,
    F: FnMut(&mut Builder<S>, ast::TypeName) -> RegisteredTypeName,
{
    let mut fields = HashMap::new();
    for field_definition in &definition.fields {
        let field_name = &field_definition.item.name.item;
        let normalized_field_definition =
            convert_field_definition(builder, &mut register_type_name, &field_definition.item)?;
        if fields
            .insert(
                field_name.clone(),
                builder.allow_all_namespaced(
                    normalized_field_definition,
                    S::introspection_namespace_node(),
                ),
            )
            .is_some()
        {
            // TODO, throw an error
        }
    }

    let mut implements = HashMap::new();
    for interface in &definition.implements {
        if implements
            .insert(
                register_type_name(builder, ast::TypeName(interface.item.clone())),
                builder.allow_all_namespaced((), S::introspection_namespace_node()),
            )
            .is_some()
        {
            // TODO throw an error
        }
    }
    Ok(Object::new(
        builder,
        ast::TypeName(definition.name.item.clone()),
        definition
            .description
            .as_ref()
            .map(|description| description.item.clone()),
        fields,
        implements,
    ))
}

fn convert_interface_type_definition<S, F>(
    builder: &mut Builder<S>,
    mut register_type_name: F,
    definition: &sdl::InterfaceTypeDefinition,
) -> Result<Interface<S>>
where
    S: SchemaContext,
    F: FnMut(&mut Builder<S>, ast::TypeName) -> RegisteredTypeName,
{
    let mut fields = HashMap::new();
    for field_definition in &definition.fields {
        let field_name = &field_definition.item.name.item;
        let normalized_field_definition =
            convert_field_definition(builder, &mut register_type_name, &field_definition.item)?;
        if fields
            .insert(
                field_name.clone(),
                builder.allow_all_namespaced(
                    normalized_field_definition,
                    S::introspection_namespace_node(),
                ),
            )
            .is_some()
        {
            // TODO, throw an error
        }
    }
    let mut implements = HashMap::new();

    for interface in &definition.implements {
        if implements
            .insert(
                register_type_name(builder, ast::TypeName(interface.item.clone())),
                builder.allow_all_namespaced((), S::introspection_namespace_node()),
            )
            .is_some()
        {
            // TODO throw an error
        }
    }
    let implemented_by = HashMap::new();
    Ok(Interface::new(
        builder,
        ast::TypeName(definition.name.item.clone()),
        definition
            .description
            .as_ref()
            .map(|description| description.item.clone()),
        fields,
        implements,
        implemented_by,
    ))
}

fn convert_union_type_definition<S, F>(
    builder: &mut Builder<S>,
    mut register_type_name: F,
    definition: &sdl::UnionTypeDefinition,
) -> Result<Union<S>>
where
    S: SchemaContext,
    F: FnMut(&mut Builder<S>, ast::TypeName) -> RegisteredTypeName,
{
    let mut members = HashMap::new();
    for member in &definition.members {
        if members
            .insert(
                register_type_name(builder, ast::TypeName(member.item.clone())),
                builder.allow_all_namespaced((), S::introspection_namespace_node()),
            )
            .is_some()
        {
            // TODO throw an error
        }
    }
    Ok(Union::new(
        builder,
        ast::TypeName(definition.name.item.clone()),
        definition
            .description
            .as_ref()
            .map(|description| description.item.clone()),
        members,
    ))
}

fn convert_input_value_definition<S, F>(
    builder: &mut Builder<S>,
    mut register_type_name: F,
    definition: &sdl::InputValueDefinition,
) -> Result<InputField<S>>
where
    S: SchemaContext,
    F: FnMut(&mut Builder<S>, ast::TypeName) -> RegisteredTypeName,
{
    Ok(InputField::new(
        definition.name.item.clone(),
        definition
            .description
            .as_ref()
            .map(|description| description.item.clone()),
        S::introspection_node(),
        definition
            .ty
            .item
            .clone()
            .map(|t| register_type_name(builder, t)),
        definition
            .default_value
            .as_ref()
            .map(|default_value| default_value.item.clone()),
        DeprecationStatus::default(),
    ))
}

fn convert_input_object_type_definition<S, F>(
    builder: &mut Builder<S>,
    mut register_type_name: F,
    definition: &sdl::InputObjectTypeDefinition,
) -> Result<InputObject<S>>
where
    S: SchemaContext,
    F: FnMut(&mut Builder<S>, ast::TypeName) -> RegisteredTypeName,
{
    let mut fields = HashMap::new();
    for field_definition in &definition.fields {
        let field_name = &field_definition.item.name.item;
        let normalized_field_definition = convert_input_value_definition(
            builder,
            &mut register_type_name,
            &field_definition.item,
        )?;
        if fields
            .insert(
                field_name.clone(),
                builder.allow_all_namespaced(
                    normalized_field_definition,
                    S::introspection_namespace_node(),
                ),
            )
            .is_some()
        {
            // TODO, throw an error
        }
    }
    Ok(InputObject::new(
        ast::TypeName(definition.name.item.clone()),
        definition
            .description
            .as_ref()
            .map(|description| description.item.clone()),
        fields,
    ))
}
