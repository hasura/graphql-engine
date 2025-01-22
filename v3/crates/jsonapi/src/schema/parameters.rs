use metadata_resolve::{Qualified, QualifiedBaseType, QualifiedTypeName, QualifiedTypeReference};
use open_dds::types::CustomTypeName;

use super::shared::{array_schema, enum_schema, int_schema, pretty_typename, string_schema};
use crate::catalog::{Model, ObjectType, Type};
use crate::schema::shared::json_schema;
use std::collections::BTreeMap;
use std::string::ToString;

pub fn page_offset_parameter() -> oas3::spec::Parameter {
    let schema = oas3::spec::ObjectOrReference::Object(int_schema());
    oas3::spec::Parameter {
        name: "page[offset]".into(),
        allow_empty_value: None,
        allow_reserved: None,
        content: None,
        deprecated: None,
        description: Some("Optional offset for fetched items".into()),
        example: Some("10".into()),
        explode: None,
        examples: BTreeMap::new(),
        extensions: BTreeMap::new(),
        location: oas3::spec::ParameterIn::Query,
        schema: Some(schema),
        style: None,
        required: None,
    }
}

pub fn page_limit_parameter() -> oas3::spec::Parameter {
    let schema = oas3::spec::ObjectOrReference::Object(int_schema());
    oas3::spec::Parameter {
        name: "page[limit]".into(),
        allow_empty_value: None,
        allow_reserved: None,
        content: None,
        deprecated: None,
        description: Some("Optional limit for fetched items".into()),
        example: Some("5".into()),
        explode: None,
        examples: BTreeMap::new(),
        extensions: BTreeMap::new(),
        location: oas3::spec::ParameterIn::Query,
        schema: Some(schema),
        style: None,
        required: None,
    }
}

/// To collect and store nested field types of an object type
struct NestedFieldTypes<'a>(BTreeMap<&'a Qualified<CustomTypeName>, &'a ObjectType>);

impl<'a> NestedFieldTypes<'a> {
    /// Collect all nested field types of an object type
    fn collect(
        object_type: &'a ObjectType,
        object_types: &'a BTreeMap<Qualified<CustomTypeName>, ObjectType>,
    ) -> Self {
        let mut collected = BTreeMap::new();
        Self::collect_from_object_type(object_type, object_types, &mut collected);
        NestedFieldTypes(collected)
    }

    /// Collect nested field types of on object type
    fn collect_from_object_type(
        object_type: &'a ObjectType,
        object_types: &'a BTreeMap<Qualified<CustomTypeName>, ObjectType>,
        collected: &mut BTreeMap<&'a Qualified<CustomTypeName>, &'a ObjectType>,
    ) {
        for field_type in object_type.type_fields.values() {
            Self::collect_from_type(field_type, object_types, collected);
        }
    }

    /// Collect nested object types from a type
    fn collect_from_type(
        ty: &'a Type,
        object_types: &'a BTreeMap<Qualified<CustomTypeName>, ObjectType>,
        collected: &mut BTreeMap<&'a Qualified<CustomTypeName>, &'a ObjectType>,
    ) {
        match ty {
            Type::Scalar(_) | Type::ScalarForDataConnector(_) => {}
            Type::List(inner_type) => {
                Self::collect_from_type(inner_type, object_types, collected);
            }
            Type::Object(type_name) => {
                // Collect only if this type is accessible for the role
                if let Some(object_type) = object_types.get(type_name) {
                    // Ignore if the type has already been collected to prevent infinite recursion caused by cyclic types.
                    if collected.insert(type_name, object_type).is_none() {
                        Self::collect_from_object_type(object_type, object_types, collected);
                    }
                }
            }
        }
    }
}

// Generate "fields[TYPE]" parameter for the given object type and its nested field types.
pub fn fields_parameters(
    type_name: &Qualified<CustomTypeName>,
    object_type: &ObjectType,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectType>,
) -> Vec<oas3::spec::Parameter> {
    // Collect all nested field types of this object type
    let NestedFieldTypes(mut types) = NestedFieldTypes::collect(object_type, object_types);
    // Include this object type in the collection
    types.insert(type_name, object_type);
    // Build "fields[TYPE]" parameter for all collected types
    types
        .into_iter()
        .map(|(type_name, object_type)| build_fields_parameter(type_name, object_type))
        .collect()
}

// Generate "fields[TYPE]" parameter for the given object type
fn build_fields_parameter(
    type_name: &Qualified<CustomTypeName>,
    object_type: &ObjectType,
) -> oas3::spec::Parameter {
    let schema = oas3::spec::ObjectOrReference::Object(oas3::spec::ObjectSchema {
        items: Some(Box::new(oas3::spec::ObjectOrReference::Object(
            enum_schema(
                object_type
                    .type_fields
                    .keys()
                    .map(ToString::to_string)
                    .collect(),
            ),
        ))),
        ..oas3::spec::ObjectSchema::default()
    });

    let mut example = String::new();
    for (i, field) in object_type.type_fields.keys().enumerate() {
        if i > 0 && i < object_type.type_fields.len() {
            example.push(',');
        }
        example.push_str(&field.to_string());
    }

    oas3::spec::Parameter {
        name: format!("fields[{}]", type_name.name),
        allow_empty_value: None,
        allow_reserved: None,
        content: None,
        deprecated: None,
        description: Some(format!("Optional list of fields from {} type to include in response. If no fields are provided, all fields are returned", type_name.name)),
        example: Some(example.into()),
        explode: None,
        examples: BTreeMap::new(),
        extensions: BTreeMap::new(),
        location: oas3::spec::ParameterIn::Query,
        schema: Some(schema),
        style: None,
        required: None,
    }
}

pub fn ordering_parameter(model: &Model, object_type: &ObjectType) -> oas3::spec::Parameter {
    // each field can be `thing` (sort ascending by 'thing') or `-thing` (sort descending by
    // 'thing')
    let mut sort_keys = Vec::new();
    for type_field in object_type.type_fields.keys() {
        sort_keys.push(format!("{type_field}"));
        sort_keys.push(format!("-{type_field}"));
    }

    let schema = oas3::spec::ObjectOrReference::Object(oas3::spec::ObjectSchema {
        items: Some(Box::new(oas3::spec::ObjectOrReference::Object(
            enum_schema(sort_keys.clone()),
        ))),
        ..oas3::spec::ObjectSchema::default()
    });

    // pick out the first few for an example
    let mut example = String::new();
    for (i, field) in sort_keys.iter().enumerate() {
        if i % 3 == 0 && i < 10 {
            if i > 0 && i < sort_keys.len() {
                example.push(',');
            }
            example.push_str(&field.to_string());
        }
    }

    oas3::spec::Parameter {
        name: "sort".into(),
        allow_empty_value: None,
        allow_reserved: None,
        content: None,
        deprecated: None,
        description: Some(format!("Optional list of fields from {} to use in sorting response. 'field' will sort in ascending order, whilst '-field' will sort descending.",model.name.name)),
        example: Some(example.into()),
        explode: None,
        examples: BTreeMap::new(),
        extensions: BTreeMap::new(),
        location: oas3::spec::ParameterIn::Query,
        schema: Some(schema),
        style: None,
        required: None,
    }
}

pub fn include_parameter(model: &Model, object_type: &ObjectType) -> oas3::spec::Parameter {
    let schema = oas3::spec::ObjectOrReference::Object(array_schema(
        oas3::spec::ObjectOrReference::Object(string_schema(None)),
    ));
    let example = object_type
        .type_relationships
        .keys()
        .map(ToString::to_string)
        .collect::<Vec<String>>()
        .join(",");
    let description = format!(
        "Optional list of relationships from {} to include in the response. \
         Use dot-separated names to include nested relationships.",
        model.name.name
    );
    oas3::spec::Parameter {
        name: "include".into(),
        allow_empty_value: None,
        allow_reserved: None,
        content: None,
        deprecated: None,
        description: Some(description),
        example: Some(example.into()),
        explode: None,
        examples: BTreeMap::new(),
        extensions: BTreeMap::new(),
        location: oas3::spec::ParameterIn::Query,
        schema: Some(schema),
        style: None,
        required: None,
    }
}

// Generate "filter" parameter for the given model with a given object type
pub fn filter_parameters(
    model: &Model,
    // We don't need this right away, this will be used once we start supporting nested filters
    _object_type: &ObjectType,
    schemas: &mut BTreeMap<String, oas3::spec::ObjectOrReference<oas3::spec::ObjectSchema>>,
    // We don't need this right away, this will be used once we start supporting nested filters
    _filter_boolean_expression_types: &BTreeMap<
        String,
        metadata_resolve::ResolvedObjectBooleanExpressionType,
    >,
) -> Option<oas3::spec::Parameter> {
    // only include a filter if the model has a `BooleanExpressionType`
    match &model.filter_expression_type {
        Some(boolean_expression_type) => {
            if schemas
                .get(&pretty_typename(&boolean_expression_type.name))
                .is_none()
            {
                // Add basic information about the filter
                let mut filter_schema = oas3::spec::ObjectSchema {
                    title: Some(pretty_typename(&boolean_expression_type.name)),
                    description: Some(format!("Filter expression for {}", model.name.name)),
                    schema_type: Some(oas3::spec::SchemaTypeSet::Single(
                        oas3::spec::SchemaType::Object,
                    )),
                    ..Default::default()
                };

                // Keep track of the keys we add to the schema so we can add the `oneOf`
                let mut filter_schema_keys = Vec::new();

                // Add the filter schema for each scalar field
                for (field_name, field_comparison) in &boolean_expression_type.fields.scalar_fields
                {
                    // Add basic information about the filter field
                    let mut field_schema = oas3::spec::ObjectSchema {
                        title: Some(field_name.to_string()),
                        description: Some(format!(
                            "Filter expression for filtering on {field_name}"
                        )),
                        schema_type: Some(oas3::spec::SchemaTypeSet::Single(
                            oas3::spec::SchemaType::Object,
                        )),
                        ..Default::default()
                    };

                    // Add the operators
                    for (operator_name, operator_type) in &field_comparison.operators {
                        field_schema.properties.insert(
                            format!("${operator_name}"),
                            oas3::spec::ObjectOrReference::Object(type_schema(operator_type)),
                        );
                    }
                    filter_schema.properties.insert(
                        field_name.to_string(),
                        oas3::spec::ObjectOrReference::Object(field_schema),
                    );
                    filter_schema_keys.push(field_name.to_string());
                }

                // Add the filter schema for each object field
                for field_name in boolean_expression_type.fields.object_fields.keys() {
                    filter_schema.properties.insert(
                        field_name.to_string(),
                        // TODO: Get the valid filter schema for the object type
                        oas3::spec::ObjectOrReference::Object(json_schema()),
                    );
                }

                // UNCOMMENT this once we start supporting nested filters. This will add the filter schema for each
                // relationship field
                //
                // for (field_name, field_comparison) in
                //     &boolean_expression_type.fields.relationship_fields
                // {
                //     // only include the filter if the object type has the relationship
                //     if object_type
                //         .type_relationships
                //         .contains_key(&field_comparison.relationship_name)
                //     {
                //         if let Some(filter_boolean_expression_type) =
                //             filter_boolean_expression_types
                //                 .get(&pretty_typename(&field_comparison.boolean_expression_type))
                //         {
                //             filter_schema.properties.insert(
                //                 field_name.to_string(),
                //                 oas3::spec::ObjectOrReference::Ref {
                //                     ref_path: format!(
                //                         "#/components/schemas/{}",
                //                         pretty_typename(&filter_boolean_expression_type.name)
                //                     ),
                //                 },
                //             );
                //         }
                //     }
                // }

                // Add the filter schema for $and
                filter_schema.properties.insert(
                    "$and".into(),
                    oas3::spec::ObjectOrReference::Object(array_schema(
                        oas3::spec::ObjectOrReference::Ref {
                            ref_path: format!(
                                "#/components/schemas/{}",
                                pretty_typename(&boolean_expression_type.name)
                            ),
                        },
                    )),
                );
                filter_schema_keys.push("$and".into());

                // Add the filter schema for $or
                filter_schema.properties.insert(
                    "$or".into(),
                    oas3::spec::ObjectOrReference::Object(array_schema(
                        oas3::spec::ObjectOrReference::Ref {
                            ref_path: format!(
                                "#/components/schemas/{}",
                                pretty_typename(&boolean_expression_type.name)
                            ),
                        },
                    )),
                );
                filter_schema_keys.push("$or".into());

                // Add the oneOf
                filter_schema.one_of = filter_schema_keys
                    .into_iter()
                    .map(|key| {
                        oas3::spec::ObjectOrReference::Object(oas3::spec::ObjectSchema {
                            required: vec![key],
                            ..oas3::spec::ObjectSchema::default()
                        })
                    })
                    .collect();
                schemas.insert(
                    pretty_typename(&boolean_expression_type.name),
                    oas3::spec::ObjectOrReference::Object(filter_schema.clone()),
                );
            };

            // Note: We are using the content field here because the filter is a JSON object. We cannot use schema here.
            let mut content = BTreeMap::new();
            content.insert(
                // This is how we tell OpenAPI that this is a JSON media object
                "application/json".into(),
                oas3::spec::MediaType {
                    encoding: BTreeMap::new(),
                    examples: None,
                    // This is the schema for the filter
                    schema: Some(oas3::spec::ObjectOrReference::Ref {
                        ref_path: format!(
                            "#/components/schemas/{}",
                            pretty_typename(&boolean_expression_type.name)
                        ),
                    }),
                },
            );
            Some(oas3::spec::Parameter {
                name: "filter".into(),
                allow_empty_value: None,
                allow_reserved: None,
                content: Some(content),
                deprecated: None,
                description: Some(format!("Filter expression for {}", model.name.name)),
                example: None,
                explode: None,
                // TODO: add examples
                examples: BTreeMap::new(),
                extensions: BTreeMap::new(),
                location: oas3::spec::ParameterIn::Query,
                schema: None,
                style: None,
                required: None,
            })
        }
        None => None,
    }
}

// Generate schema for the given type
fn type_schema(ty: &QualifiedTypeReference) -> oas3::spec::ObjectSchema {
    let mut schema = oas3::spec::ObjectSchema::default();
    match &ty.underlying_type {
        QualifiedBaseType::List(type_reference) => {
            schema.items = Some(Box::new(oas3::spec::ObjectOrReference::Object(
                type_schema(type_reference),
            )));
            schema.schema_type = Some(oas3::spec::SchemaTypeSet::Single(
                oas3::spec::SchemaType::Array,
            ));
        }
        QualifiedBaseType::Named(type_name) => match type_name {
            QualifiedTypeName::Inbuilt(inbuilt_type) => match inbuilt_type {
                open_dds::types::InbuiltType::ID | open_dds::types::InbuiltType::String => {
                    schema.schema_type = Some(oas3::spec::SchemaTypeSet::Single(
                        oas3::spec::SchemaType::String,
                    ));
                }
                open_dds::types::InbuiltType::Int => {
                    schema.schema_type = Some(oas3::spec::SchemaTypeSet::Single(
                        oas3::spec::SchemaType::Integer,
                    ));
                }
                open_dds::types::InbuiltType::Float => {
                    schema.schema_type = Some(oas3::spec::SchemaTypeSet::Single(
                        oas3::spec::SchemaType::Number,
                    ));
                }
                open_dds::types::InbuiltType::Boolean => {
                    schema.schema_type = Some(oas3::spec::SchemaTypeSet::Single(
                        oas3::spec::SchemaType::Boolean,
                    ));
                }
            },
            QualifiedTypeName::Custom(_custom_type_name) => {
                schema.schema_type = None;
            }
        },
    }
    schema
}
