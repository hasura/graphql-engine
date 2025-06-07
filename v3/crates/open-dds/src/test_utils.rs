use schemars::schema::{InstanceType, Schema, SchemaObject, SingleOrVec, SubschemaValidation};

pub struct JsonSchemaValidationConfig {
    pub schemas_with_arbitrary_additional_properties_allowed: Vec<&'static str>,
}

impl Default for JsonSchemaValidationConfig {
    fn default() -> Self {
        Self::new()
    }
}

impl JsonSchemaValidationConfig {
    pub fn new() -> Self {
        Self {
            schemas_with_arbitrary_additional_properties_allowed: vec![],
        }
    }
}

// Helper functions for the `test_validate_json_schema` test
pub fn validate_root_json_schema(
    root_schema: schemars::schema::RootSchema,
    config: &JsonSchemaValidationConfig,
) {
    validate_json_schema(&Schema::Object(root_schema.schema), false, config);
    for (_, schema) in &root_schema.definitions {
        validate_json_schema(schema, false, config);
    }
}

fn is_externally_tagged_enum(subschema_validation: &SubschemaValidation) -> bool {
    let mut subschemas = subschema_validation
        .one_of
        .iter()
        .flatten()
        .chain(subschema_validation.any_of.iter().flatten())
        .peekable();
    if subschemas.peek().is_none() {
        return false;
    }
    subschemas.all(|subscehma| {
        if let Schema::Object(SchemaObject {
            object: Some(object),
            ..
        }) = subscehma
        {
            object.properties.len() == 1
        } else {
            false
        }
    })
}

fn is_fixed_enum_value(schema: &Schema) -> bool {
    if let Schema::Object(SchemaObject {
        enum_values: Some(enum_values),
        ..
    }) = schema
    {
        enum_values.len() == 1
    } else {
        false
    }
}

fn is_top_level_metadata_object_variant(schema: &Schema) -> bool {
    if let Schema::Object(SchemaObject {
        object: Some(object),
        ..
    }) = schema
    {
        // We want to support the following combinations:
        // - kind, version, definition (common case)
        // - kind, definition (for unversioned objects)
        // - version, definition (if this utility is used in an intermediate schema before the kind is added)
        if !object.properties.contains_key("definition") || object.properties.len() == 1 {
            return false;
        }
        if !object
            .properties
            .keys()
            .all(|key| key == "definition" || key == "version" || key == "kind")
        {
            return false;
        }
        // Further validate that kind and version are just enums.
        if let Some(property_schema) = object.properties.get("version") {
            if !is_fixed_enum_value(property_schema) {
                return false;
            }
        }

        if let Some(property_schema) = object.properties.get("kind") {
            if !is_fixed_enum_value(property_schema) {
                return false;
            }
        }

        true
    } else {
        false
    }
}

fn is_nullable_wrapper(schema: &Schema) -> bool {
    if let Schema::Object(SchemaObject {
        subschemas: Some(subschemas),
        ..
    }) = schema
    {
        if let Some(any_of) = &subschemas.any_of {
            if any_of.len() == 2
                && any_of.iter().any(|subschema| {
                    if let Schema::Object(SchemaObject {
                        instance_type: Some(SingleOrVec::Single(instance_type)),
                        ..
                    }) = subschema
                    {
                        **instance_type == InstanceType::Null
                    } else {
                        false
                    }
                })
            {
                return true;
            }
        }
    }
    false
}

fn is_all_of_wrapper(schema: &Schema) -> bool {
    if let Schema::Object(SchemaObject {
        subschemas: Some(subschemas),
        ..
    }) = schema
    {
        if let Some(all_of) = &subschemas.all_of {
            return all_of.len() == 1;
        }
    }
    false
}

fn validate_json_schema(
    schema: &Schema,
    is_externally_tagged_enum_variant: bool,
    config: &JsonSchemaValidationConfig,
) {
    // Run validation checks at the current level.
    run_json_schema_validation_checks(schema, is_externally_tagged_enum_variant, config);

    // Recurse into all nested objects and validate them as well.
    if let Schema::Object(schema_object) = schema {
        if let Some(object) = &schema_object.object {
            for (_, property_schema) in &object.properties {
                validate_json_schema(property_schema, false, config);
            }
            if let Some(additional_properties) = &object.additional_properties {
                validate_json_schema(additional_properties, false, config);
            }
        }
        if let Some(subschemas) = &schema_object.subschemas {
            let externally_tagged_enum = is_externally_tagged_enum(subschemas);
            for subschema in subschemas.one_of.iter().flatten() {
                validate_json_schema(subschema, externally_tagged_enum, config);
            }
            for subschema in subschemas.any_of.iter().flatten() {
                validate_json_schema(subschema, externally_tagged_enum, config);
            }
        }
    }
}

fn run_json_schema_validation_checks(
    schema: &Schema,
    is_externally_tagged_enum_variant: bool,
    config: &JsonSchemaValidationConfig,
) {
    check_no_arbitrary_additional_properties(schema, config);
    check_titles(schema, is_externally_tagged_enum_variant);
    check_description(schema, is_externally_tagged_enum_variant);
}

fn check_description(schema: &Schema, is_externally_tagged_enum_variant: bool) {
    if is_externally_tagged_enum_variant {
        // If it is an externally tagged enum variant, its description is validated in the parent schema
        // as part of the subschema validation.
        return;
    }
    if is_top_level_metadata_object_variant(schema) {
        // Top-level metadata objects are handled specially in tooling, so okay to not have title for those.
        return;
    }
    if is_nullable_wrapper(schema) {
        // If this is just wrapping a type to make it nullable, it does not need a description.
        return;
    }
    if is_all_of_wrapper(schema) {
        // If this is just an all of wrapping another schema (which happens when we have a field-level description in the schema),
        // we don't need a description.
        return;
    }

    if let Schema::Object(object) = schema {
        let has_description = object
            .metadata
            .as_ref()
            .is_some_and(|metadata| metadata.description.is_some());

        // If the schema is of a struct, it should definitely have a description
        if object.object.is_some() {
            assert!(
                has_description,
                "Schema does not have a description present: {}",
                serde_json::to_string_pretty(schema).unwrap()
            );
        }
        // If the schema is of an enum, either it or its all subschemas should have a description.
        if let Some(subschemas) = &object.subschemas {
            let has_description_in_subschemas =
                subschemas.one_of.iter().flatten().all(|subschema| {
                    if let Schema::Object(subschema_object) = subschema {
                        subschema_object
                            .metadata
                            .as_ref()
                            .is_some_and(|metadata| metadata.description.is_some())
                    } else {
                        // If the subschema is not an object, no need of description.
                        true
                    }
                });
            // Assert for description
            assert!(
                has_description || has_description_in_subschemas,
                "Schema or its subschemas do not have a description present: {}",
                serde_json::to_string_pretty(schema).unwrap()
            );
        }
    }
}

fn check_titles(schema: &Schema, is_externally_tagged_enum_variant: bool) {
    if is_externally_tagged_enum_variant {
        // Okay to not have titles for externally tagged enum variants if the corresponding sub-schema has a title.
        return;
    }
    if is_top_level_metadata_object_variant(schema) {
        // Top-level metadata objects are handled specially in tooling, so okay to not have title for those.
        return;
    }
    if is_nullable_wrapper(schema) {
        // If this is just wrapping a type to make it nullable, it does not need a title.
        return;
    }
    if is_all_of_wrapper(schema) {
        // If this is just an all of wrapping another schema (which happens when we have a field-level description in the schema),
        // we don't need a title.
        return;
    }
    if let Schema::Object(object) = schema {
        // All objects and enums should have titles present.
        let should_have_title = object.object.is_some() || object.subschemas.is_some();

        let has_title = object
            .metadata
            .as_ref()
            .is_some_and(|metadata| metadata.title.is_some());

        assert!(
            !should_have_title || has_title,
            "Schema does not have a title present: {}",
            serde_json::to_string_pretty(schema).unwrap()
        );
    }
}

fn check_no_arbitrary_additional_properties(schema: &Schema, config: &JsonSchemaValidationConfig) {
    if let Schema::Object(schema_object) = schema {
        if let Some(object) = &schema_object.object {
            let has_arbitrary_additional_properties: bool = object.additional_properties.is_none()
                || object
                    .additional_properties
                    .as_ref()
                    .is_some_and(|property_schema| matches!(**property_schema, Schema::Bool(true)));

            let is_allowed = schema_object.metadata.as_ref().is_some_and(|metadata| {
                metadata.title.as_ref().is_some_and(|title| {
                    config
                        .schemas_with_arbitrary_additional_properties_allowed
                        .contains(&title.as_str())
                })
            });

            assert!(
                !has_arbitrary_additional_properties || is_allowed,
                "Schema has arbitrary additional properties: {}",
                serde_json::to_string_pretty(schema).unwrap()
            );
        }
    }
}
