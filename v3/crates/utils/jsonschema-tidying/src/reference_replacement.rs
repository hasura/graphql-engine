//! Replace object references within a JSON schema.

use schemars::schema::{
    ArrayValidation, ObjectValidation, Schema, SchemaObject, SingleOrVec, SubschemaValidation,
};

/// Replace any `search` reference with the `replace` reference in the given schema.
pub fn in_schema(schema: &mut Schema, search: &str, replace: &str) {
    if let Schema::Object(schema_object) = schema {
        in_schema_object(schema_object, search, replace);
    }
}

/// Replace any `search` reference with the `replace` reference in the given schema object.
pub fn in_schema_object(schema_object: &mut SchemaObject, search: &str, replace: &str) {
    if let Some(subschema_validation) = &mut schema_object.subschemas {
        in_subschema_validation(&mut *subschema_validation, search, replace)
    }

    if let Some(array_validation) = &mut schema_object.array {
        in_array_validation(&mut *array_validation, search, replace)
    }

    if let Some(object_validation) = &mut schema_object.object {
        in_object_validation(&mut *object_validation, search, replace)
    }

    // This is the whole reason this module exists.
    if let Some(reference) = &schema_object.reference {
        if reference == search {
            schema_object.reference = Some(replace.to_string());
        }
    }
}

/// Replace any `search` reference with the `replace` reference in the given subschema validation.
pub fn in_subschema_validation(
    subschema_validation: &mut SubschemaValidation,
    search: &str,
    replace: &str,
) {
    if let Some(all_of) = &mut subschema_validation.all_of {
        for schema in all_of.iter_mut() {
            in_schema(schema, search, replace);
        }
    }

    if let Some(any_of) = &mut subschema_validation.any_of {
        for schema in any_of.iter_mut() {
            in_schema(schema, search, replace);
        }
    }

    if let Some(one_of) = &mut subschema_validation.one_of {
        for schema in one_of.iter_mut() {
            in_schema(schema, search, replace);
        }
    }

    if let Some(schema) = &mut subschema_validation.not {
        in_schema(&mut *schema, search, replace);
    }

    if let Some(schema) = &mut subschema_validation.if_schema {
        in_schema(&mut *schema, search, replace);
    }

    if let Some(schema) = &mut subschema_validation.then_schema {
        in_schema(&mut *schema, search, replace);
    }

    if let Some(schema) = &mut subschema_validation.else_schema {
        in_schema(&mut *schema, search, replace);
    }
}

/// Replace any `search` reference with the `replace` reference in the given array validation.
pub fn in_array_validation(array_validation: &mut ArrayValidation, search: &str, replace: &str) {
    if let Some(items) = &mut array_validation.items {
        match items {
            SingleOrVec::Single(schema) => in_schema(schema, search, replace),
            SingleOrVec::Vec(schemas) => {
                for schema in schemas.iter_mut() {
                    in_schema(&mut *schema, search, replace);
                }
            }
        }
    }

    if let Some(additional_items) = &mut array_validation.additional_items {
        in_schema(&mut *additional_items, search, replace);
    }

    if let Some(contains) = &mut array_validation.contains {
        in_schema(&mut *contains, search, replace);
    }
}

/// Replace any `search` reference with the `replace` reference in the given object validation.
pub fn in_object_validation(object_validation: &mut ObjectValidation, search: &str, replace: &str) {
    for property in object_validation.properties.values_mut() {
        in_schema(property, search, replace);
    }

    for property in object_validation.pattern_properties.values_mut() {
        in_schema(property, search, replace);
    }

    if let Some(additional_properties) = &mut object_validation.additional_properties {
        in_schema(&mut *additional_properties, search, replace);
    }

    if let Some(property_names) = &mut object_validation.property_names {
        in_schema(&mut *property_names, search, replace);
    }
}
