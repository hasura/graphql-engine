use super::shared::{any_of_schema, pretty_typename};
use crate::catalog::{Model, ObjectType, RelationshipTarget, Type};
use crate::schema::{
    array_schema, bool_schema, enum_schema, float_schema, int_schema, json_schema, object_schema,
    string_schema,
};
use metadata_resolve::{unwrap_custom_type_name, Qualified};
use oas3::spec::{ObjectOrReference, ObjectSchema};
use open_dds::relationships::RelationshipType;
use open_dds::types::CustomTypeName;
use std::collections::BTreeMap;

// output types as per JSON API
// only covering bare minimum atm
// {"data":[
//  {"type":"app_DeviceConfigurationTemplate",
//   "id":"1",
//   "attributes":{
//      "id":"df3182fbd08d372caa6b7fc5",
//      "organization":{"isDefaultOrganization":false,"name":"subseco"}
//    }
//  }
// ]}%

// an OpenDD type represented in OpenAPI
fn type_schema(ty: &Type) -> ObjectOrReference<ObjectSchema> {
    match ty {
        Type::ScalarForDataConnector(set_of_types) => {
            // if there is only one, use it, otherwise, JSON
            match set_of_types.type_representations.first() {
                Some(ty) => {
                    if set_of_types.type_representations.len() == 1 {
                        ObjectOrReference::Object(from_type_representation(ty))
                    } else {
                        ObjectOrReference::Object(from_type_representation(
                            &ndc_models::TypeRepresentation::JSON,
                        ))
                    }
                }
                None => ObjectOrReference::Object(from_type_representation(
                    &ndc_models::TypeRepresentation::JSON,
                )),
            }
        }
        Type::Scalar(type_representation) => {
            ObjectOrReference::Object(from_type_representation(type_representation))
        }
        Type::List(field_type) => ObjectOrReference::Object(array_schema(type_schema(field_type))),
        Type::Object(object_type_name) => ObjectOrReference::Ref {
            ref_path: pretty_typename(object_type_name),
        },
    }
}

// what we output for each type
pub fn object_schema_for_object_type(object_type: &ObjectType) -> ObjectSchema {
    let mut fields = BTreeMap::new();
    for (name, ty) in &object_type.type_fields {
        fields.insert(name.to_string(), type_schema(ty));
    }
    let required = vec![]; // these are used as output types for the moment so everything is
                           // nullable
    object_schema(fields, required)
}

#[allow(deprecated)]
fn from_type_representation(type_representation: &ndc_models::TypeRepresentation) -> ObjectSchema {
    match type_representation {
        ndc_models::TypeRepresentation::Boolean => bool_schema(),
        ndc_models::TypeRepresentation::Int8
        | ndc_models::TypeRepresentation::Int16
        | ndc_models::TypeRepresentation::Int32
        | ndc_models::TypeRepresentation::Int64
        | ndc_models::TypeRepresentation::BigInteger => int_schema(),
        ndc_models::TypeRepresentation::Float32
        | ndc_models::TypeRepresentation::Float64
        | ndc_models::TypeRepresentation::BigDecimal => float_schema(),
        ndc_models::TypeRepresentation::JSON => json_schema(),
        ndc_models::TypeRepresentation::UUID => string_schema(Some("uuid".into())),
        ndc_models::TypeRepresentation::Date => string_schema(Some("date".into())),
        ndc_models::TypeRepresentation::Bytes
        | ndc_models::TypeRepresentation::String
        | ndc_models::TypeRepresentation::Timestamp
        | ndc_models::TypeRepresentation::TimestampTZ => string_schema(None),
        ndc_models::TypeRepresentation::Geography | ndc_models::TypeRepresentation::Geometry => {
            json_schema()
        }
        ndc_models::TypeRepresentation::Enum { one_of } => enum_schema(one_of.clone()),
    }
}

// Generate schema of the "relationships" property of  "data" object
fn jsonapi_relationships_property(object_type: &ObjectType) -> Option<ObjectSchema> {
    let mut relationships = BTreeMap::new();
    for (relationship_name, relationship_target) in &object_type.type_relationships {
        let (type_name, relationship_type) = match relationship_target {
            RelationshipTarget::Model {
                object_type,
                relationship_type,
            } => (object_type, relationship_type),
            RelationshipTarget::Command { type_reference } => {
                match unwrap_custom_type_name(type_reference) {
                    Some(type_name) => (
                        type_name,
                        &crate::helpers::type_reference_to_relationship_type(type_reference),
                    ),
                    None => continue, // Skip command relationships with built-in output types
                }
            }
        };
        let relationship_object_schema = {
            let mut relationship_object_properties = BTreeMap::new();
            relationship_object_properties
                .insert("id".into(), ObjectOrReference::Object(string_schema(None)));
            relationship_object_properties.insert(
                "_type".into(),
                ObjectOrReference::Object(enum_schema(vec![pretty_typename(type_name)])),
            );
            object_schema(
                relationship_object_properties,
                vec!["id".into(), "_type".into()],
            )
        };
        let schema = match relationship_type {
            RelationshipType::Object => {
                let mut properties = BTreeMap::new();
                properties.insert(
                    "data".into(),
                    ObjectOrReference::Object(relationship_object_schema),
                );
                object_schema(properties, vec!["data".into()])
            }
            RelationshipType::Array => {
                let mut properties = BTreeMap::new();
                properties.insert(
                    "data".into(),
                    ObjectOrReference::Object(array_schema(ObjectOrReference::Object(
                        relationship_object_schema,
                    ))),
                );
                object_schema(properties, vec!["data".into()])
            }
        };
        relationships.insert(
            relationship_name.to_string(),
            ObjectOrReference::Object(schema),
        );
    }
    if relationships.is_empty() {
        None
    } else {
        Some(object_schema(relationships, vec![]))
    }
}

// a single 'data' item. we'll need to pass in types so we can
// have nice types for the `attributes` and `type`, for now let's make
// them stringy
fn jsonapi_data_schema(
    type_name: &Qualified<CustomTypeName>,
    object_type: &ObjectType,
) -> ObjectSchema {
    let mut attributes = BTreeMap::new();
    for (field_name, field_type) in &object_type.type_fields {
        attributes.insert(field_name.to_string(), type_schema(field_type));
    }

    let mut properties = BTreeMap::new();

    properties.insert("id".into(), ObjectOrReference::Object(string_schema(None)));

    properties.insert(
        "_type".into(),
        ObjectOrReference::Object(enum_schema(vec![pretty_typename(type_name)])),
    );

    properties.insert(
        "attributes".into(),
        ObjectOrReference::Object(object_schema(attributes, vec![])),
    );

    // relationships property
    if let Some(relationships) = jsonapi_relationships_property(object_type) {
        properties.insert(
            "relationships".into(),
            ObjectOrReference::Object(relationships),
        );
    }
    object_schema(
        properties,
        vec!["id".into(), "_type".into(), "attributes".into()],
    )
}

// A single item of top level "included" field.
// Can be any one of accessible relationship's schema.
fn jsonapi_included_schema(
    object_type: &ObjectType,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectType>,
) -> Option<ObjectSchema> {
    let mut relationships = Vec::new();
    for (_relationship_name, relationship_target) in &object_type.type_relationships {
        let type_name = match relationship_target {
            RelationshipTarget::Model {
                object_type,
                relationship_type: _,
            } => object_type,
            RelationshipTarget::Command { type_reference } => {
                match unwrap_custom_type_name(type_reference) {
                    Some(type_name) => type_name,
                    None => continue, // Skip command relationships with built-in output types
                }
            }
        };
        // Omit the relationship field if the role lacks permission to access the relationship type.
        if let Some(relationship_object_type) = object_types.get(type_name) {
            relationships.push(jsonapi_data_schema(type_name, relationship_object_type));
        }
    }
    if relationships.is_empty() {
        None
    } else {
        Some(any_of_schema(relationships))
    }
}

// top level jsonapi document
pub fn jsonapi_document_schema(
    model: &Model,
    object_type: &ObjectType,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectType>,
) -> ObjectSchema {
    let mut properties = BTreeMap::new();

    properties.insert(
        "data".into(),
        ObjectOrReference::Object(array_schema(ObjectOrReference::Object(
            jsonapi_data_schema(&model.data_type, object_type),
        ))),
    );

    if let Some(included) = jsonapi_included_schema(object_type, object_types) {
        properties.insert(
            "included".into(),
            ObjectOrReference::Object(array_schema(ObjectOrReference::Object(included))),
        );
    }

    object_schema(properties, vec!["data".into()])
}
