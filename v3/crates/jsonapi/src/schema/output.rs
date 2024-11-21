use super::shared::pretty_typename;
use crate::catalog::{Model, ObjectType, Type};
use crate::schema::{
    array_schema, bool_schema, enum_schema, float_schema, int_schema, json_schema, object_schema,
    string_schema,
};
use oas3::spec::{ObjectOrReference, ObjectSchema};
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

// a single 'data' item. we'll need to pass in types so we can
// have nice types for the `attributes` and `type`, for now let's make
// them stringy
fn jsonapi_data_schema(model: &Model, object_type: &ObjectType) -> ObjectSchema {
    let mut attributes = BTreeMap::new();
    for (field_name, field_type) in &object_type.type_fields {
        attributes.insert(field_name.to_string(), type_schema(field_type));
    }

    let mut properties = BTreeMap::new();

    properties.insert("id".into(), ObjectOrReference::Object(string_schema(None)));

    properties.insert(
        "_type".into(),
        ObjectOrReference::Object(enum_schema(vec![pretty_typename(&model.data_type)])),
    );

    properties.insert(
        "attributes".into(),
        ObjectOrReference::Object(object_schema(attributes, vec![])),
    );

    object_schema(
        properties,
        vec!["id".into(), "_type".into(), "attributes".into()],
    )
}

// top level jsonapi document
pub fn jsonapi_document_schema(model: &Model, object_type: &ObjectType) -> ObjectSchema {
    let mut properties = BTreeMap::new();

    properties.insert(
        "data".into(),
        ObjectOrReference::Object(array_schema(ObjectOrReference::Object(
            jsonapi_data_schema(model, object_type),
        ))),
    );

    object_schema(properties, vec!["data".into()])
}
