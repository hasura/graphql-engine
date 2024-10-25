use crate::schema::{
    array_schema, bool_schema, enum_schema, float_schema, int_schema, json_schema, object_schema,
    string_schema,
};
use crate::{FieldType, Model};

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
fn type_schema(field_type: &FieldType) -> ObjectSchema {
    match field_type {
        FieldType::TypeRepresentation(type_representation) => {
            from_type_representation(type_representation)
        }
        FieldType::List(field_type) => {
            let inner = type_schema(field_type);
            array_schema(ObjectOrReference::Object(inner))
        }
        FieldType::Object(fields) => object_schema(
            fields
                .iter()
                .map(|(k, v)| (k.to_string(), ObjectOrReference::Object(type_schema(v))))
                .collect(),
            vec![],
        ),
    }
}

#[allow(deprecated)]
fn from_type_representation(type_representation: &ndc_models::TypeRepresentation) -> ObjectSchema {
    match type_representation {
        ndc_models::TypeRepresentation::Boolean => bool_schema(),
        ndc_models::TypeRepresentation::Integer
        | ndc_models::TypeRepresentation::Int8
        | ndc_models::TypeRepresentation::Int16
        | ndc_models::TypeRepresentation::Int32
        | ndc_models::TypeRepresentation::Int64
        | ndc_models::TypeRepresentation::BigInteger => int_schema(),
        ndc_models::TypeRepresentation::Number
        | ndc_models::TypeRepresentation::Float32
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
fn jsonapi_data_schema(model: &Model) -> ObjectSchema {
    let mut attributes = BTreeMap::new();
    for (field_name, field_type) in &model.type_fields {
        attributes.insert(
            field_name.to_string(),
            ObjectOrReference::Object(type_schema(field_type)),
        );
    }

    let mut properties = BTreeMap::new();

    properties.insert("id".into(), ObjectOrReference::Object(string_schema(None)));

    properties.insert(
        "_type".into(),
        ObjectOrReference::Object(enum_schema(vec![model.pretty_typename()])),
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
pub fn jsonapi_document_schema(model: &Model) -> ObjectSchema {
    let mut properties = BTreeMap::new();

    properties.insert(
        "data".into(),
        ObjectOrReference::Object(array_schema(ObjectOrReference::Object(
            jsonapi_data_schema(model),
        ))),
    );

    object_schema(properties, vec!["data".into()])
}
