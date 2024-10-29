use super::shared::{enum_schema, int_schema};
use crate::Model;
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

pub fn fields_parameter(model: &Model) -> oas3::spec::Parameter {
    let schema = oas3::spec::ObjectOrReference::Object(oas3::spec::ObjectSchema {
        items: Some(Box::new(oas3::spec::ObjectOrReference::Object(
            enum_schema(model.type_fields.keys().map(ToString::to_string).collect()),
        ))),
        ..oas3::spec::ObjectSchema::default()
    });

    let mut example = String::new();
    for (i, field) in model.type_fields.keys().enumerate() {
        if i > 0 && i < model.type_fields.len() {
            example.push(',');
        }
        example.push_str(&field.to_string());
    }

    oas3::spec::Parameter {
        name: format!("fields[{}]", model.name.name),
        allow_empty_value: None,
        allow_reserved: None,
        content: None,
        deprecated: None,
        description: Some(format!("Optional list of fields from {} to include in response. If no fields are provided, all fields are returned",model.name.name)),
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

pub fn ordering_parameter(model: &Model) -> oas3::spec::Parameter {
    // each field can be `thing` (sort ascending by 'thing') or `-thing` (sort descending by
    // 'thing')
    let mut sort_keys = Vec::new();
    for type_field in model.type_fields.keys() {
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
