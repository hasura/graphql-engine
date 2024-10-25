use crate::{Model, State};
use std::collections::BTreeMap;
use std::string::ToString;
mod output;
mod shared;
use shared::{
    array_schema, bool_schema, enum_schema, float_schema, int_schema, json_schema, object_schema,
    string_schema,
};

fn page_offset_parameter() -> oas3::spec::Parameter {
    let schema = oas3::spec::ObjectOrReference::Object(int_schema());
    oas3::spec::Parameter {
        name: "page[offset]".into(),
        allow_empty_value: None,
        allow_reserved: None,
        content: None,
        deprecated: None,
        description: None,
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

fn page_limit_parameter() -> oas3::spec::Parameter {
    let schema = oas3::spec::ObjectOrReference::Object(int_schema());
    oas3::spec::Parameter {
        name: "page[limit]".into(),
        allow_empty_value: None,
        allow_reserved: None,
        content: None,
        deprecated: None,
        description: None,
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

fn get_fields_for_model(model: &Model) -> oas3::spec::Parameter {
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
        description: model.description.clone(),
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

fn get_response(model: &Model) -> oas3::spec::Response {
    let schema = oas3::spec::ObjectOrReference::Object(output::jsonapi_document_schema(model));

    let media_type = oas3::spec::MediaType {
        encoding: BTreeMap::new(),
        examples: None,
        schema: Some(schema),
    };

    let mut content = BTreeMap::new();
    content.insert("Success".into(), media_type);

    oas3::spec::Response {
        description: None,
        extensions: BTreeMap::new(),
        headers: BTreeMap::new(),
        links: BTreeMap::new(),
        content,
    }
}

fn get_route_for_model(model: &Model) -> oas3::spec::Operation {
    let parameters = vec![
        oas3::spec::ObjectOrReference::Object(page_limit_parameter()),
        oas3::spec::ObjectOrReference::Object(page_offset_parameter()),
        oas3::spec::ObjectOrReference::Object(get_fields_for_model(model)),
    ];

    let mut responses = BTreeMap::new();
    responses.insert(
        "200".into(),
        oas3::spec::ObjectOrReference::Object(get_response(model)),
    );

    oas3::spec::Operation {
        callbacks: BTreeMap::new(),
        deprecated: None,
        description: None,
        extensions: BTreeMap::new(),
        external_docs: None,
        operation_id: None,
        parameters,
        request_body: None,
        responses: Some(responses),
        servers: vec![],
        summary: Some(format!("Fetch {} values", model.data_type.name)),
        tags: vec![],
    }
}

// output when we make a request for an unknown role
pub fn empty_schema() -> oas3::Spec {
    let info = oas3::spec::Info {
        title: "JSONAPI".into(),
        summary: None,
        description: None,
        terms_of_service: None,
        version: "0.1".into(),
        contact: None,
        license: None,
        extensions: BTreeMap::new(),
    };
    oas3::Spec {
        openapi: "3.1.0".into(),
        info,
        servers: vec![],
        paths: None,
        components: None,
        tags: vec![],
        webhooks: BTreeMap::new(),
        external_docs: None,
        extensions: BTreeMap::new(),
    }
}
pub fn openapi_schema(state: &State) -> oas3::Spec {
    let info = oas3::spec::Info {
        title: "JSONAPI".into(),
        summary: None,
        description: None,
        terms_of_service: None,
        version: "0.1".into(),
        contact: None,
        license: None,
        extensions: BTreeMap::new(),
    };
    let mut paths = BTreeMap::new();
    for (route_name, route) in &state.routes {
        let get = get_route_for_model(route);

        let path_item = oas3::spec::PathItem {
            delete: None,
            description: None,
            extensions: BTreeMap::new(),
            get: Some(get),
            head: None,
            options: None,
            parameters: vec![],
            patch: None,
            post: None,
            put: None,
            reference: None,
            servers: vec![],
            summary: None,
            trace: None,
        };

        paths.insert(route_name.clone(), path_item);
    }
    oas3::Spec {
        openapi: "3.1.0".into(),
        info,
        servers: vec![],
        paths: Some(paths),
        components: None,
        tags: vec![],
        webhooks: BTreeMap::new(),
        external_docs: None,
        extensions: BTreeMap::new(),
    }
}
