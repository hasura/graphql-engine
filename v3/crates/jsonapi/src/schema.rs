use crate::{Model, State};
use std::collections::BTreeMap;
mod output;
mod parameters;
mod shared;
use shared::{
    array_schema, bool_schema, enum_schema, float_schema, int_schema, json_schema, object_schema,
    string_schema,
};

// JSONAPI specifies "application/vnd.api+json"
// we're going with the more universally supported application/json
static JSONAPI_MEDIA_TYPE: &str = "application/json";

fn get_response(model: &Model) -> oas3::spec::Response {
    let schema = oas3::spec::ObjectOrReference::Object(output::jsonapi_document_schema(model));

    let media_type = oas3::spec::MediaType {
        encoding: BTreeMap::new(),
        examples: None,
        schema: Some(schema),
    };

    let mut content = BTreeMap::new();
    content.insert(JSONAPI_MEDIA_TYPE.into(), media_type);

    oas3::spec::Response {
        description: Some(format!("Successful {} response", model.name.name)),
        extensions: BTreeMap::new(),
        headers: BTreeMap::new(),
        links: BTreeMap::new(),
        content,
    }
}

fn get_route_for_model(model: &Model) -> oas3::spec::Operation {
    let parameters = vec![
        oas3::spec::ObjectOrReference::Object(parameters::page_limit_parameter()),
        oas3::spec::ObjectOrReference::Object(parameters::page_offset_parameter()),
        oas3::spec::ObjectOrReference::Object(parameters::fields_parameter(model)),
        oas3::spec::ObjectOrReference::Object(parameters::ordering_parameter(model)),
    ];

    let mut responses = BTreeMap::new();
    responses.insert(
        "200".into(),
        oas3::spec::ObjectOrReference::Object(get_response(model)),
    );

    oas3::spec::Operation {
        callbacks: BTreeMap::new(),
        deprecated: None,
        description: model.description.clone(),
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
        title: "Hasura JSONAPI (alpha)".into(),
        summary: None,
        description: Some(
            "REST API generated to match the JSON:API spec: https://jsonapi.org".into(),
        ),
        terms_of_service: None,
        version: "0.1".into(),
        contact: None,
        license: None,
        extensions: BTreeMap::new(),
    };
    let mut paths = BTreeMap::new();
    for (route_name, route) in &state.routes {
        let get = get_route_for_model(route);

        let full_route_path = format!("/v1/rest{route_name}");

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

        paths.insert(full_route_path, path_item);
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
