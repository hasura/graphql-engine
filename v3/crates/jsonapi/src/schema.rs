use crate::catalog::{Model, ObjectType, State};
use std::collections::BTreeMap;
mod output;
mod parameters;
mod shared;
use metadata_resolve::Qualified;
use open_dds::types::CustomTypeName;
use output::object_schema_for_object_type;
use shared::{
    array_schema, bool_schema, enum_schema, float_schema, int_schema, json_schema, object_schema,
    pretty_typename, string_schema,
};

#[derive(Debug, thiserror::Error, serde::Serialize, serde::Deserialize)]
pub enum SchemaError {
    #[error("Object {0} not found")]
    ObjectNotFound(Qualified<CustomTypeName>),
}

// JSONAPI specifies "application/vnd.api+json"
// we're going with the more universally supported application/json
static JSONAPI_MEDIA_TYPE: &str = "application/json";

fn get_response(
    model: &Model,
    object_type: &ObjectType,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectType>,
) -> oas3::spec::Response {
    let schema = oas3::spec::ObjectOrReference::Object(output::jsonapi_document_schema(
        model,
        object_type,
        object_types,
    ));

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

fn get_route_for_model(
    model: &Model,
    object_type: &ObjectType,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectType>,
    schemas: &mut BTreeMap<String, oas3::spec::ObjectOrReference<oas3::spec::ObjectSchema>>,
    filter_boolean_expression_types: &BTreeMap<
        String,
        metadata_resolve::ResolvedObjectBooleanExpressionType,
    >,
) -> oas3::spec::Operation {
    let mut parameters = vec![
        oas3::spec::ObjectOrReference::Object(parameters::page_limit_parameter()),
        oas3::spec::ObjectOrReference::Object(parameters::page_offset_parameter()),
        oas3::spec::ObjectOrReference::Object(parameters::ordering_parameter(model, object_type)),
        oas3::spec::ObjectOrReference::Object(parameters::include_parameter(model, object_type)),
    ];
    if let Some(filter_parameter) =
        parameters::filter_parameters(model, object_type, schemas, filter_boolean_expression_types)
    {
        parameters.push(oas3::spec::ObjectOrReference::Object(filter_parameter));
    }

    let fields_parameters =
        parameters::fields_parameters(&model.data_type, object_type, object_types)
            .into_iter()
            .map(oas3::spec::ObjectOrReference::Object)
            .collect::<Vec<_>>();
    parameters.extend_from_slice(&fields_parameters);

    let mut responses = BTreeMap::new();
    responses.insert(
        "200".into(),
        oas3::spec::ObjectOrReference::Object(get_response(model, object_type, object_types)),
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

pub fn openapi_schema(state: &State) -> Result<oas3::Spec, SchemaError> {
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
    let mut schemas = BTreeMap::new();

    // get all the filter boolean expression types that we need to include in the schema
    let mut filter_boolean_expression_types = BTreeMap::new();
    for model in state.routes.values() {
        if let Some(boolean_expression_type) = &model.filter_expression_type {
            filter_boolean_expression_types.insert(
                pretty_typename(&boolean_expression_type.name),
                boolean_expression_type.clone(),
            );
        }
    }

    for (route_name, model) in &state.routes {
        let object_type = state
            .object_types
            .get(&model.data_type)
            .ok_or_else(|| SchemaError::ObjectNotFound(model.data_type.clone()))?;

        let get = get_route_for_model(
            model,
            object_type,
            &state.object_types,
            &mut schemas,
            &filter_boolean_expression_types,
        );

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

    for (object_type_name, object_type) in &state.object_types {
        schemas.insert(
            pretty_typename(object_type_name),
            oas3::spec::ObjectOrReference::Object(object_schema_for_object_type(object_type)),
        );
    }

    // we'll need to generate a named object for each object type that we can reference in our
    // models etc
    let components = oas3::spec::Components {
        callbacks: BTreeMap::new(),
        examples: BTreeMap::new(),
        extensions: BTreeMap::new(),
        headers: BTreeMap::new(),
        links: BTreeMap::new(),
        parameters: BTreeMap::new(),
        path_items: BTreeMap::new(),
        request_bodies: BTreeMap::new(),
        responses: BTreeMap::new(),
        schemas,
        security_schemes: BTreeMap::new(),
    };

    Ok(oas3::Spec {
        openapi: "3.1.0".into(),
        info,
        servers: vec![],
        paths: Some(paths),
        components: Some(components),
        tags: vec![],
        webhooks: BTreeMap::new(),
        external_docs: None,
        extensions: BTreeMap::new(),
    })
}
