/*
This module provides functions to generate introspection result as GraphQL schema
for each namespace from the schema.
 */
use std::collections::HashMap;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("unable to parse introspection query: {0}")]
    ParseIntrospectionQuery(String),
    #[error("unable to normalize introspection query: {0}")]
    NormalizeIntrospectionQuery(String),
    #[error("unable to find field call")]
    FieldCallNotFound,
    #[error("Only __schema field is expected but found: {name:}")]
    OnlySchemaFieldExpected { name: String },
    #[error("introspection query failed: {0}")]
    IntrospactionQueryError(#[from] crate::introspection::Error),
    #[error("unable to serialize to json: {0}")]
    SerializeJson(#[from] serde_json::Error),
}

/// Generate GraphQL schema for each namespace from given schema.
pub fn build_namespace_schemas<S: crate::schema::SchemaContext>(
    schema: &crate::schema::Schema<S>,
) -> Result<HashMap<&S::Namespace, serde_json::Value>, Error> {
    let mut response = HashMap::new();
    let request = crate::http::Request {
        operation_name: None,
        query: {
            let query_str = include_str!("introspection_query.graphql");
            crate::parser::Parser::new(query_str)
                .parse_executable_document()
                .map_err(|e| Error::ParseIntrospectionQuery(e.to_string()))?
        },
        variables: HashMap::new(),
    };
    for ns in &schema.namespaces {
        response.insert(ns, build_namespace_schema(ns, schema, &request)?);
    }
    Ok(response)
}

/// Generate GraphQL schema for a given namespace
fn build_namespace_schema<'s, S: crate::schema::SchemaContext>(
    ns: &'s S::Namespace,
    schema: &'s crate::schema::Schema<S>,
    request: &crate::http::Request,
) -> Result<serde_json::Value, Error> {
    let nr = crate::validation::normalize_request(ns, schema, request)
        .map_err(|e| Error::NormalizeIntrospectionQuery(e.to_string()))?;
    let mut result = HashMap::new();
    for (_alias, field) in nr.selection_set.fields.iter() {
        let field_call = field.field_call().map_err(|_| Error::FieldCallNotFound)?;
        match field_call.name.as_str() {
            "__schema" => {
                result.insert(
                    &field_call.name,
                    serde_json::to_value(crate::introspection::schema_type(
                        schema,
                        ns,
                        &field.selection_set,
                    )?)?,
                );
            }
            name => Err(Error::OnlySchemaFieldExpected {
                name: name.to_string(),
            })?,
        }
    }
    Ok(serde_json::to_value(result)?)
}
