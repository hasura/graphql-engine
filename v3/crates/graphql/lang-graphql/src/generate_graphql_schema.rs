/*
This module provides functions to generate introspection result as GraphQL schema
for each namespace from the schema.
 */
use json_ext;
use std::collections::BTreeMap;
use std::sync::OnceLock;
use tracing_util::SpanVisibility;
use tracing_util::{ErrorVisibility, TraceableError};

#[derive(Debug, thiserror::Error)]
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
impl TraceableError for Error {
    fn visibility(&self) -> ErrorVisibility {
        ErrorVisibility::User
    }
}

/// Generate GraphQL schema for a given namespace
pub fn build_namespace_schema<
    S: crate::schema::SchemaContext,
    NSGet: crate::schema::NamespacedGetter<S>,
>(
    namespaced_getter: &NSGet,
    schema: &crate::schema::Schema<S>,
) -> Result<serde_json::Value, Error> {
    let tracer = tracing_util::global_tracer();
    tracer.in_span(
        "build_namespace_schema",
        "Generate GraphQL schema for a given namespace",
        SpanVisibility::Internal,
        || {
            let nr = crate::validation::normalize_request(
                namespaced_getter,
                schema,
                introspection_request(),
                // We can safely ignore this because we don't have any variables
                crate::validation::NonNullGraphqlVariablesValidation::Validate,
            )
            .map_err(|e| Error::NormalizeIntrospectionQuery(e.to_string()))?;
            // Build a Value directly to avoid the same performance hit we fixed in f709e4f8a
            let mut result = serde_json::Map::with_capacity(nr.selection_set.fields.len());
            for (_alias, field) in &nr.selection_set.fields {
                let field_call = field.field_call().map_err(|_| Error::FieldCallNotFound)?;
                match field_call.name.as_str() {
                    "__schema" => {
                        result.insert(
                            field_call.name.to_string(),
                            json_ext::alias_map_to_value(crate::introspection::schema_type(
                                schema,
                                namespaced_getter,
                                &field.selection_set,
                            )?),
                        );
                    }
                    name => Err(Error::OnlySchemaFieldExpected {
                        name: name.to_string(),
                    })?,
                }
            }
            Ok(serde_json::Value::Object(result))
        },
    )
}

fn introspection_request() -> &'static crate::http::Request {
    static CELL: OnceLock<crate::http::Request> = OnceLock::new();
    CELL.get_or_init(|| crate::http::Request {
        operation_name: None,
        query: {
            let query_str = include_str!("introspection_query.graphql");
            crate::parser::Parser::new(query_str)
                .parse_executable_document()
                .unwrap()
        },
        variables: BTreeMap::new(),
    })
}
