use indexmap::IndexMap;
use lang_graphql as gql;
use lang_graphql::ast::common as ast;
use serde_json as json;
use tracing_util::Traceable;

use crate::process_response::ProcessedResponse;
use engine_types::ExposeInternalErrors;
use execute::FieldError;

#[derive(Debug)]
pub struct RootFieldResult {
    pub is_nullable: bool,
    pub result: Result<json::Value, FieldError>,
    pub headers: Option<reqwest::header::HeaderMap>,
}

impl Traceable for RootFieldResult {
    type ErrorType<'a> = <Result<json::Value, FieldError> as Traceable>::ErrorType<'a>;

    fn get_error(&self) -> Option<Self::ErrorType<'_>> {
        Traceable::get_error(&self.result)
    }
}

impl RootFieldResult {
    pub fn new(is_nullable: bool, result: Result<json::Value, FieldError>) -> Self {
        Self {
            is_nullable,
            result,
            headers: None,
        }
    }
    pub fn from_processed_response(
        is_nullable: bool,
        result: Result<ProcessedResponse, FieldError>,
    ) -> Self {
        match result {
            Ok(processed_response) => Self {
                is_nullable,
                result: Ok(processed_response.response),
                headers: processed_response.response_headers.map(|h| h.0),
            },
            Err(field_error) => Self {
                is_nullable,
                result: Err(field_error),
                headers: None,
            },
        }
    }
}

#[derive(Debug)]
pub struct ExecuteQueryResult {
    pub root_fields: IndexMap<ast::Alias, RootFieldResult>,
}

const SET_COOKIE_HEADER_NAME: axum::http::HeaderName =
    axum::http::HeaderName::from_static("set-cookie");

impl ExecuteQueryResult {
    /// Converts the result into a GraphQL response
    #[allow(clippy::wrong_self_convention)]
    pub fn to_graphql_response(
        self,
        expose_internal_errors: ExposeInternalErrors,
    ) -> gql::http::Response {
        let mut data = IndexMap::new();
        let mut errors = Vec::new();
        let mut headers = Vec::new();
        for (alias, field_result) in self.root_fields {
            let result = match field_result.result {
                Ok(value) => value,
                Err(e) => {
                    let path = vec![gql::http::PathSegment::field(alias.clone().0)];
                    // When error occur, check if the field is nullable
                    if field_result.is_nullable {
                        // If field is nullable, collect error and mark the field as null
                        errors.push(e.to_graphql_error(expose_internal_errors, Some(path)));
                        json::Value::Null
                    } else {
                        // If the field is not nullable, return `null` data response with the error.
                        // We return whatever headers we have collected until this point.
                        return gql::http::Response::error(
                            e.to_graphql_error(expose_internal_errors, Some(path)),
                            Self::merge_headers(headers),
                        );
                    }
                }
            };
            data.insert(alias, result);

            // if this root field result has headers, collect it
            if let Some(header_map) = field_result.headers {
                headers.push(header_map);
            }
        }

        gql::http::Response::partial(data, errors, Self::merge_headers(headers))
    }

    // merge all the headers of all root fields
    fn merge_headers(headers: Vec<axum::http::HeaderMap>) -> axum::http::HeaderMap {
        let mut result_map = axum::http::HeaderMap::new();
        for header_map in headers {
            for (name, val) in header_map {
                if let Some(name) = name {
                    match result_map.entry(&name) {
                        axum::http::header::Entry::Vacant(vacant) => {
                            vacant.insert(val);
                        }
                        axum::http::header::Entry::Occupied(mut occupied) => {
                            if name == SET_COOKIE_HEADER_NAME {
                                let prev_val = occupied.get();
                                if prev_val != val {
                                    occupied.append(val);
                                }
                            } else {
                                occupied.insert(val);
                            }
                        }
                    }
                }
            }
        }
        result_map
    }
}
