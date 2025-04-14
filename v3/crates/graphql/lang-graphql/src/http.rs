use std::collections::BTreeMap;

use indexmap::IndexMap;
use nonempty::{NonEmpty, nonempty};
use serde::{Deserialize, Serialize};

use crate::ast::common as ast;
use crate::ast::executable;

/// The request as we receive it from the client, before we
/// parse the query string
#[derive(Clone, Serialize, Deserialize, Debug)]
#[serde(rename_all(serialize = "camelCase", deserialize = "camelCase"))]
pub struct RawRequest {
    pub operation_name: Option<ast::Name>,
    pub query: String,
    pub variables: Option<BTreeMap<ast::Name, serde_json::Value>>,
}

pub struct Request {
    pub operation_name: Option<ast::Name>,
    pub query: executable::ExecutableDocument,
    pub variables: BTreeMap<ast::Name, serde_json::Value>,
}

pub type VariableValues = BTreeMap<ast::Name, serde_json::Value>;

/// A list of path segments starting at the root of the response and
/// ending with the field associated with the error.
/// <https://spec.graphql.org/October2021/#sel-HAPHRPHABABC3vT>
pub type Path = Vec<PathSegment>;

/// A path segment is either a field name or an index into a list.
/// <https://spec.graphql.org/October2021/#sel-HAPHRPJABABEyoB>
#[derive(Serialize, Debug, PartialEq)]
#[serde(untagged)]
pub enum PathSegment {
    /// Path segment that represent a field.
    Field(ast::Name),
    /// Path segment that represent list indices as 0-indexed integer.
    Index(usize),
}

impl PathSegment {
    /// Returns the path segment for a field name.
    pub fn field(name: ast::Name) -> Self {
        Self::Field(name)
    }

    /// Returns the path segment for an index into a list.
    pub fn index(index: usize) -> Self {
        Self::Index(index)
    }
}

#[derive(Serialize, Debug, PartialEq)]
pub struct Extensions {
    /// Details of any error
    pub details: serde_json::Value,
}

/// A GraphQL error as defined by the spec.
/// <https://spec.graphql.org/October2021/#sec-Errors.Error-result-format>
#[derive(Serialize, Debug, PartialEq)]
pub struct GraphQLError {
    /// A string describing the error
    pub message: String,
    /// The path of the response field which experienced the error.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub path: Option<Path>,
    /// Extensions to the error with additional information.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub extensions: Option<Extensions>,
    /// Is the error an internal error? Are we interested in monitoring it?
    #[serde(skip_serializing)]
    pub is_internal: bool,
}

#[derive(Serialize)]
pub struct Response {
    #[serde(skip_serializing)]
    pub status_code: http::status::StatusCode,
    #[serde(skip_serializing)]
    pub headers: http::HeaderMap,
    pub data: Option<IndexMap<ast::Alias, serde_json::Value>>,
    /// Errors entry shouldn't be present if no errors raised
    /// <https://spec.graphql.org/October2021/#sel-FAPHFCBUBpEm7G>
    #[serde(skip_serializing_if = "Option::is_none")]
    pub errors: Option<NonEmpty<GraphQLError>>,
}

impl Response {
    pub fn ok(data: IndexMap<ast::Alias, serde_json::Value>) -> Self {
        Self {
            status_code: http::status::StatusCode::OK,
            headers: http::HeaderMap::default(),
            data: Some(data),
            errors: None,
        }
    }
    pub fn partial(
        data: IndexMap<ast::Alias, serde_json::Value>,
        errors: Vec<GraphQLError>,
        headers: http::HeaderMap,
    ) -> Self {
        Self {
            status_code: http::status::StatusCode::OK,
            headers,
            data: Some(data),
            errors: NonEmpty::from_vec(errors),
        }
    }

    pub fn error_with_status(status_code: http::status::StatusCode, error: GraphQLError) -> Self {
        Self {
            status_code,
            headers: http::HeaderMap::default(),
            data: None,
            errors: Some(nonempty![error]),
        }
    }

    pub fn error_message_with_status(
        status_code: http::status::StatusCode,
        message: String,
        is_internal: bool,
    ) -> Self {
        Self {
            status_code,
            headers: http::HeaderMap::default(),
            data: None,
            errors: Some(nonempty![GraphQLError {
                is_internal,
                message,
                path: None,
                extensions: None,
            }]),
        }
    }

    pub fn error_message_with_status_and_details(
        status_code: http::status::StatusCode,
        message: String,
        details: serde_json::Value,
        is_internal: bool,
    ) -> Self {
        Self {
            status_code,
            headers: http::HeaderMap::default(),
            data: None,
            errors: Some(nonempty![GraphQLError {
                is_internal,
                message,
                path: None,
                extensions: Some(Extensions { details }),
            }]),
        }
    }

    pub fn error(error: GraphQLError, headers: http::HeaderMap) -> Self {
        Self {
            status_code: http::status::StatusCode::OK,
            headers,
            data: None,
            errors: Some(nonempty![error]),
        }
    }

    pub fn errors_with_status(
        status_code: http::status::StatusCode,
        errors: NonEmpty<GraphQLError>,
    ) -> Self {
        Self {
            status_code,
            headers: http::HeaderMap::default(),
            data: None,
            errors: Some(errors),
        }
    }

    pub fn errors(errors: NonEmpty<GraphQLError>) -> Self {
        Self {
            status_code: http::status::StatusCode::OK,
            headers: http::HeaderMap::default(),
            data: None,
            errors: Some(errors),
        }
    }

    pub fn does_contains_error(&self) -> bool {
        self.errors.is_some()
    }
}

impl axum::response::IntoResponse for Response {
    fn into_response(self) -> axum::response::Response {
        (
            self.status_code,
            // we clone here because -
            // 1. we can't move out `self.headers`, as we serialize `self` into JSON response in next step.
            // 2. size of response headers shouldn't be a lot, so this should be inexpensive to clone.
            // 3. if this becomes expensive, we could introduce a local struct with only data and errors and serialize that.
            self.headers.clone(),
            axum::Json(self),
        )
            .into_response()
    }
}

#[cfg(test)]
mod tests {
    use super::PathSegment;
    use crate::ast::common::Name;
    use serde_json;

    #[test]
    fn test_path_serializing() {
        let path = vec![
            PathSegment::field(Name::new("one").unwrap()),
            PathSegment::index(2),
            PathSegment::index(3),
            PathSegment::field(Name::new("four").unwrap()),
        ];
        let serialized_value = serde_json::value::to_value(path).unwrap();
        assert_eq!(serialized_value, serde_json::json!(["one", 2, 3, "four"]));
    }
}
