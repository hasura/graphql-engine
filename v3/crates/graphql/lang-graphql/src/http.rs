use std::collections::BTreeMap;

use indexmap::IndexMap;
use nonempty::{nonempty, NonEmpty};
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

/// A GraphQL response
pub struct Response {
    pub status_code: http::status::StatusCode,
    pub headers: http::HeaderMap,
    pub body: ResponseBody,
}

/// A GraphQL response body
/// Ref: <https://spec.graphql.org/October2021/#sec-Response-Format>
#[derive(Serialize)]
pub struct ResponseBody {
    #[serde(skip_serializing_if = "ResponseData::omit")]
    pub data: ResponseData,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub errors: Option<NonEmpty<GraphQLError>>,
}

impl ResponseBody {
    pub fn new(
        data: Option<IndexMap<ast::Alias, serde_json::Value>>,
        errors: Vec<GraphQLError>,
    ) -> Self {
        Self {
            data: ResponseData::Data(data),
            errors: NonEmpty::from_vec(errors),
        }
    }
    fn request_error(error: GraphQLError) -> Self {
        // A request error shouldn't include "data" in the response
        // Ref: https://spec.graphql.org/October2021/#sec-Errors.Request-errors
        Self {
            data: ResponseData::Omit,
            errors: Some(nonempty![error]),
        }
    }

    pub fn data(&self) -> Option<&IndexMap<ast::Alias, serde_json::Value>> {
        match &self.data {
            ResponseData::Omit => None,
            ResponseData::Data(data) => data.as_ref(),
        }
    }
}

/// `Omit` indicates a `"data":` field in the response must be omitted, e.g. because an error was raised before execution could begin (a `request_error`)
#[derive(Debug)]
pub enum ResponseData {
    Omit,
    Data(Option<IndexMap<ast::Alias, serde_json::Value>>),
}

impl ResponseData {
    fn omit(&self) -> bool {
        match self {
            Self::Omit => true,
            Self::Data(_) => false,
        }
    }
}

impl Serialize for ResponseData {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            ResponseData::Omit => serializer.serialize_none(),
            ResponseData::Data(data) => data.serialize(serializer),
        }
    }
}

impl Response {
    pub fn partial(
        data: IndexMap<ast::Alias, serde_json::Value>,
        errors: Vec<GraphQLError>,
        headers: http::HeaderMap,
    ) -> Self {
        Self {
            status_code: http::status::StatusCode::OK,
            headers,
            body: ResponseBody::new(Some(data), errors),
        }
    }

    pub fn errors(errors: Vec<GraphQLError>, headers: http::HeaderMap) -> Self {
        Self {
            status_code: http::status::StatusCode::OK,
            headers,
            body: ResponseBody::new(None, errors),
        }
    }

    /// Ref: <https://spec.graphql.org/October2021/#sec-Errors.Request-errors>
    pub fn request_error_with_status(
        status_code: http::status::StatusCode,
        error: GraphQLError,
    ) -> Self {
        Self {
            status_code,
            headers: http::HeaderMap::default(),
            body: ResponseBody::request_error(error),
        }
    }

    /// Ref: <https://spec.graphql.org/October2021/#sec-Errors.Request-errors>
    pub fn request_error_message_with_status(
        status_code: http::status::StatusCode,
        message: String,
        is_internal: bool,
    ) -> Self {
        Self::request_error_with_status(
            status_code,
            GraphQLError {
                is_internal,
                message,
                path: None,
                extensions: None,
            },
        )
    }

    /// Ref: <https://spec.graphql.org/October2021/#sec-Errors.Request-errors>
    pub fn request_error(error: GraphQLError, headers: http::HeaderMap) -> Self {
        Self {
            status_code: http::status::StatusCode::OK,
            headers,
            body: ResponseBody::request_error(error),
        }
    }

    pub fn does_contains_error(&self) -> bool {
        self.body.errors.is_some()
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
            axum::Json(self.body),
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
