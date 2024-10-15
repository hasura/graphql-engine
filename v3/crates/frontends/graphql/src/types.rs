use lang_graphql as gql;
use lang_graphql::http::Response;
use tracing_util::{ErrorVisibility, Traceable, TraceableError};

#[derive(Debug)]
/// A simple wrapper around a reference of GraphQL errors
pub struct GraphQLErrors<'a>(pub &'a nonempty::NonEmpty<gql::http::GraphQLError>);

impl<'a> std::fmt::Display for GraphQLErrors<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let messages = self
            .0
            .iter()
            .map(|e| e.message.as_str())
            .collect::<Vec<_>>();
        write!(f, "{}", messages.join(", "))
    }
}

/// Implement traceable error for GraphQL Errors
impl<'a> TraceableError for GraphQLErrors<'a> {
    fn visibility(&self) -> ErrorVisibility {
        // Traces related to GraphQL errors are always visible to the user
        ErrorVisibility::User
    }
}

/// A simple wrapper around a GraphQL HTTP response
pub struct GraphQLResponse(gql::http::Response);

impl GraphQLResponse {
    pub fn from_result(
        result: execute::ExecuteQueryResult,
        expose_internal_errors: execute::ExposeInternalErrors,
    ) -> Self {
        Self(result.to_graphql_response(expose_internal_errors))
    }

    pub fn from_error(
        err: &execute::RequestError,
        expose_internal_errors: execute::ExposeInternalErrors,
    ) -> Self {
        Self(Response::error(
            err.to_graphql_error(expose_internal_errors),
            axum::http::HeaderMap::default(),
        ))
    }

    pub fn from_response(response: gql::http::Response) -> Self {
        Self(response)
    }

    pub fn does_contain_error(&self) -> bool {
        self.0.does_contains_error()
    }

    pub fn inner(self) -> gql::http::Response {
        self.0
    }
}

/// Implement traceable for GraphQL Response
impl Traceable for GraphQLResponse {
    type ErrorType<'a> = GraphQLErrors<'a>;

    fn get_error(&self) -> Option<GraphQLErrors<'_>> {
        self.0.errors.as_ref().map(GraphQLErrors)
    }
}

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub struct GraphQlParseError(#[from] pub gql::ast::spanning::Positioned<gql::parser::Error>);

impl TraceableError for GraphQlParseError {
    fn visibility(&self) -> ErrorVisibility {
        ErrorVisibility::User
    }
}

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub struct GraphQlValidationError(#[from] pub gql::validation::Error);
impl TraceableError for GraphQlValidationError {
    fn visibility(&self) -> ErrorVisibility {
        ErrorVisibility::User
    }
}
