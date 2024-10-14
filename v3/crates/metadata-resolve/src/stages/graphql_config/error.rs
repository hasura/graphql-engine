use lang_graphql::ast::common as ast;

#[derive(Debug, thiserror::Error)]
pub enum GraphqlConfigError {
    #[error("graphql configuration is not defined in supergraph")]
    MissingGraphqlConfig,
    #[error("graphql configuration should be defined only once in supergraph")]
    MultipleGraphqlConfigDefinition,
    #[error("the fieldName for limitInput needs to be defined in GraphqlConfig, when models have a selectMany graphql API")]
    MissingLimitFieldInGraphqlConfig,
    #[error("the fieldName for offsetInput needs to be defined in GraphqlConfig, when models have a selectMany graphql API")]
    MissingOffsetFieldInGraphqlConfig,
    #[error("the filterInput needs to be defined in GraphqlConfig, when models have filterExpressionType")]
    MissingFilterInputFieldInGraphqlConfig,
    #[error("the orderByInput needs to be defined in GraphqlConfig, when models have orderByExpressionType")]
    MissingOrderByInputFieldInGraphqlConfig,
    #[error("the orderByInput.enumTypeNames needs to be defined in GraphqlConfig, when models have orderByExpressionType")]
    MissingOrderByEnumTypeNamesInGraphqlConfig,
    #[error("only one enumTypeNames can be defined in GraphqlConfig, whose direction values are both 'asc' and 'desc'.")]
    MultipleOrderByEnumTypeNamesInGraphqlConfig,
    #[error(
            "invalid directions: {directions} defined in orderByInput of GraphqlConfig , currently there is no support for partial directions. Please specify a type which has both 'asc' and 'desc' directions"
        )]
    InvalidOrderByDirection { directions: String },
    #[error("the fieldName for argumentsInput needs to be defined in GraphqlConfig, when models have argumentsInputType")]
    MissingArgumentsInputFieldInGraphqlConfig,
    #[error("the filterInputFieldName for aggregate needs to be defined in GraphqlConfig, when models have a selectAggregate graphql API")]
    MissingAggregateFilterInputFieldNameInGraphqlConfig,
    #[error("\"{name:}\" is not a valid GraphQL name.")]
    InvalidGraphQlName { name: String },
    #[error("multiple graphql types found with the same name: {graphql_type_name:}")]
    ConflictingGraphQlType { graphql_type_name: ast::TypeName },
}
