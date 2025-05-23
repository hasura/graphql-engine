use std::sync::Arc;

use crate::{ArgumentInfo, helpers::types::DuplicateRootFieldError, types::warning::Warning};
use indexmap::IndexMap;
use lang_graphql::ast::common::{self as ast};
use open_dds::{
    aggregates::AggregateExpressionName,
    data_connector::{DataConnectorColumnName, DataConnectorName},
    models::ModelName,
    query::ArgumentName,
    types::{CustomTypeName, Deprecated, FieldName},
};
use serde::{Deserialize, Serialize};

use crate::stages::{boolean_expressions, models, object_types};
use crate::types::error::ShouldBeAnError;
use crate::types::subgraph::{Qualified, QualifiedTypeReference};
use crate::{OrderByExpressionIdentifier, helpers::types::NdcColumnForComparison};

#[derive(Debug)]
pub struct ModelsWithGraphqlOutput {
    pub models_with_graphql: IndexMap<Qualified<ModelName>, ModelWithGraphql>,
    pub issues: Vec<Warning>,
}

/// A Model resolved with regards to it's data source
#[derive(Debug)]
pub(crate) struct ModelWithGraphql {
    pub inner: Model,
    pub filter_expression_type: Option<boolean_expressions::ResolvedObjectBooleanExpressionType>,
    pub graphql_api: ModelGraphQlApi,
    pub arguments: IndexMap<ArgumentName, ArgumentInfo>,
    pub description: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct Model {
    pub path: jsonpath::JSONPath,
    pub name: Qualified<ModelName>,
    pub data_type: Qualified<CustomTypeName>,
    pub type_fields: IndexMap<FieldName, object_types::FieldDefinition>,
    pub global_id_fields: Vec<FieldName>,
    pub source: Option<Arc<models::ModelSource>>, // wrapped in Arc because we include these in our `Plan`
    pub global_id_source: Option<models::NDCFieldSourceMapping>,
    pub apollo_federation_key_source: Option<models::NDCFieldSourceMapping>,
    pub aggregate_expression: Option<Qualified<AggregateExpressionName>>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct UniqueIdentifierField {
    pub field_type: QualifiedTypeReference,
    pub ndc_column: Option<NdcColumnForComparison>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct SelectUniqueGraphQlDefinition {
    pub query_root_field: ast::Name,
    pub unique_identifier: IndexMap<FieldName, UniqueIdentifierField>,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub description: Option<String>,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub deprecated: Option<Deprecated>,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub subscription: Option<SubscriptionGraphQlDefinition>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct SelectManyGraphQlDefinition {
    pub query_root_field: ast::Name,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub description: Option<String>,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub deprecated: Option<Deprecated>,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub subscription: Option<SubscriptionGraphQlDefinition>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct SelectAggregateGraphQlDefinition {
    pub query_root_field: ast::Name,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub description: Option<String>,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub deprecated: Option<Deprecated>,
    pub aggregate_expression_name: Qualified<AggregateExpressionName>,
    pub filter_input_field_name: ast::Name,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub subscription: Option<SubscriptionGraphQlDefinition>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct SubscriptionGraphQlDefinition {
    pub root_field: ast::Name,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub description: Option<String>,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub deprecated: Option<Deprecated>,
    pub polling_interval_ms: u64,
}

// TODO: add support for aggregates
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct OrderByExpressionInfo {
    pub ndc_column: DataConnectorColumnName,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ModelOrderByExpression {
    pub data_connector_name: Qualified<DataConnectorName>,
    pub order_by_type_name: ast::TypeName,
    pub order_by_field_name: ast::Name,
    pub order_by_expression_identifier: Qualified<OrderByExpressionIdentifier>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ModelGraphqlApiArgumentsConfig {
    pub field_name: ast::Name,
    pub type_name: ast::TypeName,
}
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct LimitFieldGraphqlConfig {
    pub field_name: ast::Name,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct OffsetFieldGraphqlConfig {
    pub field_name: ast::Name,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Default)]
pub struct ModelGraphQlApi {
    pub arguments_input_config: Option<ModelGraphqlApiArgumentsConfig>,
    pub select_uniques: Vec<SelectUniqueGraphQlDefinition>,
    pub select_many: Option<SelectManyGraphQlDefinition>,
    pub select_aggregate: Option<SelectAggregateGraphQlDefinition>,
    pub order_by_expression: Option<ModelOrderByExpression>,
    pub limit_field: Option<LimitFieldGraphqlConfig>,
    pub offset_field: Option<OffsetFieldGraphqlConfig>,
    pub filter_input_type_name: Option<ast::TypeName>,
}

#[derive(Debug, thiserror::Error)]
pub enum ModelGraphqlIssue {
    #[error(
        "the model {model_name} has defined a selectAggregate graphql API, but it will not appear in the GraphQL API unless query.aggregate.filterInputFieldName is also configured in GraphqlConfig"
    )]
    MissingAggregateFilterInputFieldNameInGraphqlConfig { model_name: Qualified<ModelName> },

    #[error("the model {model_name} has a duplicate root field in the GraphQL schema: {error:}")]
    DuplicateRootField {
        model_name: Qualified<ModelName>,
        error: DuplicateRootFieldError,
    },

    #[error(
        "model arguments graphql input configuration has been specified for model {model_name:} that does not have arguments"
    )]
    UnnecessaryModelArgumentsGraphQlInputConfiguration { model_name: Qualified<ModelName> },
    #[error(
        "an unnecessary filter input type name graphql configuration has been specified for model {model_name:} that does not use aggregates"
    )]
    UnnecessaryFilterInputTypeNameGraphqlConfiguration { model_name: Qualified<ModelName> },
}

impl ShouldBeAnError for ModelGraphqlIssue {
    fn should_be_an_error(&self, flags: &open_dds::flags::OpenDdFlags) -> bool {
        match self {
            ModelGraphqlIssue::MissingAggregateFilterInputFieldNameInGraphqlConfig { .. }
            | ModelGraphqlIssue::UnnecessaryModelArgumentsGraphQlInputConfiguration { .. }
            | ModelGraphqlIssue::UnnecessaryFilterInputTypeNameGraphqlConfiguration { .. } => false,
            ModelGraphqlIssue::DuplicateRootField { .. } => {
                flags.contains(open_dds::flags::Flag::RequireUniqueModelGraphqlNames)
            }
        }
    }
}
