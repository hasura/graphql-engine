use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{
    Spanned,
    aggregates::AggregateExpressionName,
    arguments::ArgumentDefinition,
    commands::ArgumentMapping,
    data_connector::{CollectionName, DataConnectorName},
    identifier::Identifier,
    order_by_expression::OrderByExpressionName,
    str_newtype,
    traits::{OpenDd, OpenDdDeserializeError},
    types::{CustomTypeName, Deprecated, FieldName, GraphQlFieldName, GraphQlTypeName},
};

str_newtype!(ModelName over Identifier | doc "The name of data model.");

/// The definition of a data model.
/// A data model is a collection of objects of a particular type. Models can support one or more CRUD operations.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[opendd(
    as_versioned_with_definition,
    json_schema(title = "Model", example = "Model::example")
)]
pub enum Model {
    V1(ModelV1),
    V2(ModelV2),
}

impl Model {
    fn example() -> serde_json::Value {
        serde_json::json!({
          "kind": "Model",
          "version": "v1",
          "definition": {
              "name": "Articles",
              "objectType": "article",
              "globalIdSource": true,
              "arguments": [],
              "source": {
                "dataConnectorName": "data_connector",
                "collection": "articles",
                "argumentMapping": {}
              },
              "filterExpressionType": "Article_bool_exp",
              "orderableFields": [
                {
                  "fieldName": "article_id",
                  "orderByDirections": {
                    "enableAll": true
                  }
                },
                {
                  "fieldName": "title",
                  "orderByDirections": {
                    "enableAll": true
                  }
                },
                {
                  "fieldName": "author_id",
                  "orderByDirections": {
                    "enableAll": true
                  }
                }
              ],
              "graphql": {
                  "selectUniques": [
                      {
                          "queryRootField": "ArticleByID",
                          "uniqueIdentifier": [
                              "article_id"
                          ],
                          "description": "Description for the select unique ArticleByID"
                      }
                  ],
                  "selectMany": {
                      "queryRootField": "ArticleMany",
                      "description": "Description for the select many ArticleMany"
                  },
                  "orderByExpressionType": "Article_Order_By",
                  "apolloFederation": {
                    "entitySource": true
                  }
              },
              "description": "Description for the model Articles"
          }
        })
    }

    // Upgrading from ModelV1 to ModelV2 requires creating a new OrderByExpression
    // We don't want to confuse the user by generating the new types and then validating against them
    // instead of the original types.
    // Therefore, we defer the upgrade until the metadata resolve step.
    pub fn upgrade(self) -> Model {
        self
    }

    pub fn name(&self) -> &ModelName {
        match self {
            Model::V1(v1) => &v1.name,
            Model::V2(v2) => &v2.name,
        }
    }

    pub fn object_type(&self) -> &Spanned<CustomTypeName> {
        match self {
            Model::V1(v1) => &v1.object_type,
            Model::V2(v2) => &v2.object_type,
        }
    }

    pub fn global_id_source(&self) -> bool {
        match self {
            Model::V1(v1) => v1.global_id_source,
            Model::V2(v2) => v2.global_id_source,
        }
    }

    pub fn arguments(&self) -> &Vec<ArgumentDefinition> {
        match self {
            Model::V1(v1) => &v1.arguments,
            Model::V2(v2) => &v2.arguments,
        }
    }

    pub fn source(&self) -> &Option<ModelSource> {
        match self {
            Model::V1(v1) => &v1.source,
            Model::V2(v2) => &v2.source,
        }
    }

    pub fn filter_expression_type(&self) -> &Option<CustomTypeName> {
        match self {
            Model::V1(v1) => &v1.filter_expression_type,
            Model::V2(v2) => &v2.filter_expression_type,
        }
    }

    pub fn aggregate_expression(&self) -> &Option<AggregateExpressionName> {
        match self {
            Model::V1(v1) => &v1.aggregate_expression,
            Model::V2(v2) => &v2.aggregate_expression,
        }
    }

    pub fn graphql(&self) -> Option<ModelGraphQlDefinitionV2> {
        match self {
            Model::V1(v1) => v1.graphql.clone().map(ModelGraphQlDefinition::upgrade),
            Model::V2(v2) => v2.graphql.clone(),
        }
    }

    pub fn description(&self) -> &Option<String> {
        match self {
            Model::V1(v1) => &v1.description,
            Model::V2(v2) => &v2.description,
        }
    }
}

#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "ModelV1"))]
/// The definition of a data model.
/// A data model is a collection of objects of a particular type. Models can support one or more CRUD operations.
pub struct ModelV1 {
    /// The name of the data model.
    pub name: ModelName,
    /// The type of the objects of which this model is a collection.
    pub object_type: Spanned<CustomTypeName>,
    /// Whether this model should be used as the global ID source for all objects of its type.
    #[opendd(default)]
    pub global_id_source: bool,
    /// A list of arguments accepted by this model. Defaults to no arguments.
    #[opendd(default, json_schema(default_exp = "serde_json::json!([])"))]
    pub arguments: Vec<ArgumentDefinition>,
    /// The source configuration for this model.
    pub source: Option<ModelSource>,
    /// The boolean expression type that should be used to perform filtering on this model.
    pub filter_expression_type: Option<CustomTypeName>,
    /// A list of fields that can be used to order the objects in this model.
    pub orderable_fields: Vec<OrderableField>,
    /// The name of the AggregateExpression that defines how to aggregate over this model
    pub aggregate_expression: Option<AggregateExpressionName>,
    /// Configuration for how this model should appear in the GraphQL schema.
    pub graphql: Option<ModelGraphQlDefinition>,
    /// The description of the model.
    /// Gets added to the description of the model in the graphql schema.
    pub description: Option<String>,
}

#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "ModelV2"))]
/// The definition of a data model.
/// A data model is a collection of objects of a particular type. Models can support one or more CRUD operations.
/// ModelV2 implements the changes described in rfcs/open-dd-expression-type-changes.md.
pub struct ModelV2 {
    /// The name of the data model.
    pub name: ModelName,
    /// The type of the objects of which this model is a collection.
    pub object_type: Spanned<CustomTypeName>,
    /// Whether this model should be used as the global ID source for all objects of its type.
    #[opendd(default)]
    pub global_id_source: bool,
    /// A list of arguments accepted by this model. Defaults to no arguments.
    #[opendd(default, json_schema(default_exp = "serde_json::json!([])"))]
    pub arguments: Vec<ArgumentDefinition>,
    /// The source configuration for this model.
    pub source: Option<ModelSource>,
    /// The boolean expression type that should be used to perform filtering on this model.
    pub filter_expression_type: Option<CustomTypeName>,
    /// The order by expression to use for this model.
    pub order_by_expression: Option<OrderByExpressionName>,
    /// The name of the AggregateExpression that defines how to aggregate over this model
    pub aggregate_expression: Option<AggregateExpressionName>,
    /// Configuration for how this model should appear in the GraphQL schema.
    pub graphql: Option<ModelGraphQlDefinitionV2>,
    /// The description of the model.
    /// Gets added to the description of the model in the graphql schema.
    pub description: Option<String>,
}

#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "ModelSource", example = "ModelSource::example"))]
/// Description of how a model maps to a particular data connector
pub struct ModelSource {
    /// The name of the data connector backing this model.
    pub data_connector_name: Spanned<DataConnectorName>,

    /// The collection in the data connector that backs this model.
    pub collection: Spanned<CollectionName>,

    /// Mapping from model argument names to data connector collection argument names.
    #[opendd(default)]
    pub argument_mapping: ArgumentMapping,
}

impl ModelSource {
    fn example() -> serde_json::Value {
        serde_json::json!(
            {
              "dataConnectorName": "data_connector",
              "collection": "articles"
            }
        )
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(
    title = "ModelGraphQlDefinition",
    example = "ModelGraphQlDefinition::example"
))]
/// The definition of how a model appears in the GraphQL API.
pub struct ModelGraphQlDefinition {
    /// For each select unique defined here, a query root field is added to the GraphQL API that
    /// can be used to select a unique object from the model.
    pub select_uniques: Vec<SelectUniqueGraphQlDefinition>,
    /// Select many configuration for a model adds a query root field to the GraphQl API that
    /// can be used to retrieve multiple objects from the model.
    pub select_many: Option<SelectManyGraphQlDefinition>,
    /// The type name of the input type used to hold the arguments of the model.
    pub arguments_input_type: Option<GraphQlTypeName>,
    /// The type name of the order by expression input type.
    pub order_by_expression_type: Option<GraphQlTypeName>,
    /// Apollo Federation configuration
    pub apollo_federation: Option<ModelApolloFederationConfiguration>,
    /// The type name of the input type used to hold the filtering settings used by
    /// aggregates (etc) to filter their input before processing
    pub filter_input_type_name: Option<GraphQlTypeName>,
    /// Configures the query root field added to the GraphQL API that can be used to
    /// aggregate over the model
    pub aggregate: Option<ModelAggregateGraphQlDefinition>,
}

impl ModelGraphQlDefinition {
    fn example() -> serde_json::Value {
        serde_json::json!({
            "selectUniques": [
                {
                    "queryRootField": "ArticleByID",
                    "uniqueIdentifier": [
                        "article_id"
                    ],
                    "description": "Description for the select unique ArticleByID"
                }
            ],
            "selectMany": {
                "queryRootField": "ArticleMany",
                "description": "Description for the select many ArticleMany"
            },
            "orderByExpressionType": "Article_Order_By",
            "aggregate": {
                "queryRootField": "ArticleAggregate",
                "description": "Aggregate over Articles"
            }
        })
    }

    pub fn upgrade(self) -> ModelGraphQlDefinitionV2 {
        ModelGraphQlDefinitionV2 {
            select_uniques: self.select_uniques,
            select_many: self.select_many,
            arguments_input_type: self.arguments_input_type,
            apollo_federation: self.apollo_federation,
            filter_input_type_name: self.filter_input_type_name,
            aggregate: self.aggregate,
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(
    title = "ModelGraphQlDefinitionV2",
    example = "ModelGraphQlDefinitionV2::example"
))]
/// The definition of how a model appears in the GraphQL API.
/// Note: ModelGraphQlDefinitionV2 removed the `order_by_expression_type` property.
/// See rfcs/open-dd-expression-type-changes.md.
pub struct ModelGraphQlDefinitionV2 {
    /// For each select unique defined here, a query root field is added to the GraphQL API that
    /// can be used to select a unique object from the model.
    pub select_uniques: Vec<SelectUniqueGraphQlDefinition>,
    /// Select many configuration for a model adds a query root field to the GraphQl API that
    /// can be used to retrieve multiple objects from the model.
    pub select_many: Option<SelectManyGraphQlDefinition>,
    /// The type name of the input type used to hold the arguments of the model.
    pub arguments_input_type: Option<GraphQlTypeName>,
    /// Apollo Federation configuration
    pub apollo_federation: Option<ModelApolloFederationConfiguration>,
    /// The type name of the input type used to hold the filtering settings used by
    /// aggregates (etc) to filter their input before processing
    pub filter_input_type_name: Option<GraphQlTypeName>,
    /// Configures the query root field added to the GraphQL API that can be used to
    /// aggregate over the model
    pub aggregate: Option<ModelAggregateGraphQlDefinition>,
}

impl ModelGraphQlDefinitionV2 {
    fn example() -> serde_json::Value {
        serde_json::json!({
            "selectUniques": [
                {
                    "queryRootField": "ArticleByID",
                    "uniqueIdentifier": [
                        "article_id"
                    ],
                    "description": "Description for the select unique ArticleByID"
                }
            ],
            "selectMany": {
                "queryRootField": "ArticleMany",
                "description": "Description for the select many ArticleMany"
            },
            "aggregate": {
                "queryRootField": "ArticleAggregate",
                "description": "Aggregate over Articles"
            }
        })
    }
}

/// The definition of the GraphQL API for selecting a unique row/object from a model.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "SelectUniqueGraphQlDefinition"))]
pub struct SelectUniqueGraphQlDefinition {
    /// The name of the query root field for this API.
    pub query_root_field: GraphQlFieldName,
    /// A set of fields which can uniquely identify a row/object in the model.
    pub unique_identifier: Vec<FieldName>,
    /// The description of the select unique graphql definition of the model.
    /// Gets added to the description of the select unique root field of the model in the graphql schema.
    pub description: Option<String>,
    /// Whether this select unique query field is deprecated.
    /// If set, the deprecation status is added to the select unique root field's graphql schema.
    pub deprecated: Option<Deprecated>,
    /// Enable subscription on this select unique root field.
    pub subscription: Option<SubscriptionGraphQlDefinition>,
}

/// The definition of the GraphQL API for selecting rows from a model.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "SelectManyGraphQlDefinition"))]
pub struct SelectManyGraphQlDefinition {
    /// The name of the query root field for this API.
    pub query_root_field: GraphQlFieldName,
    /// The description of the select many graphql definition of the model.
    /// Gets added to the description of the select many root field of the model in the graphql schema.
    pub description: Option<String>,
    /// Whether this select many query field is deprecated.
    /// If set, the deprecation status is added to the select many root field's graphql schema.
    pub deprecated: Option<Deprecated>,
    /// Enable subscription on this select many root field.
    pub subscription: Option<SubscriptionGraphQlDefinition>,
}

/// The definition of the GraphQL API for enabling subscription on query root fields.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "SubscriptionGraphQlDefinition"))]
pub struct SubscriptionGraphQlDefinition {
    /// The name of the subscription root field.
    pub root_field: GraphQlFieldName,
    /// The description of the subscription graphql definition.
    /// Gets added to the description of the subscription root field in the graphql schema.
    pub description: Option<String>,
    /// Whether this subscription root field is deprecated.
    /// If set, the deprecation status is added to the subscription root field's graphql schema.
    pub deprecated: Option<Deprecated>,
    /// Polling interval in milliseconds for the subscription.
    #[opendd(default = 1000)]
    pub polling_interval_ms: u64,
}

/// A field that can be used to order the objects in a model.
#[derive(Serialize, Deserialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(json_schema(title = "OrderableField"))]
pub struct OrderableField {
    pub field_name: FieldName,
    pub order_by_directions: EnableAllOrSpecific<OrderByDirection>,
}

/// Enable all or specific values.
#[derive(Serialize, Deserialize, Clone, Debug, Eq, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[schemars(title = "EnableAllOrSpecific")]
pub enum EnableAllOrSpecific<T> {
    EnableAll(bool),
    EnableSpecific(Vec<T>),
}

impl<'de, T: serde::Deserialize<'de> + JsonSchema> OpenDd for EnableAllOrSpecific<T> {
    fn deserialize(
        json: serde_json::Value,
        _path: jsonpath::JSONPath,
    ) -> Result<Self, OpenDdDeserializeError> {
        serde_path_to_error::deserialize(json).map_err(|e| OpenDdDeserializeError {
            path: jsonpath::JSONPath::from_serde_path(e.path()),
            error: e.into_inner(),
        })
    }

    fn json_schema(generator: &mut schemars::r#gen::SchemaGenerator) -> schemars::schema::Schema {
        <Self as ::schemars::JsonSchema>::json_schema(generator)
    }

    fn _schema_name() -> String {
        <Self as ::schemars::JsonSchema>::schema_name()
    }

    fn _schema_is_referenceable() -> bool {
        <Self as ::schemars::JsonSchema>::is_referenceable()
    }
}

#[derive(
    Serialize, Deserialize, Clone, Debug, Eq, PartialEq, JsonSchema, PartialOrd, Ord, Hash,
)]
#[schemars(title = "OrderByDirection")]
pub enum OrderByDirection {
    Asc,
    Desc,
}

/// Apollo Federation configuration for a model.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(json_schema(title = "ModelApolloFederationConfiguration"))]
pub struct ModelApolloFederationConfiguration {
    /// Whether this model should be used as the source for fetching _entity for object of its type.
    pub entity_source: bool,
}

/// The definition of the GraphQL API for aggregating over a model.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(json_schema(title = "ModelAggregateGraphQlDefinition"))]
pub struct ModelAggregateGraphQlDefinition {
    /// The name of the query root field for this API.
    pub query_root_field: GraphQlFieldName,
    /// The description of the aggregate graphql definition of the model.
    /// Gets added to the description of the aggregate root field of the model in the graphql schema.
    pub description: Option<String>,
    /// Whether this aggregate query field is deprecated.
    /// If set, the deprecation status is added to the aggregate root field's graphql schema.
    pub deprecated: Option<Deprecated>,
    /// Enable subscription on this aggregate root field.
    pub subscription: Option<SubscriptionGraphQlDefinition>,
}
