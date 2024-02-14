use std::collections::HashMap;

use derive_more::Display;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{
    arguments::{ArgumentDefinition, ArgumentName},
    commands::TypeMapping,
    data_connector::DataConnectorName,
    types::{CustomTypeName, FieldName, GraphQlFieldName, GraphQlTypeName},
};

/// The name of data model.
#[derive(
    Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash, derive_more::Display, JsonSchema,
)]
pub struct ModelName(pub String);

/// The definition of a data model.
/// A data model is a collection of objects of a particular type. Models can support one or more CRUD operations.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "Model")]
pub enum Model {
    V1(ModelV1),
}

impl Model {
    pub fn upgrade(self) -> ModelV1 {
        match self {
            Model::V1(v1) => v1,
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "ModelV1")]
#[schemars(example = "ModelV1::example")]
/// The definition of a data model.
/// A data model is a collection of objects of a particular type. Models can support one or more CRUD operations.
pub struct ModelV1 {
    /// The name of the data model.
    pub name: ModelName,
    /// The type of the objects of which this model is a collection.
    pub object_type: CustomTypeName,
    #[serde(default)]
    /// Whether this model should be used as the global ID source for all objects of its type.
    pub global_id_source: bool,
    #[serde(default)]
    /// A list of arguments accepted by this model. Defaults to no arguments.
    pub arguments: Vec<ArgumentDefinition>,
    /// The source configuration for this model.
    pub source: Option<ModelSource>,
    /// A list of fields that can be used to filter the objects in this model.
    pub filterable_fields: Vec<FilterableField>,
    /// A list of fields that can be used to order the objects in this model.
    pub orderable_fields: Vec<OrderableField>,
    /// Configuration for how this model should appear in the GraphQL schema.
    pub graphql: Option<ModelGraphQlDefinition>,
    /// The description of the model.
    /// Gets added to the description of the model in the graphql schema.
    pub description: Option<String>,
}

impl ModelV1 {
    fn example() -> Self {
        serde_json::from_str(
            r#"
            {
              "name": "Articles",
              "description": "Description for the model Articles",
              "objectType": "article",
              "globalIdSource": true,
              "source": {
                "dataConnectorName": "data_connector",
                "collection": "articles",
                "typeMapping": {
                  "article": {
                    "fieldMapping": {
                      "article_id": {
                        "column": "id"
                      },
                      "title": {
                        "column": "title"
                      },
                      "author_id": {
                        "column": "author_id"
                      }
                    }
                  }
                }
              },
              "filterableFields": [
                {
                  "fieldName": "article_id",
                  "operators": {
                    "enableAll": true
                  }
                },
                {
                  "fieldName": "title",
                  "operators": {
                    "enableAll": true
                  }
                },
                {
                  "fieldName": "author_id",
                  "operators": {
                    "enableAll": true
                  }
                }
              ],
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
                "filterExpressionType": "Article_Where_Exp",
                "orderByExpressionType": "Article_Order_By"
              }
            }
        "#,
        )
        .unwrap()
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "ModelSource")]
#[schemars(example = "ModelSource::example")]
/// Description of how a model maps to a particular data connector
pub struct ModelSource {
    /// The name of the data connector backing this model.
    pub data_connector_name: DataConnectorName,

    /// The collection in the data connector that backs this model.
    pub collection: String,

    /// How the various types used in this model correspond to
    /// entities in the data connector.
    #[serde(default)]
    pub type_mapping: HashMap<CustomTypeName, TypeMapping>,

    // Mapping from model argument names to data connector table argument names.
    #[serde(default)]
    pub argument_mapping: HashMap<ArgumentName, String>,
}

impl ModelSource {
    fn example() -> Self {
        serde_json::from_str(
            r#"
            {
              "dataConnectorName": "data_connector",
              "collection": "articles",
              "typeMapping": {
                "article": {
                  "fieldMapping": {
                    "article_id": {
                      "column": "id"
                    },
                    "title": {
                      "column": "title"
                    },
                    "author_id": {
                      "column": "author_id"
                    }
                  }
                }
              }
            }
        "#,
        )
        .unwrap()
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "ModelGraphQlDefinition")]
#[schemars(example = "ModelGraphQlDefinition::example")]
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
    /// The type name of the filter boolean expression input type.
    pub filter_expression_type: Option<GraphQlTypeName>,
    /// The type name of the order by expression input type.
    pub order_by_expression_type: Option<GraphQlTypeName>,
}

impl ModelGraphQlDefinition {
    fn example() -> Self {
        serde_json::from_str(
            r#"
            {
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
              "filterExpressionType": "Article_Where_Exp",
              "orderByExpressionType": "Article_Order_By"
            }
        "#,
        )
        .unwrap()
    }
}

/// The definition of the GraphQL API for selecting a unique row/object from a model.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "SelectUniqueGraphQlDefinition")]
pub struct SelectUniqueGraphQlDefinition {
    /// The name of the query root field for this API.
    pub query_root_field: GraphQlFieldName,
    /// A set of fields which can uniquely identify a row/object in the model.
    pub unique_identifier: Vec<FieldName>,
    /// The description of the select unique graphql definition of the model.
    /// Gets added to the description of the select unique root field of the model in the graphql schema.
    pub description: Option<String>,
}

/// The definition of the GraphQL API for selecting rows from a model.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "SelectManyGraphQlDefinition")]
pub struct SelectManyGraphQlDefinition {
    /// The name of the query root field for this API.
    pub query_root_field: GraphQlFieldName,
    /// The description of the select many graphql definition of the model.
    /// Gets added to the description of the select many root field of the model in the graphql schema.
    pub description: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, Debug, Eq, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "FilterableField")]
pub struct FilterableField {
    pub field_name: FieldName,
    pub operators: EnableAllOrSpecific<OperatorName>,
}

#[derive(Serialize, Deserialize, Clone, Debug, Eq, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "OrderableField")]
pub struct OrderableField {
    pub field_name: FieldName,
    pub order_by_directions: EnableAllOrSpecific<OrderByDirection>,
}

#[derive(Serialize, Deserialize, Clone, Debug, Eq, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[schemars(title = "EnableAllOrSpecific")]
pub enum EnableAllOrSpecific<T> {
    EnableAll(bool),
    EnableSpecific(Vec<T>),
}

#[derive(
    Serialize, Deserialize, Clone, Debug, Eq, PartialEq, JsonSchema, PartialOrd, Ord, Hash,
)]
#[schemars(title = "OrderByDirection")]
pub enum OrderByDirection {
    Asc,
    Desc,
}

#[derive(
    Display, Serialize, Deserialize, Clone, Debug, Eq, PartialEq, JsonSchema, PartialOrd, Ord, Hash,
)]
pub struct OperatorName(pub String);
