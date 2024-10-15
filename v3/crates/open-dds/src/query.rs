use indexmap::IndexMap;

use serde::{Deserialize, Serialize};

use crate::{
    aggregates::AggregationFunctionName,
    arguments::ArgumentName,
    commands::CommandName,
    identifier::{Identifier, SubgraphName},
    models::{ModelName, OrderByDirection},
    relationships::RelationshipName,
    str_newtype,
    types::{FieldName, OperatorName},
};

str_newtype!(Alias over Identifier | doc "Alias to refer to a particular query or selection in the response.");

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
#[serde(tag = "version")]
/// Representation of a set of data queries on an OpenDD graph
pub enum QueryRequest {
    #[serde(rename = "v1")]
    V1(QueryRequestV1),
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct QueryRequestV1 {
    /// Queries to execute. Each query has an alias that will be used to identify it in the response.
    pub queries: IndexMap<Alias, Query>,
}

/// Representation of a data query on an OpenDD graph
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub enum Query {
    Model(ModelSelection),
    ModelAggregate(ModelAggregateSelection),
    // ModelGroups(ModelGroupsSelection),
    Command(CommandSelection),
    // CommandAggregate(CommandAggregateSelection),
    // CommandGroups(CommandGroupsSelection),
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
/// Query selecting objects from a model.
pub struct ModelSelection {
    #[serde(flatten)]
    pub target: ModelTarget,
    /// What to select from each retrieved object in the model.
    pub selection: IndexMap<Alias, ObjectSubSelection>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
/// Query selecting metrics aggregated over the objects of a model.
pub struct ModelAggregateSelection {
    #[serde(flatten)]
    pub target: ModelTarget,
    /// What metrics aggregated across the model's objects to retrieve.
    pub selection: IndexMap<Alias, Aggregate>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
/// Query executing a command and if applicable, selecting part of the output.
pub struct CommandSelection {
    #[serde(flatten)]
    pub target: CommandTarget,
    /// If the command result is an object type or an array of object types, what to select from that/those object(s).
    pub selection: Option<IndexMap<Alias, ObjectSubSelection>>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
/// Selection of a part of an OpenDD object.
pub enum ObjectSubSelection {
    Field(ObjectFieldSelection),
    // FieldAggregate(ObjectFieldAggregateSelection),
    // FieldGroups(ObjectFieldGroupsSelection),
    Relationship(RelationshipSelection),
    RelationshipAggregate(RelationshipAggregateSelection),
    // RelationshipGroups(RelationshipGroupsSelection),
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
/// Selection of a field of an OpenDD object type.
pub struct ObjectFieldSelection {
    #[serde(flatten)]
    pub target: ObjectFieldTarget,
    /// If the field has an object type or an array of object types, what to select from that/those object(s).
    pub selection: Option<IndexMap<String, ObjectSubSelection>>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
/// Selection of a relationship on an OpenDD object type.
pub struct RelationshipSelection {
    #[serde(flatten)]
    pub target: RelationshipTarget,
    /// If the relationship output produces an object type or an array of object types, what to select from that/those object(s).
    pub selection: Option<IndexMap<String, ObjectSubSelection>>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
/// Selection of metrics aggregated over related values.
pub struct RelationshipAggregateSelection {
    #[serde(flatten)]
    pub target: RelationshipTarget,
    /// What aggregated metrics to select.
    pub selection: IndexMap<String, Aggregate>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
/// The model lookup to target in a query.
pub struct ModelTarget {
    pub subgraph: SubgraphName,
    pub model_name: ModelName,
    #[serde(default)]
    pub arguments: IndexMap<ArgumentName, Value>,
    pub filter: Option<BooleanExpression>,
    #[serde(default)]
    pub order_by: Vec<OrderByElement>,
    pub limit: Option<usize>,
    pub offset: Option<usize>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
/// The field lookup of an OpenDD Object type to target in a query.
pub struct ObjectFieldTarget {
    pub field_name: FieldName,
    #[serde(default)]
    pub arguments: IndexMap<ArgumentName, Value>,
    // If we support filtering, ordering, limiting on array fields, add those here
    // or consider merging FieldTarget and RelationshipTarget.
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
/// The relationship lookup on an OpenDD Object type to target in a query.
pub struct RelationshipTarget {
    pub relationship_name: RelationshipName,
    #[serde(default)]
    pub arguments: IndexMap<ArgumentName, Value>,

    // The ones bellow apply only to array model relationships
    pub filter: Option<BooleanExpression>,
    #[serde(default)]
    pub order_by: Vec<OrderByElement>,
    pub limit: Option<usize>,
    pub offset: Option<usize>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
/// The command execution to to target in a query.
pub struct CommandTarget {
    pub subgraph: SubgraphName,
    pub command_name: CommandName,
    #[serde(default)]
    pub arguments: IndexMap<ArgumentName, Value>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
/// A value that can be passed for an argument.
pub enum Value {
    BooleanExpression(BooleanExpression),
    Literal(serde_json::Value),
}

impl Value {
    pub fn fmt_for_explain(&self) -> &str {
        match &self {
            Value::BooleanExpression(_) => "{expr}",
            Value::Literal(_) => "{literal}",
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
/// A boolean expression value that can be used for model filters or boolean expression arguments.
pub enum BooleanExpression {
    And(Vec<BooleanExpression>),
    Or(Vec<BooleanExpression>),
    Not(Box<BooleanExpression>),
    IsNull(Operand),
    Comparison {
        operand: Operand,
        operator: OperatorName,
        argument: Box<Value>,
    },
}

impl BooleanExpression {
    pub fn fmt_for_explain(&self) -> String {
        match &self {
            BooleanExpression::And(exprs) => format!(
                "({})",
                exprs
                    .iter()
                    .map(BooleanExpression::fmt_for_explain)
                    .collect::<Vec<_>>()
                    .join(" AND ")
            ),
            BooleanExpression::Or(exprs) => format!(
                "({})",
                exprs
                    .iter()
                    .map(BooleanExpression::fmt_for_explain)
                    .collect::<Vec<_>>()
                    .join(" OR ")
            ),
            BooleanExpression::Not(expr) => format!("(NOT {})", expr.fmt_for_explain()),
            BooleanExpression::IsNull(operand) => format!("{} IS NULL", operand.fmt_for_explain()),
            BooleanExpression::Comparison {
                operand,
                operator,
                argument,
            } => match operator.as_str() {
                "_eq" => format!(
                    "{} = {}",
                    operand.fmt_for_explain(),
                    argument.fmt_for_explain()
                ),
                op => format!(
                    "{}({}, {})",
                    operand.fmt_for_explain(),
                    op,
                    argument.fmt_for_explain()
                ),
            },
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
/// A single ordering condition composed of an ordering key and ordering direction.
pub struct OrderByElement {
    pub operand: Operand,
    pub direction: OrderByDirection,
}

impl OrderByElement {
    pub fn fmt_for_explain(&self) -> String {
        format!(
            "{} {}",
            self.operand.fmt_for_explain(),
            match self.direction {
                OrderByDirection::Asc => "asc",
                OrderByDirection::Desc => "desc",
            }
        )
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
/// An aggregate function to execute.
pub enum AggregationFunction {
    Count {},
    CountDistinct {},
    Custom { name: AggregationFunctionName },
}

/// An aggregate metric computed over a set of values in whose context this is used.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct Aggregate {
    pub function: AggregationFunction,
    // Optional to allow aggregating over "self", instead of a nested field.
    pub operand: Option<Operand>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
/// Within the context of an OpenDD object, which specific key to target for filtering / ordering / aggregating.
pub enum Operand {
    Field(ObjectFieldOperand),
    // FieldAggregate(ObjectFieldAggregateOperand),
    Relationship(RelationshipOperand),
    RelationshipAggregate(RelationshipAggregateOperand),
}

impl Operand {
    pub fn fmt_for_explain(&self) -> String {
        match &self {
            Operand::Field(field) => field.fmt_for_explain(),
            Operand::Relationship(_) => "{rel}".into(),
            Operand::RelationshipAggregate(_) => "{rel_agg}".into(),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
/// Operand targeting a particular OpenDD object field or an operand nested within that field.
pub struct ObjectFieldOperand {
    #[serde(flatten)]
    pub target: Box<ObjectFieldTarget>,
    pub nested: Option<Box<Operand>>,
}

impl ObjectFieldOperand {
    pub fn fmt_for_explain(&self) -> String {
        format!(
            "{}{}",
            self.target.field_name.as_str(),
            match &self.nested {
                None => String::new(),
                Some(nested) => format!(".{}", nested.fmt_for_explain()),
            }
        )
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
/// Operand targeting a particular relationship or an operand nested within that relationship.
pub struct RelationshipOperand {
    #[serde(flatten)]
    target: Box<RelationshipTarget>,
    nested: Option<Box<Operand>>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
/// Operand targeting a metric aggregated over related values of an OpenDD object.
pub struct RelationshipAggregateOperand {
    #[serde(flatten)]
    target: Box<RelationshipTarget>,
    aggregate: Box<Aggregate>,
}

#[cfg(test)]
mod test {
    use super::QueryRequest;

    #[test]
    fn test_deserialize_model_query() {
        /*
        This is the equivalent of
        query {
            authors(
                where: {
                    date_of_birth: { _lt: "19000101" }
                },
                order_by: {
                    articles_aggregate {
                        _count: Desc
                    }
                },
                limit: 10
            ) {
                author_name: name
                articles_aggregate {
                    article_count: _count
                }
            }
        }
        */
        serde_json::from_str::<QueryRequest>(
            r#"{
                "version": "v1",
                "queries": {
                    "authors": {
                        "model": {
                            "subgraph": "default",
                            "modelName": "Authors",
                            "filter": {
                                "comparison": {
                                    "operand": {
                                        "field": {
                                            "fieldName": "date_of_birth"
                                        }
                                    },
                                    "operator": "_lt",
                                    "argument": {
                                        "literal": "19000101"
                                    }
                                }
                            },
                            "orderBy": [
                                {
                                    "operand": {
                                        "relationshipAggregate": {
                                            "relationshipName": "articles",
                                            "aggregate": {
                                                "function": {
                                                    "count": {}
                                                }
                                            }
                                        }
                                    },
                                    "direction": "Desc"
                                }
                            ],
                            "limit": 10,
                            "selection": {
                                "author_name": {
                                    "field": {
                                        "fieldName": "name"
                                    }
                                },
                                "articles_aggregate": {
                                    "relationshipAggregate": {
                                        "relationshipName": "articles",
                                        "selection": {
                                            "article_count": {
                                                "function": {
                                                    "count": {}
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }"#,
        )
        .unwrap();
    }
}
