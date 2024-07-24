use super::ndc_request;
use crate::{
    error,
    ir::{
        self,
        aggregates::AggregateSelectionSet,
        order_by::OrderByElement,
        selection_set::{NdcFieldName, NdcRelationshipName},
    },
    ndc,
    remote_joins::types::VariableName,
    HttpContext,
};
use async_recursion::async_recursion;
use indexmap::IndexMap;
use open_dds::{
    commands::ProcedureName,
    data_connector::{CollectionName, DataConnectorColumnName},
    relationships::{RelationshipName, RelationshipType},
    types::DataConnectorArgumentName,
};
use std::collections::BTreeMap;
use tracing_util::SpanVisibility;

/// Argument plan to express various kinds of arguments
#[derive(Debug, Clone, PartialEq)]
pub enum Argument<'s> {
    /// The argument is provided as a literal value
    Literal {
        value: serde_json::Value,
    },
    /// The argument is provided by reference to a variable
    Variable {
        name: VariableName,
    },
    BooleanExpression {
        predicate: FilterExpression<'s>,
    },
}

impl<'s> Argument<'s> {
    /// Generate the argument plan from IR argument
    pub fn plan<'a>(
        ir_argument: &'a ir::arguments::Argument<'s>,
        relationships: &mut BTreeMap<NdcRelationshipName, Relationship>,
    ) -> Result<Self, super::error::Error> {
        let planned_argument = match ir_argument {
            ir::arguments::Argument::Literal { value } => Argument::Literal {
                value: value.clone(),
            },
            ir::arguments::Argument::BooleanExpression { predicate } => {
                let expression = super::filter::plan_expression(predicate, relationships)?;
                Argument::BooleanExpression {
                    predicate: expression,
                }
            }
        };
        Ok(planned_argument)
    }

    pub async fn resolve(self, http_context: &HttpContext) -> Result<Self, error::FieldError> {
        match self {
            Argument::Literal { value } => Ok(Argument::Literal { value }),
            Argument::Variable { name } => Ok(Argument::Variable { name }),
            Argument::BooleanExpression { predicate } => {
                let resolved_predicate = predicate.resolve(http_context).await?;
                Ok(Argument::BooleanExpression {
                    predicate: resolved_predicate,
                })
            }
        }
    }
}

/// Argument plan to express various kinds of arguments
#[derive(Debug, Clone, PartialEq)]
pub enum MutationArgument<'s> {
    /// The argument is provided as a literal value
    Literal {
        value: serde_json::Value,
    },
    BooleanExpression {
        predicate: FilterExpression<'s>,
    },
}

impl<'s> MutationArgument<'s> {
    /// Generate the argument plan from IR argument
    pub fn plan<'a>(
        ir_argument: &'a ir::arguments::Argument<'s>,
        relationships: &mut BTreeMap<NdcRelationshipName, Relationship>,
    ) -> Result<Self, super::error::Error> {
        let planned_argument = match ir_argument {
            ir::arguments::Argument::Literal { value } => MutationArgument::Literal {
                value: value.clone(),
            },
            ir::arguments::Argument::BooleanExpression { predicate } => {
                let expression = super::filter::plan_expression(predicate, relationships)?;
                MutationArgument::BooleanExpression {
                    predicate: expression,
                }
            }
        };
        Ok(planned_argument)
    }

    pub async fn resolve(self, http_context: &HttpContext) -> Result<Self, error::FieldError> {
        match self {
            MutationArgument::Literal { value } => Ok(MutationArgument::Literal { value }),
            MutationArgument::BooleanExpression { predicate } => {
                let resolved_predicate = predicate.resolve(http_context).await?;
                Ok(MutationArgument::BooleanExpression {
                    predicate: resolved_predicate,
                })
            }
        }
    }
}

/// Filter expression plan to be resolved
#[derive(Debug, Clone, PartialEq)]
pub enum FilterExpression<'s> {
    And {
        expressions: Vec<FilterExpression<'s>>,
    },
    Or {
        expressions: Vec<FilterExpression<'s>>,
    },
    Not {
        expression: Box<FilterExpression<'s>>,
    },
    LocalFieldComparison(ir::filter::expression::LocalFieldComparison),
    LocalRelationshipComparison {
        relationship: NdcRelationshipName,
        predicate: Box<FilterExpression<'s>>,
    },
    RemoteRelationshipComparison {
        relationship_name: RelationshipName,
        model_name: String,
        ndc_column_mapping: Vec<ir::filter::expression::RelationshipColumnMapping>,
        remote_collection: CollectionName,
        remote_query_node: Box<QueryNode<'s>>,
        collection_relationships: BTreeMap<NdcRelationshipName, Relationship>,
        data_connector: &'s metadata_resolve::DataConnectorLink,
    },
}

impl<'s> FilterExpression<'s> {
    pub fn remove_always_true_expression(self) -> Option<FilterExpression<'s>> {
        match &self {
            FilterExpression::And { expressions } if expressions.is_empty() => None,
            FilterExpression::Not { expression } => match expression.as_ref() {
                FilterExpression::Or { expressions } if expressions.is_empty() => None,
                _ => Some(self),
            },
            _ => Some(self),
        }
    }

    /// Creates a 'FilterExpression::And' and applies some basic expression simplification logic
    /// to remove redundant boolean logic operators
    pub fn mk_and(expressions: Vec<FilterExpression>) -> FilterExpression {
        // If the `and` only contains one expression, we can unwrap it and get rid of the `and`
        // ie. and([x]) == x
        if expressions.len() == 1 {
            expressions.into_iter().next().unwrap()
        }
        // If all subexpressions are also `and`, we can flatten into a single `and`
        // ie. and([and([x,y]), and([a,b])]) == and([x,y,a,b])
        else if expressions
            .iter()
            .all(|expr| matches!(expr, FilterExpression::And { .. }))
        {
            let subexprs = expressions
                .into_iter()
                .flat_map(|expr| match expr {
                    FilterExpression::And { expressions } => expressions,
                    _ => vec![],
                })
                .collect();
            FilterExpression::And {
                expressions: subexprs,
            }
        } else {
            FilterExpression::And { expressions }
        }
    }

    /// Creates a 'FilterExpression::Or' and applies some basic expression simplification logic
    /// to remove redundant boolean logic operators
    pub fn mk_or(expressions: Vec<FilterExpression>) -> FilterExpression {
        // If the `or` only contains one expression, we can unwrap it and get rid of the `or`
        // ie. or([x]) == x
        if expressions.len() == 1 {
            expressions.into_iter().next().unwrap()
        }
        // If all subexpressions are also `or`, we can flatten into a single `or`
        // ie. or([or([x,y]), or([a,b])]) == or([x,y,a,b])
        else if expressions
            .iter()
            .all(|expr| matches!(expr, FilterExpression::Or { .. }))
        {
            let subexprs = expressions
                .into_iter()
                .flat_map(|expr| match expr {
                    FilterExpression::Or { expressions } => expressions,
                    _ => vec![],
                })
                .collect();
            FilterExpression::Or {
                expressions: subexprs,
            }
        } else {
            FilterExpression::Or { expressions }
        }
    }

    /// Creates a 'FilterExpression::Not' and applies some basic expression simplification logic
    /// to remove redundant boolean logic operators
    pub fn mk_not(expression: FilterExpression) -> FilterExpression {
        match expression {
            // Double negations can be removed
            // ie. not(not(x))) == x
            FilterExpression::Not { expression } => *expression,
            _ => FilterExpression::Not {
                expression: Box::new(expression),
            },
        }
    }

    /// Resolve the filter expression plan and generate NDC expression.
    #[async_recursion]
    pub async fn resolve(self, http_context: &HttpContext) -> Result<Self, error::FieldError>
    where
        's: 'async_recursion,
    {
        match self {
            FilterExpression::And { expressions } => {
                let mut resolved_expressions: Vec<FilterExpression> = Vec::new();
                for expression in expressions {
                    let resolved_expression = expression.resolve(http_context).await?;
                    resolved_expressions.push(resolved_expression);
                }
                Ok(FilterExpression::And {
                    expressions: resolved_expressions,
                })
            }
            FilterExpression::Or { expressions } => {
                let mut resolved_expressions = Vec::new();
                for expression in expressions {
                    let resolve_expression = expression.resolve(http_context).await?;
                    resolved_expressions.push(resolve_expression);
                }
                Ok(FilterExpression::Or {
                    expressions: resolved_expressions,
                })
            }
            FilterExpression::Not { expression } => {
                let resolved_expression = expression.resolve(http_context).await?;
                Ok(FilterExpression::Not {
                    expression: Box::new(resolved_expression),
                })
            }
            FilterExpression::LocalFieldComparison(local_field_comparison) => Ok(
                FilterExpression::LocalFieldComparison(local_field_comparison),
            ),
            FilterExpression::LocalRelationshipComparison {
                relationship,
                predicate,
            } => {
                let resolved_expression = predicate.resolve(http_context).await?;
                Ok(FilterExpression::LocalRelationshipComparison {
                    relationship,
                    predicate: Box::new(resolved_expression),
                })
            }
            FilterExpression::RemoteRelationshipComparison {
                ndc_column_mapping,
                remote_collection,
                remote_query_node,
                collection_relationships,
                data_connector,
                relationship_name,
                ..
            } => {
                let tracer = tracing_util::global_tracer();
                tracer.in_span_async(
                    "resolve_remote_relationship_predicate",
                    format!("Resolve remote relationship comparison expression: {relationship_name}"),
                    SpanVisibility::User,
                    || {
                        Box::pin(async {
                            let query_execution_plan = QueryExecutionPlan {
                                query_node: remote_query_node.resolve(http_context).await?,
                                collection: remote_collection,
                                arguments: BTreeMap::new(),
                                collection_relationships,
                                variables: None,
                                data_connector
                            };

                            let ndc_query_request = ndc_request::make_ndc_query_request(query_execution_plan)?;

                            // Generate LHS mapping NDC columns values from the remote data connector
                            // using the RHS NDC columns.
                            let connector_result = ndc::fetch_from_data_connector(
                                http_context,
                                &ndc_query_request,
                                data_connector,
                                None,
                            )
                                .await
                                .map_err(error::FilterPredicateError::RemoteRelationshipNDCRequest)?;

                            // Assume a single row set is returned
                            let single_rowset = crate::process_response::get_single_rowset(
                                connector_result.as_latest_rowsets(),
                            )
                                .map_err(|e| error::FilterPredicateError::NotASingleRowSet(e.to_string()))?;

                            let column_comparison = build_source_column_comparisons(
                                single_rowset.rows.unwrap_or_else(Vec::new), &ndc_column_mapping
                            )?;

                            Ok(column_comparison)
                        })
                    }
                ).await
            }
        }
    }
}

/// Utility to store distinct comparisons to avoid duplicate comparison predicates
/// in the remote relationship comparison expression.
struct DistinctComparisons<'s> {
    comparisons: Vec<FilterExpression<'s>>,
}

impl<'s> DistinctComparisons<'s> {
    fn new() -> Self {
        DistinctComparisons {
            comparisons: Vec::new(),
        }
    }

    fn push(&mut self, expression: FilterExpression<'s>) {
        if !self.comparisons.contains(&expression) {
            self.comparisons.push(expression);
        }
    }
}

/// Build the column comparison expressions using the equal operator NDC response rows.
///
/// [[(a, a_value_1, b, b_value_1), (a, a_value_2, b, b_value_2)]] --->
/// WHERE (a = a_value_1 AND b = b_value_1) OR (a = a_value_2 AND b = b_value_2)
/// The above filter is semantically equivalent to
/// WHERE (a, b) IN ((a_value_1, b_value_1), (a_value_2, b_value_2))
fn build_source_column_comparisons<'s>(
    mut rows: Vec<IndexMap<ndc_models::FieldName, ndc_models::RowFieldValue>>,
    ndc_column_mapping: &[ir::filter::expression::RelationshipColumnMapping],
) -> Result<FilterExpression<'s>, error::FieldError> {
    let mut expressions = DistinctComparisons::new();
    for row in &mut rows {
        let mut column_comparisons = Vec::new();
        for column_mapping in ndc_column_mapping {
            let target_column_field =
                ndc_models::FieldName::from(column_mapping.target_ndc_column.as_str());
            // Fetch RHS (target) column value from the row
            let target_value = row.swap_remove(&target_column_field).ok_or_else(
                || error::FieldInternalError::InternalGeneric{
                        description: format!(
                            "Unable to build remote predicate local comparison. Target field from NDC response not found: {target_column_field}"
                        ),
                }
            )?;

            let ir::filter::expression::SourceNdcColumn {
                column: source_column,
                field_path,
                eq_operator,
            } = &column_mapping.source_ndc_column;
            // Generate LHS (source) column comparison with target column value
            column_comparisons.push(FilterExpression::LocalFieldComparison(
                ir::filter::expression::LocalFieldComparison::BinaryComparison {
                    column: ir::filter::expression::ComparisonTarget::Column {
                        name: source_column.clone(),
                        field_path: field_path.clone(),
                    },
                    operator: eq_operator.clone(),
                    value: ir::filter::expression::ComparisonValue::Scalar {
                        value: target_value.0,
                    },
                },
            ));
        }
        // combine column comparisons from each row with AND
        // Ex. (source_column_a = target_column_value) AND (source_column_b = target_column_value)
        expressions.push(FilterExpression::mk_and(column_comparisons));
    }
    // combine all row comparisons with OR
    // Ex. (source_column_a = target_column_value) AND (source_column_b = target_column_value)
    //     OR (source_column_a = target_column_value) AND (source_column_b = target_column_value)
    Ok(FilterExpression::mk_or(expressions.comparisons))
}

#[derive(Debug, Clone, PartialEq)]
pub struct QueryExecutionPlan<'s> {
    pub query_node: QueryNode<'s>,
    /// The name of a collection
    pub collection: CollectionName,
    /// Values to be provided to any collection arguments
    pub arguments: BTreeMap<DataConnectorArgumentName, Argument<'s>>,
    /// Any relationships between collections involved in the query request
    pub collection_relationships: BTreeMap<NdcRelationshipName, Relationship>,
    /// One set of named variables for each rowset to fetch. Each variable set
    /// should be subtituted in turn, and a fresh set of rows returned.
    pub variables: Option<Vec<BTreeMap<VariableName, serde_json::Value>>>,
    /// The data connector used to fetch the data
    pub data_connector: &'s metadata_resolve::DataConnectorLink,
}

impl<'s> QueryExecutionPlan<'s> {
    pub async fn resolve(self, http_context: &HttpContext) -> Result<Self, error::FieldError> {
        let QueryExecutionPlan {
            query_node,
            collection,
            arguments,
            collection_relationships,
            variables,
            data_connector,
        } = self;
        let query_request = QueryExecutionPlan {
            query_node: query_node.resolve(http_context).await?,
            collection,
            arguments: resolve_arguments(http_context, arguments).await?,
            collection_relationships,
            variables,
            data_connector,
        };
        Ok(query_request)
    }
}

/// Query plan for fetching data
#[derive(Debug, Clone, PartialEq)]
pub struct QueryNode<'s> {
    /// Optionally limit to N results
    pub limit: Option<u32>,
    /// Optionally offset from the Nth result
    pub offset: Option<u32>,
    /// Optionally sort results
    pub order_by: Option<Vec<OrderByElement>>,
    /// Optionally filter results
    pub predicate: Option<FilterExpression<'s>>,
    /// Aggregate fields of the query
    pub aggregates: Option<AggregateSelectionSet<'s>>,
    /// Fields of the query
    pub fields: Option<IndexMap<NdcFieldName, Field<'s>>>,
}

impl<'s> QueryNode<'s> {
    #[async_recursion]
    pub async fn resolve(self, http_context: &HttpContext) -> Result<Self, error::FieldError>
    where
        's: 'async_recursion,
    {
        let QueryNode {
            limit,
            offset,
            order_by,
            predicate,
            aggregates,
            fields,
        } = self;
        let predicate = match predicate {
            Some(predicate) => Some(predicate.resolve(http_context).await?),
            None => None,
        };
        let fields = match fields {
            Some(fields) => {
                let mut ndc_fields_ = IndexMap::new();
                for (name, field) in fields {
                    ndc_fields_.insert(name, field.resolve(http_context).await?);
                }
                Some(ndc_fields_)
            }
            None => None,
        };
        Ok(QueryNode {
            limit,
            offset,
            order_by,
            predicate,
            aggregates,
            fields,
        })
    }
}

/// Field plan
#[derive(Debug, Clone, PartialEq)]
pub enum Field<'s> {
    Column {
        /// Column
        column: DataConnectorColumnName,
        /// Nested fields if column is array or object type
        fields: Option<NestedField<'s>>,
        /// Input field arguments
        arguments: BTreeMap<DataConnectorArgumentName, Argument<'s>>,
    },
    Relationship {
        /// The relationship query
        query_node: Box<QueryNode<'s>>,
        /// The name of the relationship to follow for the subquery
        relationship: NdcRelationshipName,
        /// Values to be provided to any collection arguments
        arguments: BTreeMap<DataConnectorArgumentName, Argument<'s>>,
    },
}

impl<'s> Field<'s> {
    /// Resolve field plan into NDC field
    pub async fn resolve(self, http_context: &HttpContext) -> Result<Self, error::FieldError> {
        match self {
            Field::Column {
                column,
                fields,
                arguments,
            } => {
                let resolved_fields = match fields {
                    Some(nested_field) => Some(nested_field.resolve(http_context).await?),
                    None => None,
                };
                Ok(Field::Column {
                    column,
                    fields: resolved_fields,
                    arguments: resolve_arguments(http_context, arguments).await?,
                })
            }
            Field::Relationship {
                query_node,
                relationship,
                arguments,
            } => {
                let query_node = query_node.resolve(http_context).await?;
                Ok(Field::Relationship {
                    query_node: Box::new(query_node),
                    relationship,
                    arguments: resolve_arguments(http_context, arguments).await?,
                })
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum NestedField<'s> {
    Object(NestedObject<'s>),
    Array(NestedArray<'s>),
}

impl<'s> NestedField<'s> {
    #[async_recursion]
    pub async fn resolve(self, http_context: &HttpContext) -> Result<Self, error::FieldError>
    where
        's: 'async_recursion,
    {
        match self {
            NestedField::Object(nested_object) => Ok(NestedField::Object(
                nested_object.resolve(http_context).await?,
            )),
            NestedField::Array(nested_array) => Ok(NestedField::Array(
                nested_array.resolve(http_context).await?,
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NestedObject<'s> {
    pub fields: IndexMap<NdcFieldName, Field<'s>>,
}

impl<'s> NestedObject<'s> {
    pub async fn resolve(self, http_context: &HttpContext) -> Result<Self, error::FieldError> {
        let mut fields = IndexMap::new();
        for (name, field) in self.fields {
            fields.insert(name, field.resolve(http_context).await?);
        }
        Ok(NestedObject { fields })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NestedArray<'s> {
    pub fields: Box<NestedField<'s>>,
}

impl<'s> NestedArray<'s> {
    pub async fn resolve(self, http_context: &HttpContext) -> Result<Self, error::FieldError> {
        let fields = self.fields.resolve(http_context).await?;
        Ok(NestedArray {
            fields: Box::new(fields),
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Relationship {
    /// A mapping between columns on the source collection to columns on the target collection
    pub column_mapping: BTreeMap<DataConnectorColumnName, DataConnectorColumnName>,
    pub relationship_type: RelationshipType,
    /// The name of a collection
    pub target_collection: CollectionName,
    /// Values to be provided to any collection arguments
    pub arguments: BTreeMap<DataConnectorArgumentName, RelationshipArgument>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RelationshipArgument {
    Column { name: DataConnectorColumnName },
}

#[derive(Debug)]
pub struct MutationExecutionPlan<'s> {
    /// The name of a procedure
    pub procedure_name: ProcedureName,
    /// Any named procedure arguments
    pub procedure_arguments: BTreeMap<DataConnectorArgumentName, MutationArgument<'s>>,
    /// The fields to return from the result, or null to return everything
    pub procedure_fields: Option<NestedField<'s>>,
    /// Any relationships between collections involved in the query request
    pub collection_relationships: BTreeMap<NdcRelationshipName, Relationship>,
    /// The data connector used to fetch the data
    pub data_connector: &'s metadata_resolve::DataConnectorLink,
}

impl<'s> MutationExecutionPlan<'s> {
    pub async fn resolve(
        self,
        http_context: &HttpContext,
    ) -> Result<MutationExecutionPlan<'s>, error::FieldError> {
        let MutationExecutionPlan {
            procedure_name,
            procedure_arguments,
            procedure_fields,
            collection_relationships,
            data_connector,
        } = self;

        let resolved_fields = match procedure_fields {
            Some(fields) => Some(fields.resolve(http_context).await?),
            None => None,
        };

        let resolved_arguments =
            resolve_mutation_arguments(http_context, procedure_arguments).await?;

        Ok(MutationExecutionPlan {
            procedure_name,
            procedure_arguments: resolved_arguments,
            procedure_fields: resolved_fields,
            collection_relationships,
            data_connector,
        })
    }
}

async fn resolve_arguments<'s>(
    http_context: &HttpContext,
    arguments: BTreeMap<DataConnectorArgumentName, Argument<'s>>,
) -> Result<BTreeMap<DataConnectorArgumentName, Argument<'s>>, error::FieldError> {
    let mut result = BTreeMap::new();
    for (argument_name, argument_value) in arguments {
        result.insert(argument_name, argument_value.resolve(http_context).await?);
    }
    Ok(result)
}

async fn resolve_mutation_arguments<'s>(
    http_context: &HttpContext,
    arguments: BTreeMap<DataConnectorArgumentName, MutationArgument<'s>>,
) -> Result<BTreeMap<DataConnectorArgumentName, MutationArgument<'s>>, error::FieldError> {
    let mut result = BTreeMap::new();
    for (argument_name, argument_value) in arguments {
        result.insert(argument_name, argument_value.resolve(http_context).await?);
    }
    Ok(result)
}
