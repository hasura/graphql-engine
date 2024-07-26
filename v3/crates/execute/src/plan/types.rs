use super::ndc_request;
use crate::{
    error,
    ir::{
        self,
        aggregates::AggregateSelectionSet,
        order_by::OrderByElement,
        selection_set::{NdcFieldAlias, NdcRelationshipName},
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
    relationships::RelationshipType,
    types::DataConnectorArgumentName,
};
use std::collections::BTreeMap;
use tracing_util::SpanVisibility;

pub type UnresolvedArgument<'s> = Argument<ir::filter::expression::Expression<'s>>;
pub type ResolvedArgument = Argument<ResolvedFilterExpression>;

/// Argument plan to express various kinds of arguments
#[derive(Debug, Clone, PartialEq)]
pub enum Argument<TFilterExpression> {
    /// The argument is provided as a literal value
    Literal {
        value: serde_json::Value,
    },
    /// The argument is provided by reference to a variable
    Variable {
        name: VariableName,
    },
    BooleanExpression {
        predicate: TFilterExpression,
    },
}

impl<'s> UnresolvedArgument<'s> {
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

    pub async fn resolve(
        self,
        http_context: &HttpContext,
    ) -> Result<ResolvedArgument, error::FieldError> {
        match self {
            Argument::Literal { value } => Ok(Argument::Literal { value }),
            Argument::Variable { name } => Ok(Argument::Variable { name }),
            Argument::BooleanExpression { predicate } => {
                let resolved_predicate = resolve_expression(predicate, http_context).await?;
                Ok(Argument::BooleanExpression {
                    predicate: resolved_predicate,
                })
            }
        }
    }
}

pub type UnresolvedMutationArgument<'s> = MutationArgument<ir::filter::expression::Expression<'s>>;
pub type ResolvedMutationArgument = MutationArgument<ResolvedFilterExpression>;

/// Argument plan to express various kinds of arguments
#[derive(Debug, Clone, PartialEq)]
pub enum MutationArgument<TFilterExpression> {
    /// The argument is provided as a literal value
    Literal {
        value: serde_json::Value,
    },
    BooleanExpression {
        predicate: TFilterExpression,
    },
}

impl<'s> UnresolvedMutationArgument<'s> {
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

    pub async fn resolve(
        self,
        http_context: &HttpContext,
    ) -> Result<ResolvedMutationArgument, error::FieldError> {
        match self {
            MutationArgument::Literal { value } => Ok(MutationArgument::Literal { value }),
            MutationArgument::BooleanExpression { predicate } => {
                let resolved_predicate = resolve_expression(predicate, http_context).await?;
                Ok(MutationArgument::BooleanExpression {
                    predicate: resolved_predicate,
                })
            }
        }
    }
}

/// Filter expression plan to be resolved
#[derive(Debug, Clone, PartialEq)]
pub enum ResolvedFilterExpression {
    And {
        expressions: Vec<ResolvedFilterExpression>,
    },
    Or {
        expressions: Vec<ResolvedFilterExpression>,
    },
    Not {
        expression: Box<ResolvedFilterExpression>,
    },
    LocalFieldComparison(ir::filter::expression::LocalFieldComparison),
    LocalRelationshipComparison {
        relationship: NdcRelationshipName,
        predicate: Box<ResolvedFilterExpression>,
    },
}

impl ResolvedFilterExpression {
    pub fn remove_always_true_expression(self) -> Option<ResolvedFilterExpression> {
        match &self {
            ResolvedFilterExpression::And { expressions } if expressions.is_empty() => None,
            ResolvedFilterExpression::Not { expression } => match expression.as_ref() {
                ResolvedFilterExpression::Or { expressions } if expressions.is_empty() => None,
                _ => Some(self),
            },
            _ => Some(self),
        }
    }

    /// Creates a 'FilterExpression::And' and applies some basic expression simplification logic
    /// to remove redundant boolean logic operators
    pub fn mk_and(expressions: Vec<ResolvedFilterExpression>) -> ResolvedFilterExpression {
        // If the `and` only contains one expression, we can unwrap it and get rid of the `and`
        // ie. and([x]) == x
        if expressions.len() == 1 {
            expressions.into_iter().next().unwrap()
        }
        // If all subexpressions are also `and`, we can flatten into a single `and`
        // ie. and([and([x,y]), and([a,b])]) == and([x,y,a,b])
        else if expressions
            .iter()
            .all(|expr| matches!(expr, ResolvedFilterExpression::And { .. }))
        {
            let subexprs = expressions
                .into_iter()
                .flat_map(|expr| match expr {
                    ResolvedFilterExpression::And { expressions } => expressions,
                    _ => vec![],
                })
                .collect();
            ResolvedFilterExpression::And {
                expressions: subexprs,
            }
        } else {
            ResolvedFilterExpression::And { expressions }
        }
    }

    /// Creates a 'FilterExpression::Or' and applies some basic expression simplification logic
    /// to remove redundant boolean logic operators
    pub fn mk_or(expressions: Vec<ResolvedFilterExpression>) -> ResolvedFilterExpression {
        // If the `or` only contains one expression, we can unwrap it and get rid of the `or`
        // ie. or([x]) == x
        if expressions.len() == 1 {
            expressions.into_iter().next().unwrap()
        }
        // If all subexpressions are also `or`, we can flatten into a single `or`
        // ie. or([or([x,y]), or([a,b])]) == or([x,y,a,b])
        else if expressions
            .iter()
            .all(|expr| matches!(expr, ResolvedFilterExpression::Or { .. }))
        {
            let subexprs = expressions
                .into_iter()
                .flat_map(|expr| match expr {
                    ResolvedFilterExpression::Or { expressions } => expressions,
                    _ => vec![],
                })
                .collect();
            ResolvedFilterExpression::Or {
                expressions: subexprs,
            }
        } else {
            ResolvedFilterExpression::Or { expressions }
        }
    }

    /// Creates a 'FilterExpression::Not' and applies some basic expression simplification logic
    /// to remove redundant boolean logic operators
    pub fn mk_not(expression: ResolvedFilterExpression) -> ResolvedFilterExpression {
        match expression {
            // Double negations can be removed
            // ie. not(not(x))) == x
            ResolvedFilterExpression::Not { expression } => *expression,
            _ => ResolvedFilterExpression::Not {
                expression: Box::new(expression),
            },
        }
    }
}

/// Resolve the filter expression plan and generate NDC expression.
#[async_recursion]
pub async fn resolve_expression<'s>(
    expression: ir::filter::expression::Expression<'s>,
    http_context: &HttpContext,
) -> Result<ResolvedFilterExpression, error::FieldError>
where
    's: 'async_recursion,
{
    match expression {
        ir::filter::expression::Expression::And { expressions } => {
            let mut resolved_expressions: Vec<ResolvedFilterExpression> = Vec::new();
            for subexpression in expressions {
                let resolved_expression = resolve_expression(subexpression, http_context).await?;
                resolved_expressions.push(resolved_expression);
            }
            Ok(ResolvedFilterExpression::And {
                expressions: resolved_expressions,
            })
        }
        ir::filter::expression::Expression::Or { expressions } => {
            let mut resolved_expressions = Vec::new();
            for subexpression in expressions {
                let resolve_expression = resolve_expression(subexpression, http_context).await?;
                resolved_expressions.push(resolve_expression);
            }
            Ok(ResolvedFilterExpression::Or {
                expressions: resolved_expressions,
            })
        }
        ir::filter::expression::Expression::Not {
            expression: subexpression,
        } => {
            let resolved_expression = resolve_expression(*subexpression, http_context).await?;
            Ok(ResolvedFilterExpression::Not {
                expression: Box::new(resolved_expression),
            })
        }
        ir::filter::expression::Expression::LocalField(local_field_comparison) => Ok(
            ResolvedFilterExpression::LocalFieldComparison(local_field_comparison),
        ),
        ir::filter::expression::Expression::LocalRelationship {
            relationship,
            predicate,
            info: _,
        } => {
            let resolved_expression = resolve_expression(*predicate, http_context).await?;
            Ok(ResolvedFilterExpression::LocalRelationshipComparison {
                relationship,
                predicate: Box::new(resolved_expression),
            })
        }
        ir::filter::expression::Expression::RemoteRelationship {
            relationship,
            target_model_name: _,
            target_model_source,
            ndc_column_mapping,
            predicate,
        } => {
            let tracer = tracing_util::global_tracer();
            tracer
                .in_span_async(
                    "resolve_remote_relationship_predicate",
                    format!("Resolve remote relationship comparison expression: {relationship}"),
                    SpanVisibility::User,
                    || {
                        Box::pin(async {
                            let (remote_query_node, collection_relationships) =
                                super::filter::plan_remote_predicate(
                                    &ndc_column_mapping,
                                    &predicate,
                                )
                                .map_err(|e| {
                                    error::FilterPredicateError::RemoteRelationshipPlanError(
                                        Box::new(e),
                                    )
                                })?;

                            let query_execution_plan = QueryExecutionPlan {
                                query_node: remote_query_node.resolve(http_context).await?,
                                collection: target_model_source.collection.clone(),
                                arguments: BTreeMap::new(),
                                collection_relationships,
                                variables: None,
                                data_connector: &target_model_source.data_connector,
                            };

                            let ndc_query_request =
                                ndc_request::make_ndc_query_request(query_execution_plan)?;

                            // Generate LHS mapping NDC columns values from the remote data connector
                            // using the RHS NDC columns.
                            let connector_result = ndc::fetch_from_data_connector(
                                http_context,
                                &ndc_query_request,
                                &target_model_source.data_connector,
                                None,
                            )
                            .await
                            .map_err(error::FilterPredicateError::RemoteRelationshipNDCRequest)?;

                            // Assume a single row set is returned
                            let single_rowset = crate::process_response::get_single_rowset(
                                connector_result.as_latest_rowsets(),
                            )
                            .map_err(|e| {
                                error::FilterPredicateError::NotASingleRowSet(e.to_string())
                            })?;

                            let column_comparison = build_source_column_comparisons(
                                single_rowset.rows.unwrap_or_else(Vec::new),
                                &ndc_column_mapping,
                            )?;

                            Ok(column_comparison)
                        })
                    },
                )
                .await
        }
    }
}

/// Utility to store distinct comparisons to avoid duplicate comparison predicates
/// in the remote relationship comparison expression.
struct DistinctComparisons {
    comparisons: Vec<ResolvedFilterExpression>,
}

impl DistinctComparisons {
    fn new() -> Self {
        DistinctComparisons {
            comparisons: Vec::new(),
        }
    }

    fn push(&mut self, expression: ResolvedFilterExpression) {
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
fn build_source_column_comparisons(
    mut rows: Vec<IndexMap<ndc_models::FieldName, ndc_models::RowFieldValue>>,
    ndc_column_mapping: &[ir::filter::expression::RelationshipColumnMapping],
) -> Result<ResolvedFilterExpression, error::FieldError> {
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
            column_comparisons.push(ResolvedFilterExpression::LocalFieldComparison(
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
        expressions.push(ResolvedFilterExpression::mk_and(column_comparisons));
    }
    // combine all row comparisons with OR
    // Ex. (source_column_a = target_column_value) AND (source_column_b = target_column_value)
    //     OR (source_column_a = target_column_value) AND (source_column_b = target_column_value)
    Ok(ResolvedFilterExpression::mk_or(expressions.comparisons))
}

pub type UnresolvedQueryExecutionPlan<'s> =
    QueryExecutionPlan<'s, ir::filter::expression::Expression<'s>>;
pub type ResolvedQueryExecutionPlan<'s> = QueryExecutionPlan<'s, ResolvedFilterExpression>;

#[derive(Debug, Clone, PartialEq)]
pub struct QueryExecutionPlan<'s, TFilterExpression> {
    pub query_node: QueryNode<'s, TFilterExpression>,
    /// The name of a collection
    pub collection: CollectionName,
    /// Values to be provided to any collection arguments
    pub arguments: BTreeMap<DataConnectorArgumentName, Argument<TFilterExpression>>,
    /// Any relationships between collections involved in the query request
    pub collection_relationships: BTreeMap<NdcRelationshipName, Relationship>,
    /// One set of named variables for each rowset to fetch. Each variable set
    /// should be subtituted in turn, and a fresh set of rows returned.
    pub variables: Option<Vec<BTreeMap<VariableName, serde_json::Value>>>,
    /// The data connector used to fetch the data
    pub data_connector: &'s metadata_resolve::DataConnectorLink,
}

impl<'s> UnresolvedQueryExecutionPlan<'s> {
    pub async fn resolve(
        self,
        http_context: &'s HttpContext,
    ) -> Result<ResolvedQueryExecutionPlan<'s>, error::FieldError> {
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

pub type UnresolvedQueryNode<'s> = QueryNode<'s, ir::filter::expression::Expression<'s>>;
pub type ResolvedQueryNode<'s> = QueryNode<'s, ResolvedFilterExpression>;

/// Query plan for fetching data
#[derive(Debug, Clone, PartialEq)]
pub struct QueryNode<'s, TFilterExpression> {
    /// Optionally limit to N results
    pub limit: Option<u32>,
    /// Optionally offset from the Nth result
    pub offset: Option<u32>,
    /// Optionally sort results
    pub order_by: Option<Vec<OrderByElement>>,
    /// Optionally filter results
    pub predicate: Option<TFilterExpression>,
    /// Aggregate fields of the query
    pub aggregates: Option<AggregateSelectionSet<'s>>,
    /// Fields of the query
    pub fields: Option<IndexMap<NdcFieldAlias, Field<'s, TFilterExpression>>>,
}

impl<'s> UnresolvedQueryNode<'s> {
    #[async_recursion]
    pub async fn resolve(
        self,
        http_context: &'s HttpContext,
    ) -> Result<ResolvedQueryNode<'s>, error::FieldError>
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
            Some(predicate) => Some(resolve_expression(predicate, http_context).await?),
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

pub type UnresolvedField<'s> = Field<'s, ir::filter::expression::Expression<'s>>;
pub type ResolvedField<'s> = Field<'s, ResolvedFilterExpression>;

/// Field plan
#[derive(Debug, Clone, PartialEq)]
pub enum Field<'s, TFilterExpression> {
    Column {
        /// Column
        column: DataConnectorColumnName,
        /// Nested fields if column is array or object type
        fields: Option<NestedField<'s, TFilterExpression>>,
        /// Input field arguments
        arguments: BTreeMap<DataConnectorArgumentName, Argument<TFilterExpression>>,
    },
    Relationship {
        /// The relationship query
        query_node: Box<QueryNode<'s, TFilterExpression>>,
        /// The name of the relationship to follow for the subquery
        relationship: NdcRelationshipName,
        /// Values to be provided to any collection arguments
        arguments: BTreeMap<DataConnectorArgumentName, Argument<TFilterExpression>>,
    },
}

impl<'s> UnresolvedField<'s> {
    /// Resolve field plan into NDC field
    pub async fn resolve(
        self,
        http_context: &'s HttpContext,
    ) -> Result<ResolvedField<'s>, error::FieldError> {
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

pub type UnresolvedNestedField<'s> = NestedField<'s, ir::filter::expression::Expression<'s>>;
pub type ResolvedNestedField<'s> = NestedField<'s, ResolvedFilterExpression>;

#[derive(Debug, Clone, PartialEq)]
pub enum NestedField<'s, TFilterExpression> {
    Object(NestedObject<'s, TFilterExpression>),
    Array(NestedArray<'s, TFilterExpression>),
}

impl<'s> UnresolvedNestedField<'s> {
    #[async_recursion]
    pub async fn resolve(
        self,
        http_context: &'s HttpContext,
    ) -> Result<ResolvedNestedField<'s>, error::FieldError>
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

pub type UnresolvedNestedObject<'s> = NestedObject<'s, ir::filter::expression::Expression<'s>>;
pub type ResolvedNestedObject<'s> = NestedObject<'s, ResolvedFilterExpression>;

#[derive(Debug, Clone, PartialEq)]
pub struct NestedObject<'s, TFilterExpression> {
    pub fields: IndexMap<NdcFieldAlias, Field<'s, TFilterExpression>>,
}

impl<'s> UnresolvedNestedObject<'s> {
    pub async fn resolve(
        self,
        http_context: &'s HttpContext,
    ) -> Result<ResolvedNestedObject<'s>, error::FieldError> {
        let mut fields = IndexMap::new();
        for (name, field) in self.fields {
            fields.insert(name, field.resolve(http_context).await?);
        }
        Ok(NestedObject { fields })
    }
}

pub type UnresolvedNestedArray<'s> = NestedArray<'s, ir::filter::expression::Expression<'s>>;
pub type ResolvedNestedArray<'s> = NestedArray<'s, ResolvedFilterExpression>;

#[derive(Debug, Clone, PartialEq)]
pub struct NestedArray<'s, TFilterExpression> {
    pub fields: Box<NestedField<'s, TFilterExpression>>,
}

impl<'s> UnresolvedNestedArray<'s> {
    pub async fn resolve(
        self,
        http_context: &'s HttpContext,
    ) -> Result<ResolvedNestedArray<'s>, error::FieldError> {
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

pub type UnresolvedMutationExecutionPlan<'s> =
    MutationExecutionPlan<'s, ir::filter::expression::Expression<'s>>;
pub type ResolvedMutationExecutionPlan<'s> = MutationExecutionPlan<'s, ResolvedFilterExpression>;

#[derive(Debug)]
pub struct MutationExecutionPlan<'s, TFilterExpression> {
    /// The name of a procedure
    pub procedure_name: ProcedureName,
    /// Any named procedure arguments
    pub procedure_arguments:
        BTreeMap<DataConnectorArgumentName, MutationArgument<TFilterExpression>>,
    /// The fields to return from the result, or null to return everything
    pub procedure_fields: Option<NestedField<'s, TFilterExpression>>,
    /// Any relationships between collections involved in the query request
    pub collection_relationships: BTreeMap<NdcRelationshipName, Relationship>,
    /// The data connector used to fetch the data
    pub data_connector: &'s metadata_resolve::DataConnectorLink,
}

impl<'s> UnresolvedMutationExecutionPlan<'s> {
    pub async fn resolve(
        self,
        http_context: &'s HttpContext,
    ) -> Result<ResolvedMutationExecutionPlan<'s>, error::FieldError> {
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
    arguments: BTreeMap<
        DataConnectorArgumentName,
        Argument<ir::filter::expression::Expression<'s>>,
    >,
) -> Result<
    BTreeMap<DataConnectorArgumentName, Argument<ResolvedFilterExpression>>,
    error::FieldError,
> {
    let mut result = BTreeMap::new();
    for (argument_name, argument_value) in arguments {
        result.insert(argument_name, argument_value.resolve(http_context).await?);
    }
    Ok(result)
}

async fn resolve_mutation_arguments<'s>(
    http_context: &HttpContext,
    arguments: BTreeMap<
        DataConnectorArgumentName,
        MutationArgument<ir::filter::expression::Expression<'s>>,
    >,
) -> Result<
    BTreeMap<DataConnectorArgumentName, MutationArgument<ResolvedFilterExpression>>,
    error::FieldError,
> {
    let mut result = BTreeMap::new();
    for (argument_name, argument_value) in arguments {
        result.insert(argument_name, argument_value.resolve(http_context).await?);
    }
    Ok(result)
}
