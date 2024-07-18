use super::ndc_request;
use crate::{error, ir, ndc, HttpContext};
use async_recursion::async_recursion;
use indexmap::IndexMap;
use metadata_resolve::data_connectors::NdcVersion;
use open_dds::relationships::RelationshipName;
use std::collections::BTreeMap;
use tracing_util::SpanVisibility;

/// Argument plan to express various kinds of arguments
#[derive(Debug, Clone, PartialEq)]
pub enum Argument<'s> {
    /// The argument is provided as a literal value
    Literal { value: serde_json::Value },
    /// The argument is provided by reference to a variable
    Variable { name: ndc_models::VariableName },
    BooleanExpression {
        ndc_version: NdcVersion,
        predicate: FilterExpression<'s>,
    },
}

impl<'s> Argument<'s> {
    /// Generate the argument plan from IR argument
    pub fn plan<'a>(
        ir_argument: &'a ir::arguments::Argument<'s>,
        ndc_version: NdcVersion,
        relationships: &'a mut BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
    ) -> Result<Self, super::error::Error> {
        let planned_argument = match ir_argument {
            ir::arguments::Argument::Literal { value } => Argument::Literal {
                value: value.clone(),
            },
            ir::arguments::Argument::BooleanExpression { predicate } => {
                let expression = super::filter::plan_expression(predicate, relationships)?;
                Argument::BooleanExpression {
                    predicate: expression,
                    ndc_version,
                }
            }
        };
        Ok(planned_argument)
    }

    /// Resolve the argument plan into NDC argument
    pub async fn resolve(
        self,
        http_context: &HttpContext,
    ) -> Result<ndc_models::Argument, error::FieldError> {
        match self {
            Argument::Literal { value } => Ok(ndc_models::Argument::Literal { value }),
            Argument::Variable { name } => Ok(ndc_models::Argument::Variable { name }),
            Argument::BooleanExpression {
                predicate,
                ndc_version,
            } => {
                let ndc_predicate = predicate.resolve(http_context).await?;
                let value = match ndc_version {
                    NdcVersion::V01 => {
                        // Downgrade the NDC expression to V01
                        let v01_expression =
                            ndc::migration::v01::downgrade_v02_expression(ndc_predicate)
                                .map_err(error::FieldInternalError::NdcRequestDowngradeError)?;

                        serde_json::to_value(v01_expression)
                            .map_err(error::FieldInternalError::ExpressionSerializationError)?
                    }
                    NdcVersion::V02 => serde_json::to_value(ndc_predicate)
                        .map_err(error::FieldInternalError::ExpressionSerializationError)?,
                };
                Ok(ndc_models::Argument::Literal { value })
            }
        }
    }

    pub async fn resolve_raw(
        self,
        http_context: &HttpContext,
    ) -> Result<serde_json::Value, error::FieldError> {
        let value = match self.resolve(http_context).await? {
            ndc_models::Argument::Literal { value } => value,
            ndc_models::Argument::Variable { name: _ } => {
                Err(error::FieldInternalError::InternalGeneric {
                    description: "Cannot encode variable argument value to raw json".to_string(),
                })?
            }
        };
        Ok(value)
    }
}

/// Filter expression plan to be resolved into NDC expression
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
    NDCComparison {
        ndc_expression: ndc_models::Expression,
    },
    LocalRelationshipComparison {
        exists_in_collection: ndc_models::ExistsInCollection,
        predicate: Box<FilterExpression<'s>>,
    },
    RemoteRelationshipComparison {
        relationship_name: RelationshipName,
        model_name: String,
        ndc_column_mapping: Vec<ir::filter::expression::RelationshipColumnMapping>,
        remote_collection: ndc_models::CollectionName,
        remote_query_node: Box<QueryNode<'s>>,
        collection_relationships: BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
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
    pub async fn resolve(
        self,
        http_context: &HttpContext,
    ) -> Result<ndc_models::Expression, error::FieldError>
    where
        's: 'async_recursion,
    {
        match self {
            FilterExpression::And { expressions } => {
                let mut ndc_expressions = Vec::new();
                for expression in expressions {
                    let ndc_expression = expression.resolve(http_context).await?;
                    ndc_expressions.push(ndc_expression);
                }
                Ok(ndc_models::Expression::And {
                    expressions: ndc_expressions,
                })
            }
            FilterExpression::Or { expressions } => {
                let mut ndc_expressions = Vec::new();
                for expression in expressions {
                    let ndc_expression = expression.resolve(http_context).await?;
                    ndc_expressions.push(ndc_expression);
                }
                Ok(ndc_models::Expression::Or {
                    expressions: ndc_expressions,
                })
            }
            FilterExpression::Not { expression } => {
                let ndc_expression = expression.resolve(http_context).await?;
                Ok(ndc_models::Expression::Not {
                    expression: Box::new(ndc_expression),
                })
            }
            FilterExpression::NDCComparison { ndc_expression } => Ok(ndc_expression),
            FilterExpression::LocalRelationshipComparison {
                exists_in_collection,
                predicate,
            } => {
                let ndc_predicate = predicate.resolve(http_context).await?;
                Ok(ndc_models::Expression::Exists {
                    in_collection: exists_in_collection,
                    predicate: Some(Box::new(ndc_predicate)),
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
                            let query = remote_query_node.resolve(http_context).await?;
                            let query_request = ndc_models::QueryRequest {
                                collection: remote_collection,
                                query,
                                arguments: BTreeMap::new(),
                                collection_relationships,
                                variables: None,
                            };

                            let ndc_query_request =
                                ndc_request::make_ndc_query_request(
                                    query_request,
                                    data_connector
                                ).map_err(error::FieldInternalError::NdcRequestDowngradeError)?;
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

                            let rows = single_rowset.rows.unwrap_or_else(Vec::new);

                            // Check if the rows returned from the remote model within the limit
                            // For now, this is a hard limit of 100k rows. We need to expose a metadata
                            // configuration to the users to customize the limit.
                            if rows.len() > 100000 {
                                Err(error::FilterPredicateError::TooManyRowsReturned)?;
                            }
                            let column_comparison = build_source_column_comparisons(rows, &ndc_column_mapping)?;
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
struct DistinctComparisons {
    comparisons: Vec<ndc_models::Expression>,
}

impl DistinctComparisons {
    fn new() -> Self {
        DistinctComparisons {
            comparisons: Vec::new(),
        }
    }

    fn push(&mut self, expression: ndc_models::Expression) {
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
) -> Result<ndc_models::Expression, error::FieldError> {
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
            let source_column_field = ndc_models::FieldName::from(source_column.as_str());
            // Generate LHS (source) column comparison with target column value
            column_comparisons.push(ndc_models::Expression::BinaryComparisonOperator {
                column: ndc_models::ComparisonTarget::Column {
                    name: source_column_field,
                    field_path: field_path.clone(),
                },
                operator: eq_operator.clone(),
                value: ndc_models::ComparisonValue::Scalar {
                    value: target_value.0,
                },
            });
        }
        // combine column comparisons from each row with AND
        // Ex. (source_column_a = target_column_value) AND (source_column_b = target_column_value)
        expressions.push(combine_expressions(column_comparisons, |exps| {
            ndc_models::Expression::And { expressions: exps }
        }));
    }
    // combine all row comparisons with OR
    // Ex. (source_column_a = target_column_value) AND (source_column_b = target_column_value)
    //     OR (source_column_a = target_column_value) AND (source_column_b = target_column_value)
    Ok(combine_expressions(expressions.comparisons, |exps| {
        ndc_models::Expression::Or { expressions: exps }
    }))
}

/// Combines a vector of expressions into one or returns the single item if there's only one.
fn combine_expressions(
    expressions: Vec<ndc_models::Expression>,
    combiner: fn(Vec<ndc_models::Expression>) -> ndc_models::Expression,
) -> ndc_models::Expression {
    match expressions.as_slice() {
        [single] => single.to_owned(),
        _ => combiner(expressions),
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct QueryExecutionPlan<'s> {
    pub query_node: QueryNode<'s>,
    /// The name of a collection
    pub collection: ndc_models::CollectionName,
    /// Values to be provided to any collection arguments
    pub arguments: BTreeMap<ndc_models::ArgumentName, Argument<'s>>,
    /// Any relationships between collections involved in the query request
    pub collection_relationships: BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
    /// One set of named variables for each rowset to fetch. Each variable set
    /// should be subtituted in turn, and a fresh set of rows returned.
    pub variables: Option<Vec<BTreeMap<ndc_models::VariableName, serde_json::Value>>>,
    /// The data connector used to fetch the data
    pub data_connector: &'s metadata_resolve::DataConnectorLink,
}

impl<'s> QueryExecutionPlan<'s> {
    /// Resolve a query execution plan into a NDC Query Request to fetch data from connector.
    pub async fn resolve(
        self,
        http_context: &HttpContext,
    ) -> Result<
        (
            ndc::NdcQueryRequest,
            &'s metadata_resolve::DataConnectorLink,
        ),
        error::FieldError,
    > {
        let QueryExecutionPlan {
            query_node,
            collection,
            arguments,
            collection_relationships,
            variables,
            data_connector,
        } = self;
        let query_request = ndc_models::QueryRequest {
            collection,
            query: query_node.resolve(http_context).await?,
            arguments: resolve_arguments(http_context, arguments).await?,
            collection_relationships,
            variables,
        };
        let ndc_query_request = ndc_request::make_ndc_query_request(query_request, data_connector)
            .map_err(error::FieldInternalError::NdcRequestDowngradeError)?;
        Ok((ndc_query_request, data_connector))
    }

    /// An utility to override the variables in the query plan
    pub fn set_variables(
        &mut self,
        variables: Option<Vec<BTreeMap<ndc_models::VariableName, serde_json::Value>>>,
    ) {
        self.variables = variables;
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
    pub order_by: Option<ndc_models::OrderBy>,
    /// Optionally filter results
    pub predicate: Option<FilterExpression<'s>>,
    /// Aggregate fields of the query
    pub aggregates: Option<IndexMap<ndc_models::FieldName, ndc_models::Aggregate>>,
    /// Fields of the query
    pub fields: Option<IndexMap<ndc_models::FieldName, Field<'s>>>,
    /// Optionally group and aggregate the selected rows
    pub groups: Option<ndc_models::Grouping>,
}

impl<'s> QueryNode<'s> {
    #[async_recursion]
    pub async fn resolve(
        self,
        http_context: &HttpContext,
    ) -> Result<ndc_models::Query, error::FieldError>
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
            groups,
        } = self;
        let ndc_predicate = match predicate {
            Some(predicate) => Some(predicate.resolve(http_context).await?),
            None => None,
        };
        let ndc_fields = match fields {
            Some(fields) => {
                let mut ndc_fields_ = IndexMap::new();
                for (name, field) in fields {
                    ndc_fields_.insert(name, field.resolve(http_context).await?);
                }
                Some(ndc_fields_)
            }
            None => None,
        };
        Ok(ndc_models::Query {
            limit,
            offset,
            order_by,
            predicate: ndc_predicate,
            aggregates,
            fields: ndc_fields,
            groups,
        })
    }
}

/// Field plan
#[derive(Debug, Clone, PartialEq)]
pub enum Field<'s> {
    Column {
        /// Column
        column: ndc_models::FieldName,
        /// Nested fields if column is array or object type
        fields: Option<NestedField<'s>>,
        /// Input field arguments
        arguments: BTreeMap<ndc_models::ArgumentName, Argument<'s>>,
    },
    Relationship {
        /// The relationship query
        query_node: Box<QueryNode<'s>>,
        /// The name of the relationship to follow for the subquery
        relationship: ndc_models::RelationshipName,
        /// Values to be provided to any collection arguments
        arguments: BTreeMap<ndc_models::ArgumentName, Argument<'s>>,
    },
}

impl<'s> Field<'s> {
    /// Resolve field plan into NDC field
    pub async fn resolve(
        self,
        http_context: &HttpContext,
    ) -> Result<ndc_models::Field, error::FieldError> {
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
                Ok(ndc_models::Field::Column {
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
                Ok(ndc_models::Field::Relationship {
                    query: Box::new(query_node),
                    relationship,
                    arguments: resolve_arguments_relationship(http_context, arguments).await?,
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
    pub async fn resolve(
        self,
        http_context: &HttpContext,
    ) -> Result<ndc_models::NestedField, error::FieldError>
    where
        's: 'async_recursion,
    {
        match self {
            NestedField::Object(nested_object) => Ok(ndc_models::NestedField::Object(
                nested_object.resolve(http_context).await?,
            )),
            NestedField::Array(nested_array) => Ok(ndc_models::NestedField::Array(
                nested_array.resolve(http_context).await?,
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NestedObject<'s> {
    pub fields: IndexMap<ndc_models::FieldName, Field<'s>>,
}

impl<'s> NestedObject<'s> {
    pub async fn resolve(
        self,
        http_context: &HttpContext,
    ) -> Result<ndc_models::NestedObject, error::FieldError> {
        let mut fields = IndexMap::new();
        for (name, field) in self.fields {
            fields.insert(name, field.resolve(http_context).await?);
        }
        Ok(ndc_models::NestedObject { fields })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NestedArray<'s> {
    pub fields: Box<NestedField<'s>>,
}

impl<'s> NestedArray<'s> {
    pub async fn resolve(
        self,
        http_context: &HttpContext,
    ) -> Result<ndc_models::NestedArray, error::FieldError> {
        let fields = self.fields.resolve(http_context).await?;
        Ok(ndc_models::NestedArray {
            fields: Box::new(fields),
        })
    }
}

#[derive(Debug)]
pub struct MutationExecutionPlan<'s> {
    /// The name of a procedure
    pub procedure_name: ndc_models::ProcedureName,
    /// Any named procedure arguments
    pub procedure_arguments: BTreeMap<ndc_models::ArgumentName, Argument<'s>>,
    /// The fields to return from the result, or null to return everything
    pub procedure_fields: Option<NestedField<'s>>,
    /// Any relationships between collections involved in the query request
    pub collection_relationships: BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
    /// The data connector used to fetch the data
    pub data_connector: &'s metadata_resolve::DataConnectorLink,
}

impl<'s> MutationExecutionPlan<'s> {
    pub async fn resolve(
        self,
        http_context: &HttpContext,
    ) -> Result<ndc::NdcMutationRequest, error::FieldError> {
        let MutationExecutionPlan {
            procedure_name,
            procedure_arguments,
            procedure_fields,
            collection_relationships,
            data_connector,
        } = self;
        let ndc_fields = match procedure_fields {
            Some(fields) => Some(fields.resolve(http_context).await?),
            None => None,
        };
        let mutation_operation = ndc_models::MutationOperation::Procedure {
            name: procedure_name,
            arguments: resolve_arguments_raw(http_context, procedure_arguments).await?,
            fields: ndc_fields,
        };
        let mutation_request = ndc_models::MutationRequest {
            operations: vec![mutation_operation],
            collection_relationships,
        };
        let ndc_mutation_request =
            ndc_request::make_ndc_mutation_request(mutation_request, data_connector)
                .map_err(error::FieldInternalError::NdcRequestDowngradeError)?;
        Ok(ndc_mutation_request)
    }
}

async fn resolve_arguments<'s>(
    http_context: &HttpContext,
    arguments: BTreeMap<ndc_models::ArgumentName, Argument<'s>>,
) -> Result<BTreeMap<ndc_models::ArgumentName, ndc_models::Argument>, error::FieldError> {
    let mut result = BTreeMap::new();
    for (argument_name, argument_value) in arguments {
        result.insert(argument_name, argument_value.resolve(http_context).await?);
    }
    Ok(result)
}

async fn resolve_arguments_raw<'s>(
    http_context: &HttpContext,
    arguments: BTreeMap<ndc_models::ArgumentName, Argument<'s>>,
) -> Result<BTreeMap<ndc_models::ArgumentName, serde_json::Value>, error::FieldError> {
    let mut result = BTreeMap::new();
    for (argument_name, argument_value) in arguments {
        result.insert(
            argument_name,
            argument_value.resolve_raw(http_context).await?,
        );
    }
    Ok(result)
}

async fn resolve_arguments_relationship<'s>(
    http_context: &HttpContext,
    arguments: BTreeMap<ndc_models::ArgumentName, Argument<'s>>,
) -> Result<BTreeMap<ndc_models::ArgumentName, ndc_models::RelationshipArgument>, error::FieldError>
{
    let mut result = BTreeMap::new();
    for (argument_name, argument_value) in arguments {
        let value = ndc_models::RelationshipArgument::Literal {
            value: argument_value.resolve_raw(http_context).await?,
        };
        result.insert(argument_name, value);
    }
    Ok(result)
}
