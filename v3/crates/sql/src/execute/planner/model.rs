use core::fmt;
use std::{any::Any, collections::BTreeMap, hash::Hash, sync::Arc};

use crate::catalog::model::common::{to_operand, try_into_column};
use datafusion::{
    arrow::{
        array::RecordBatch, datatypes::SchemaRef, error::ArrowError, json::reader as arrow_json,
    },
    common::DFSchemaRef,
    error::{DataFusionError, Result},
    logical_expr::{Expr, LogicalPlan, UserDefinedLogicalNodeCore},
    physical_expr::EquivalenceProperties,
    physical_plan::{
        metrics::{BaselineMetrics, ExecutionPlanMetricsSet},
        stream::RecordBatchStreamAdapter,
        DisplayAs, DisplayFormatType, ExecutionMode, ExecutionPlan, Partitioning, PlanProperties,
    },
};
use futures::TryFutureExt;
use hasura_authn_core::Session;
use indexmap::IndexMap;
use open_dds::{
    arguments::ArgumentName,
    models::OrderByDirection,
    query::{Aggregate, AggregationFunction, Alias, Operand, OrderByElement},
    types::FieldName,
};
use open_dds::{
    identifier::Identifier,
    query::{
        ModelSelection, ModelTarget, ObjectFieldSelection, ObjectFieldTarget, ObjectSubSelection,
    },
};
use serde::Serialize;
use tracing_util::{FutureExt, SpanVisibility, TraceableError};

use super::common::from_plan_error;
use crate::catalog::model::filter;
use execute::{ndc::NdcQueryResponse, plan::ResolvedField, HttpContext};
use graphql_ir::AggregateFieldSelection;
use plan_types::NdcFieldAlias;

use plan::{
    from_model_aggregate_selection, from_model_selection, ndc_query_to_query_execution_plan,
    NDCQuery,
};

#[derive(Debug, thiserror::Error)]
pub enum ExecutionPlanError {
    #[error("{0}")]
    NDCExecutionError(#[from] execute::ndc::client::Error),

    #[error("NDC Response not as expected: {0}")]
    NDCResponseFormat(String),

    #[error("Arrow error: {0}")]
    ArrowError(#[from] ArrowError),

    #[error("Couldn't fetch otel tracing context")]
    TracingContextNotFound,
}

impl TraceableError for ExecutionPlanError {
    fn visibility(&self) -> tracing_util::ErrorVisibility {
        tracing_util::ErrorVisibility::Internal
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ModelAggregate {
    pub(crate) model_target: ModelTarget,
    pub(crate) selection: IndexMap<String, Aggregate>,
    pub(crate) schema: DFSchemaRef,
}

impl ModelAggregate {
    pub(crate) async fn to_physical_node(
        &self,
        session: &Arc<Session>,
        http_context: &Arc<execute::HttpContext>,
        metadata: &metadata_resolve::Metadata,
        request_headers: &reqwest::header::HeaderMap,
    ) -> Result<NDCAggregatePushdown> {
        let (_, query, fields) = from_model_aggregate_selection(
            &self.model_target,
            &self.selection,
            metadata,
            session,
            http_context,
            request_headers,
        )
        .await
        .map_err(from_plan_error)?;

        Ok(NDCAggregatePushdown::new(
            query,
            http_context.clone(),
            fields,
            self.schema.inner().clone(),
        ))
    }
}

impl Hash for ModelAggregate {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.model_target.subgraph.hash(state);
        self.model_target.model_name.hash(state);
        // Implementing a full hash function is hard because
        // you want to ignore the order of keys in hash maps.
        // So, for now, we only hash some basic information.
    }
}

impl Eq for ModelAggregate {}

impl ModelAggregate {}

impl UserDefinedLogicalNodeCore for ModelAggregate {
    fn name(&self) -> &str {
        "ModelAggregate"
    }

    fn inputs(&self) -> Vec<&LogicalPlan> {
        vec![]
    }

    /// Schema for TopK is the same as the input
    fn schema(&self) -> &DFSchemaRef {
        &self.schema
    }

    fn expressions(&self) -> Vec<datafusion::logical_expr::Expr> {
        vec![]
    }

    /// For example: `TopK: k=10`
    fn fmt_for_explain(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let filter = self
            .model_target
            .filter
            .as_ref()
            .map_or(String::new(), |filter| {
                format!(", filter=[{}]", filter.fmt_for_explain())
            });
        write!(
            f,
            "ModelAggregate: model={}:{}, selection={}{filter}",
            self.model_target.subgraph,
            self.model_target.model_name,
            self.selection
                .keys()
                .map(std::string::ToString::to_string)
                .collect::<Vec<_>>()
                .join(","),
        )
    }

    fn with_exprs_and_inputs(
        &self,
        _exprs: Vec<datafusion::logical_expr::Expr>,
        _inputs: Vec<LogicalPlan>,
    ) -> datafusion::error::Result<Self> {
        Ok(self.clone())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ModelQuery {
    pub(crate) model_selection: ModelSelection,
    pub(crate) schema: DFSchemaRef,
}

impl Hash for ModelQuery {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.model_selection.target.subgraph.hash(state);
        self.model_selection.target.model_name.hash(state);
        // Implementing a full hash function is hard because
        // you want to ignore the order of keys in hash maps.
        // So, for now, we only hash some basic information.
    }
}

impl Eq for ModelQuery {}

impl ModelQuery {
    pub(crate) fn new(
        model: &crate::catalog::model::Model,
        arguments: &BTreeMap<ArgumentName, serde_json::Value>,
        projected_schema: DFSchemaRef,
        filters: &[datafusion::logical_expr::Expr],
        fetch: Option<usize>,
    ) -> datafusion::error::Result<Self> {
        let mut field_selection = IndexMap::new();
        for field in projected_schema.fields() {
            let field_name = {
                let field_name = Identifier::new(field.name().clone()).map_err(|e| {
                    DataFusionError::Internal(format!(
                        "field name conversion failed {}: {}",
                        field.name(),
                        e
                    ))
                })?;
                FieldName::new(field_name)
            };
            field_selection.insert(
                Alias::new(field_name.as_ref().clone()),
                ObjectSubSelection::Field(ObjectFieldSelection {
                    target: ObjectFieldTarget {
                        field_name,
                        arguments: IndexMap::new(),
                    },
                    selection: None,
                }),
            );
        }

        let filter = match filters {
            [] => None,
            [expr] => Some(filter::pushdown_filter(expr)?),
            _ => Some(open_dds::query::BooleanExpression::And(
                filters
                    .iter()
                    .map(filter::pushdown_filter)
                    .collect::<datafusion::error::Result<Vec<_>>>()?,
            )),
        };

        let model_selection = ModelSelection {
            target: ModelTarget {
                subgraph: model.subgraph.clone(),
                model_name: model.name.clone(),
                arguments: arguments
                    .iter()
                    .map(|(argument_name, value)| {
                        (
                            argument_name.clone(),
                            open_dds::query::Value::Literal(value.clone()),
                        )
                    })
                    .collect(),
                filter,
                order_by: vec![],
                limit: fetch,
                offset: None,
            },
            selection: field_selection,
        };
        let model_query_node = ModelQuery {
            model_selection,
            schema: projected_schema,
        };
        Ok(model_query_node)
    }

    pub(crate) fn aggregate(
        &self,
        aggregates: &[Expr],
        schema: Arc<datafusion::common::DFSchema>,
    ) -> datafusion::error::Result<Option<ModelAggregate>> {
        let mut selection = IndexMap::new();

        for (field, aggregate) in schema.fields().iter().zip(aggregates) {
            // NOTE: field_name is a String, but probably ought to be an OpenDD identifier.
            // However, the intermediate IR currently uses GraphQL identifiers, which is not
            // correct, and which does not allow some generated names. So for now, until we
            // refactor things on top of the OpenDD plan, we'll use String here.
            let field_name = field.name().clone();

            let Some(aggregate) = to_aggregate(aggregate)? else {
                // Cannot push this aggregate down
                return Ok(None);
            };

            selection.insert(field_name, aggregate);
        }

        Ok(Some(ModelAggregate {
            model_target: self.model_selection.target.clone(),
            selection,
            schema,
        }))
    }

    pub(crate) fn sort(
        &self,
        sort_by: &[Expr],
        limit: Option<usize>,
    ) -> datafusion::error::Result<Option<Self>> {
        let mut new_order_by_elements: Vec<OrderByElement> = vec![];

        for expr in sort_by {
            let Expr::Sort(sort) = expr else {
                return Ok(None);
            };
            let Some((column, path)) = try_into_column(&sort.expr)? else {
                return Ok(None);
            };

            let operand = to_operand(path, column)?;

            match operand {
                open_dds::query::Operand::Field(_) => {
                    new_order_by_elements.push(OrderByElement {
                        direction: if sort.asc {
                            OrderByDirection::Asc
                        } else {
                            OrderByDirection::Desc
                        },
                        operand,
                    });
                }
                _ => {
                    return Ok(None);
                }
            }
        }

        let order_by = [
            new_order_by_elements,
            self.model_selection.target.order_by.clone(),
        ]
        .concat();

        let new_query = ModelQuery {
            model_selection: ModelSelection {
                target: ModelTarget {
                    order_by,
                    limit,
                    offset: None,
                    ..self.model_selection.target.clone()
                },
                selection: self.model_selection.selection.clone(),
            },
            schema: self.schema.clone(),
        };

        Ok(Some(new_query))
    }

    pub(crate) fn paginate(&self, limit: Option<usize>, offset: usize) -> Self {
        let new_offset = match self.model_selection.target.offset {
            None => offset,
            Some(current_offset) => offset + current_offset,
        };
        let new_limit = match self.model_selection.target.limit {
            None => limit,
            Some(current_limit) => match limit {
                None => Some(0.max(current_limit - offset)),
                Some(limit) if limit + offset > current_limit => {
                    Some(0.max(current_limit - offset))
                }
                Some(limit) => Some(limit),
            },
        };

        ModelQuery {
            model_selection: ModelSelection {
                target: ModelTarget {
                    limit: new_limit,
                    offset: Some(new_offset),
                    ..self.model_selection.target.clone()
                },
                selection: self.model_selection.selection.clone(),
            },
            schema: self.schema.clone(),
        }
    }
}

fn to_aggregate(aggregate: &Expr) -> datafusion::error::Result<Option<Aggregate>> {
    match aggregate.clone().unalias() {
        Expr::AggregateFunction(aggr) => match aggr.func.name() {
            "count" => {
                let operand: Option<Operand> = match aggr.args.as_slice() {
                    [expr] => {
                        if let Expr::Literal(_) = expr {
                            // Push down with no operand
                            None
                        } else if let Some((path, column)) = try_into_column(expr)? {
                            let operand = to_operand(column, path)?;
                            Some(operand)
                        } else {
                            // Cannot push this aggregate down
                            return Ok(None);
                        }
                    }
                    _ => {
                        return Err(DataFusionError::Internal(
                            "'count' aggregate expects one argument'".into(),
                        ));
                    }
                };
                Ok(Some(Aggregate {
                    function: if aggr.distinct {
                        AggregationFunction::CountDistinct {}
                    } else {
                        AggregationFunction::Count {}
                    },
                    operand,
                }))
            }
            _ => Ok(None),
        },
        _ => Ok(None),
    }
}

impl ModelQuery {
    pub(super) async fn to_physical_node(
        &self,
        session: &Arc<Session>,
        http_context: &Arc<execute::HttpContext>,
        metadata: &metadata_resolve::Metadata,
        request_headers: &reqwest::header::HeaderMap,
    ) -> Result<NDCQueryPushDown> {
        let (_, query, ndc_fields) = from_model_selection(
            &self.model_selection,
            metadata,
            session,
            http_context,
            request_headers,
        )
        .await
        .map_err(from_plan_error)?;

        let ndc_pushdown = NDCQueryPushDown::new(
            http_context.clone(),
            self.schema.inner().clone(),
            ndc_fields,
            query,
        );

        Ok(ndc_pushdown)
    }
}

impl UserDefinedLogicalNodeCore for ModelQuery {
    fn name(&self) -> &str {
        "ModelQuery"
    }

    fn inputs(&self) -> Vec<&LogicalPlan> {
        vec![]
    }

    /// Schema for TopK is the same as the input
    fn schema(&self) -> &DFSchemaRef {
        &self.schema
    }

    fn expressions(&self) -> Vec<datafusion::logical_expr::Expr> {
        vec![]
    }

    /// For example: `TopK: k=10`
    fn fmt_for_explain(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let projection = format!(
            ", projection=[{}]",
            self.model_selection
                .selection
                .keys()
                .map(std::string::ToString::to_string)
                .collect::<Vec<_>>()
                .join(",")
        );
        let filter = self
            .model_selection
            .target
            .filter
            .as_ref()
            .map_or(String::new(), |filter| {
                format!(", filter=[{}]", filter.fmt_for_explain())
            });
        let sort = if self.model_selection.target.order_by.is_empty() {
            String::new()
        } else {
            format!(
                ", sort=[{}]",
                self.model_selection
                    .target
                    .order_by
                    .iter()
                    .map(|element| { element.fmt_for_explain() })
                    .collect::<Vec<_>>()
                    .join(",")
            )
        };
        let limit = self
            .model_selection
            .target
            .limit
            .as_ref()
            .map_or(String::new(), |limit| format!(", limit={limit}"));
        let offset = self
            .model_selection
            .target
            .offset
            .as_ref()
            .map_or(String::new(), |offset| format!(", offset={offset}"));
        write!(
            f,
            "ModelQuery: model={}:{}{projection}{filter}{sort}{limit}{offset}",
            self.model_selection.target.subgraph, self.model_selection.target.model_name,
        )
    }

    fn with_exprs_and_inputs(
        &self,
        _exprs: Vec<datafusion::logical_expr::Expr>,
        _inputs: Vec<LogicalPlan>,
    ) -> datafusion::error::Result<Self> {
        Ok(self.clone())
    }
}

#[derive(Debug)]
pub(crate) struct NDCAggregatePushdown {
    query: NDCQuery,
    http_context: Arc<HttpContext>,
    fields: IndexMap<NdcFieldAlias, AggregateFieldSelection>,
    projected_schema: SchemaRef,
    cache: PlanProperties,
    metrics: ExecutionPlanMetricsSet,
}

impl NDCAggregatePushdown {
    pub(crate) fn new(
        query: NDCQuery,
        http_context: Arc<HttpContext>,
        fields: IndexMap<NdcFieldAlias, AggregateFieldSelection>,
        projected_schema: SchemaRef,
    ) -> Self {
        let cache = Self::compute_properties(projected_schema.clone());
        let metrics = ExecutionPlanMetricsSet::new();
        Self {
            query,
            http_context,
            fields,
            projected_schema,
            cache,
            metrics,
        }
    }

    /// This function creates the cache object that stores the plan properties such as schema, equivalence properties, ordering, partitioning, etc.
    fn compute_properties(schema: SchemaRef) -> PlanProperties {
        let eq_properties = EquivalenceProperties::new(schema);
        PlanProperties::new(
            eq_properties,
            Partitioning::UnknownPartitioning(1),
            ExecutionMode::Bounded,
        )
    }
}

impl DisplayAs for NDCAggregatePushdown {
    fn fmt_as(&self, _t: DisplayFormatType, f: &mut fmt::Formatter) -> std::fmt::Result {
        write!(f, "NDCAggregatePushdown")
    }
}

impl ExecutionPlan for NDCAggregatePushdown {
    fn name(&self) -> &'static str {
        "NDCAggregatePushdown"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn properties(&self) -> &PlanProperties {
        &self.cache
    }

    fn children(&self) -> Vec<&Arc<dyn ExecutionPlan>> {
        vec![]
    }

    fn metrics(&self) -> Option<datafusion::physical_plan::metrics::MetricsSet> {
        Some(self.metrics.clone_inner())
    }

    fn with_new_children(
        self: Arc<Self>,
        _: Vec<Arc<dyn ExecutionPlan>>,
    ) -> datafusion::error::Result<Arc<dyn ExecutionPlan>> {
        Ok(self)
    }

    fn execute(
        &self,
        partition: usize,
        context: Arc<datafusion::execution::TaskContext>,
    ) -> datafusion::error::Result<datafusion::execution::SendableRecordBatchStream> {
        let otel_cx = context
            .session_config()
            .get_extension::<tracing_util::Context>()
            .ok_or_else(|| {
                DataFusionError::External(Box::new(ExecutionPlanError::TracingContextNotFound))
            })?;

        let baseline_metrics = BaselineMetrics::new(&self.metrics, partition);

        let query_execution_plan =
            ndc_query_to_query_execution_plan(&self.query, &IndexMap::new(), &self.fields);

        let query_request = execute::plan::ndc_request::make_ndc_query_request(
            query_execution_plan,
        )
        .map_err(|e| DataFusionError::Internal(format!("error creating ndc request: {e}")))?;

        let fut = fetch_aggregates_from_data_connector(
            self.projected_schema.clone(),
            self.http_context.clone(),
            query_request,
            self.query.data_connector.clone(),
            baseline_metrics,
        )
        .with_context((*otel_cx).clone())
        .map_err(|e| DataFusionError::External(Box::new(e)));
        let stream = futures::stream::once(fut);
        Ok(Box::pin(RecordBatchStreamAdapter::new(
            self.projected_schema.clone(),
            stream,
        )))
    }
}

#[derive(Debug, Clone)]
pub(crate) struct NDCQueryPushDown {
    http_context: Arc<execute::HttpContext>,
    fields: IndexMap<NdcFieldAlias, ResolvedField>,
    query: NDCQuery,
    projected_schema: SchemaRef,
    cache: PlanProperties,
    metrics: ExecutionPlanMetricsSet,
}

impl NDCQueryPushDown {
    pub(crate) fn new(
        http_context: Arc<HttpContext>,
        schema: SchemaRef,
        fields: IndexMap<NdcFieldAlias, ResolvedField>,
        query: NDCQuery,
    ) -> Self {
        let cache = Self::compute_properties(schema.clone());
        let metrics = ExecutionPlanMetricsSet::new();
        Self {
            http_context,
            fields,
            query,
            projected_schema: schema,
            cache,
            metrics,
        }
    }

    /// This function creates the cache object that stores the plan properties such as schema, equivalence properties, ordering, partitioning, etc.
    fn compute_properties(schema: SchemaRef) -> PlanProperties {
        let eq_properties = EquivalenceProperties::new(schema);
        PlanProperties::new(
            eq_properties,
            Partitioning::UnknownPartitioning(1),
            ExecutionMode::Bounded,
        )
    }
}

impl DisplayAs for NDCQueryPushDown {
    fn fmt_as(&self, _t: DisplayFormatType, f: &mut fmt::Formatter) -> std::fmt::Result {
        write!(f, "NDCQueryPushDown")
    }
}

impl ExecutionPlan for NDCQueryPushDown {
    fn name(&self) -> &'static str {
        "NDCPushdown"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn properties(&self) -> &PlanProperties {
        &self.cache
    }

    fn children(&self) -> Vec<&Arc<dyn ExecutionPlan>> {
        vec![]
    }

    fn metrics(&self) -> Option<datafusion::physical_plan::metrics::MetricsSet> {
        Some(self.metrics.clone_inner())
    }

    fn with_new_children(
        self: Arc<Self>,
        _: Vec<Arc<dyn ExecutionPlan>>,
    ) -> datafusion::error::Result<Arc<dyn ExecutionPlan>> {
        Ok(self)
    }

    fn execute(
        &self,
        partition: usize,
        context: Arc<datafusion::execution::TaskContext>,
    ) -> datafusion::error::Result<datafusion::execution::SendableRecordBatchStream> {
        let otel_cx = context
            .session_config()
            .get_extension::<tracing_util::Context>()
            .ok_or_else(|| {
                DataFusionError::External(Box::new(ExecutionPlanError::TracingContextNotFound))
            })?;

        let baseline_metrics = BaselineMetrics::new(&self.metrics, partition);

        let query_execution_plan =
            ndc_query_to_query_execution_plan(&self.query, &self.fields, &IndexMap::new());

        let query_request = execute::plan::ndc_request::make_ndc_query_request(
            query_execution_plan,
        )
        .map_err(|e| DataFusionError::Internal(format!("error creating ndc request: {e}")))?;

        let fut = fetch_from_data_connector(
            self.projected_schema.clone(),
            self.http_context.clone(),
            query_request,
            self.query.data_connector.clone(),
            baseline_metrics,
        )
        .with_context((*otel_cx).clone())
        .map_err(|e| DataFusionError::External(Box::new(e)));
        let stream = futures::stream::once(fut);
        Ok(Box::pin(RecordBatchStreamAdapter::new(
            self.projected_schema.clone(),
            stream,
        )))
    }
}

pub async fn fetch_from_data_connector(
    schema: SchemaRef,
    http_context: Arc<HttpContext>,
    query_request: execute::ndc::NdcQueryRequest,
    data_connector: Arc<metadata_resolve::DataConnectorLink>,
    baseline_metrics: BaselineMetrics,
) -> Result<RecordBatch, ExecutionPlanError> {
    let tracer = tracing_util::global_tracer();

    let ndc_response =
        execute::fetch_from_data_connector(&http_context, &query_request, &data_connector, None)
            .await?;
    let batch = tracer.in_span(
        "ndc_response_to_record_batch",
        "Converts NDC Response into datafusion's RecordBatch",
        SpanVisibility::Internal,
        || ndc_response_to_record_batch(schema, ndc_response, &baseline_metrics),
    )?;
    Ok(batch)
}

pub async fn fetch_aggregates_from_data_connector(
    schema: SchemaRef,
    http_context: Arc<HttpContext>,
    query_request: execute::ndc::NdcQueryRequest,
    data_connector: Arc<metadata_resolve::DataConnectorLink>,
    baseline_metrics: BaselineMetrics,
) -> Result<RecordBatch, ExecutionPlanError> {
    let tracer = tracing_util::global_tracer();

    let ndc_response =
        execute::fetch_from_data_connector(&http_context, &query_request, &data_connector, None)
            .await?;
    let batch = tracer.in_span(
        "ndc_response_to_record_batch",
        "Converts NDC Response into datafusion's RecordBatch",
        SpanVisibility::Internal,
        || ndc_aggregates_response_to_record_batch(schema, ndc_response, &baseline_metrics),
    )?;
    Ok(batch)
}

pub fn ndc_response_to_record_batch(
    schema: SchemaRef,
    ndc_response: NdcQueryResponse,
    baseline_metrics: &BaselineMetrics,
) -> Result<RecordBatch, ExecutionPlanError> {
    let rows = ndc_response
        .as_latest_rowsets()
        .pop()
        .ok_or_else(|| ExecutionPlanError::NDCResponseFormat("no row sets found".to_string()))?
        .rows
        .ok_or_else(|| {
            ExecutionPlanError::NDCResponseFormat("no rows found for the row set".to_string())
        })?;
    baseline_metrics.record_output(rows.len());
    to_record_batch(schema, &rows)
}

pub fn ndc_aggregates_response_to_record_batch(
    schema: SchemaRef,
    ndc_response: NdcQueryResponse,
    baseline_metrics: &BaselineMetrics,
) -> Result<RecordBatch, ExecutionPlanError> {
    let aggregates = ndc_response
        .as_latest_rowsets()
        .pop()
        .ok_or_else(|| ExecutionPlanError::NDCResponseFormat("no row sets found".to_string()))?
        .aggregates
        .ok_or_else(|| {
            ExecutionPlanError::NDCResponseFormat("no rows found for the row set".to_string())
        })?;
    baseline_metrics.record_output(1);
    to_record_batch(schema, &[aggregates])
}

fn to_record_batch<S: Serialize>(
    schema: SchemaRef,
    rows: &[S],
) -> Result<RecordBatch, ExecutionPlanError> {
    let mut decoder = arrow_json::ReaderBuilder::new(schema.clone()).build_decoder()?;
    decoder.serialize(rows)?;
    // flush will return `None` if there are no rows in the response
    let record_batch = decoder
        .flush()?
        .unwrap_or_else(|| RecordBatch::new_empty(schema));
    Ok(record_batch)
}

// use super::*;

#[cfg(test)]
mod tests {
    use super::*;
    use datafusion::arrow::array::{Int32Array, StringArray};
    use datafusion::arrow::datatypes::{DataType, Field, Schema};
    use execute::ndc::NdcQueryResponse;
    use ndc_models::{QueryResponse, RowFieldValue, RowSet};
    use std::sync::Arc;

    fn create_test_schema() -> SchemaRef {
        Arc::new(Schema::new(vec![
            Field::new("id", DataType::Int32, false),
            Field::new("name", DataType::Utf8, false),
        ]))
    }

    #[test]
    fn test_ndc_response_to_record_batch_success() {
        let schema = create_test_schema();

        let query_response = QueryResponse(vec![RowSet {
            rows: Some(vec![
                IndexMap::from([
                    ("id".into(), RowFieldValue(serde_json::json!(1))),
                    ("name".into(), RowFieldValue(serde_json::json!("Alice"))),
                ]),
                IndexMap::from([
                    ("id".into(), RowFieldValue(serde_json::json!(2))),
                    ("name".into(), RowFieldValue(serde_json::json!("Bob"))),
                ]),
            ]),
            aggregates: None,
            groups: None,
        }]);

        let metrics = BaselineMetrics::new(&ExecutionPlanMetricsSet::new(), 0);
        let result =
            ndc_response_to_record_batch(schema, NdcQueryResponse::V02(query_response), &metrics);
        assert!(result.is_ok());

        let record_batch = result.unwrap();
        assert_eq!(record_batch.num_rows(), 2);
        assert_eq!(record_batch.num_columns(), 2);

        let id_array = record_batch
            .column(0)
            .as_any()
            .downcast_ref::<Int32Array>()
            .unwrap();
        let name_array = record_batch
            .column(1)
            .as_any()
            .downcast_ref::<StringArray>()
            .unwrap();

        assert_eq!(id_array.value(0), 1);
        assert_eq!(id_array.value(1), 2);
        assert_eq!(name_array.value(0), "Alice");
        assert_eq!(name_array.value(1), "Bob");
    }

    #[test]
    fn test_ndc_response_to_record_batch_empty_response() {
        let schema = create_test_schema();

        let query_response = QueryResponse(vec![RowSet {
            rows: Some(vec![]),
            aggregates: None,
            groups: None,
        }]);

        let metrics = BaselineMetrics::new(&ExecutionPlanMetricsSet::new(), 0);
        let result =
            ndc_response_to_record_batch(schema, NdcQueryResponse::V02(query_response), &metrics);
        assert!(result.is_ok());

        let record_batch = result.unwrap();
        assert_eq!(record_batch.num_rows(), 0);
        assert_eq!(record_batch.num_columns(), 2);
    }

    #[test]
    fn test_ndc_response_to_record_batch_no_rowsets() {
        let schema = create_test_schema();

        let query_response = QueryResponse(vec![]);

        let metrics = BaselineMetrics::new(&ExecutionPlanMetricsSet::new(), 0);

        let result =
            ndc_response_to_record_batch(schema, NdcQueryResponse::V02(query_response), &metrics);
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err(),
            ExecutionPlanError::NDCResponseFormat(_)
        ));
    }

    #[test]
    fn test_ndc_response_to_record_batch_no_rows() {
        let schema = create_test_schema();

        let query_response = QueryResponse(vec![RowSet {
            rows: None,
            aggregates: None,
            groups: None,
        }]);

        let metrics = BaselineMetrics::new(&ExecutionPlanMetricsSet::new(), 0);

        let result =
            ndc_response_to_record_batch(schema, NdcQueryResponse::V02(query_response), &metrics);
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err(),
            ExecutionPlanError::NDCResponseFormat(_)
        ));
    }

    #[test]
    fn test_ndc_response_to_record_batch_mismatched_schema() {
        let schema = create_test_schema();

        let query_response = QueryResponse(vec![RowSet {
            rows: Some(vec![IndexMap::from([
                (
                    "id".into(),
                    RowFieldValue(serde_json::json!("not an integer")),
                ),
                ("name".into(), RowFieldValue(serde_json::json!("Alice"))),
            ])]),
            aggregates: None,
            groups: None,
        }]);

        let metrics = BaselineMetrics::new(&ExecutionPlanMetricsSet::new(), 0);

        let result =
            ndc_response_to_record_batch(schema, NdcQueryResponse::V02(query_response), &metrics);
        assert!(result.is_err());
    }
}
