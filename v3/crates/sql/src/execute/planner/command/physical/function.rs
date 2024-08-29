use core::fmt;
use datafusion::{
    arrow::{array::RecordBatch, datatypes::SchemaRef, error::ArrowError},
    common::DFSchemaRef,
    error::{DataFusionError, Result},
    physical_expr::EquivalenceProperties,
    physical_plan::{
        metrics::{BaselineMetrics, ExecutionPlanMetricsSet},
        stream::RecordBatchStreamAdapter,
        DisplayAs, DisplayFormatType, ExecutionMode, ExecutionPlan, Partitioning, PlanProperties,
    },
};
use futures::TryFutureExt;
use indexmap::IndexMap;
use metadata_resolve::Qualified;
use serde::{Deserialize, Serialize};
use std::{any::Any, collections::BTreeMap, sync::Arc};

use execute::{
    ndc::{NdcQueryResponse, FUNCTION_IR_VALUE_COLUMN_NAME},
    plan::{
        self,
        field::{NestedArray, NestedField},
        Argument, Relationship, ResolvedField, ResolvedQueryExecutionPlan, ResolvedQueryNode,
    },
    HttpContext,
};
use ir::{NdcFieldAlias, NdcRelationshipName};
use open_dds::{
    commands::FunctionName,
    data_connector::CollectionName,
    types::{CustomTypeName, DataConnectorArgumentName},
};
use tracing_util::{FutureExt, SpanVisibility, TraceableError};

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

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(crate) enum CommandOutput {
    Object(Qualified<CustomTypeName>),
    ListOfObjects(Qualified<CustomTypeName>),
}

// Physical node for calling NDC functions. Ideally, non-datafusion specific parts of this node
// should come from engine's plan but we aren't there yet
#[derive(Debug, Clone)]
pub(crate) struct NDCFunctionPushDown {
    http_context: Arc<execute::HttpContext>,
    function_name: FunctionName,
    arguments: BTreeMap<DataConnectorArgumentName, serde_json::Value>,
    fields: IndexMap<NdcFieldAlias, ResolvedField>,
    collection_relationships: BTreeMap<NdcRelationshipName, Relationship>,
    // used to post process a command's output
    output: CommandOutput,
    data_connector: Arc<metadata_resolve::DataConnectorLink>,
    // the schema of the node's output
    projected_schema: SchemaRef,
    // some datafusion detail
    cache: PlanProperties,
    metrics: ExecutionPlanMetricsSet,
}

fn wrap_ndc_fields(
    command_output: &CommandOutput,
    ndc_fields: IndexMap<NdcFieldAlias, ResolvedField>,
) -> IndexMap<NdcFieldAlias, ResolvedField> {
    let value_field = match command_output {
        CommandOutput::Object(_) => {
            NestedField::Object(plan::field::NestedObject { fields: ndc_fields })
        }
        CommandOutput::ListOfObjects(_) => {
            let nested_fields =
                NestedField::Object(plan::field::NestedObject { fields: ndc_fields });
            NestedField::Array(NestedArray {
                fields: Box::new(nested_fields),
            })
        }
    };
    IndexMap::from([(
        NdcFieldAlias::from(FUNCTION_IR_VALUE_COLUMN_NAME),
        execute::plan::field::Field::Column {
            column: open_dds::data_connector::DataConnectorColumnName::from(
                FUNCTION_IR_VALUE_COLUMN_NAME,
            ),
            fields: Some(value_field),
            arguments: BTreeMap::new(),
        },
    )])
}

impl NDCFunctionPushDown {
    pub fn new(
        http_context: Arc<execute::HttpContext>,
        data_connector: Arc<metadata_resolve::DataConnectorLink>,
        function_name: FunctionName,
        arguments: BTreeMap<DataConnectorArgumentName, serde_json::Value>,
        ndc_fields: IndexMap<NdcFieldAlias, ResolvedField>,
        // schema of the output of the command selection
        schema: &DFSchemaRef,
        output: CommandOutput,
    ) -> NDCFunctionPushDown {
        let metrics = ExecutionPlanMetricsSet::new();
        Self {
            http_context,
            function_name,
            arguments,
            fields: wrap_ndc_fields(&output, ndc_fields),
            collection_relationships: BTreeMap::new(),
            output,
            data_connector,
            projected_schema: schema.inner().clone(),
            cache: Self::compute_properties(schema.inner().clone()),
            metrics,
        }
    }
    /// This function creates the cache object that stores the plan properties such as schema,
    /// equivalence properties, ordering, partitioning, etc.
    fn compute_properties(schema: SchemaRef) -> PlanProperties {
        let eq_properties = EquivalenceProperties::new(schema);
        PlanProperties::new(
            eq_properties,
            Partitioning::UnknownPartitioning(1),
            ExecutionMode::Bounded,
        )
    }
}

impl DisplayAs for NDCFunctionPushDown {
    fn fmt_as(&self, _t: DisplayFormatType, f: &mut fmt::Formatter) -> std::fmt::Result {
        write!(f, "NDCFunctionPushDown")
    }
}

impl ExecutionPlan for NDCFunctionPushDown {
    fn name(&self) -> &'static str {
        "NDCFunctionPushdown"
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

        let query_execution_plan = ResolvedQueryExecutionPlan {
            query_node: ResolvedQueryNode {
                fields: Some(
                    self.fields
                        .iter()
                        .map(|(field_name, field)| (field_name.clone(), field.clone()))
                        .collect(),
                ),
                aggregates: None,
                limit: None,
                offset: None,
                order_by: None,
                predicate: None,
            },
            collection: CollectionName::from(self.function_name.as_str()),
            arguments: self
                .arguments
                .iter()
                .map(|(argument, value)| {
                    (
                        argument.clone(),
                        Argument::Literal {
                            value: value.clone(),
                        },
                    )
                })
                .collect(),
            collection_relationships: self.collection_relationships.clone(),
            variables: None,
            data_connector: &self.data_connector,
        };
        let query_request = plan::ndc_request::make_ndc_query_request(query_execution_plan)
            .map_err(|e| DataFusionError::Internal(format!("error creating ndc request: {e}")))?;

        let fut = fetch_from_data_connector(
            self.projected_schema.clone(),
            self.http_context.clone(),
            query_request,
            self.data_connector.clone(),
            self.output.clone(),
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
    output: CommandOutput,
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
        || ndc_response_to_record_batch(schema, ndc_response, &output, &baseline_metrics),
    )?;
    Ok(batch)
}

pub fn ndc_response_to_record_batch(
    schema: SchemaRef,
    ndc_response: NdcQueryResponse,
    output: &CommandOutput,
    baseline_metrics: &BaselineMetrics,
) -> Result<RecordBatch, ExecutionPlanError> {
    let rows = ndc_response
        .as_latest_rowsets()
        .pop()
        .ok_or_else(|| ExecutionPlanError::NDCResponseFormat("no row sets found".to_string()))?
        .rows
        .ok_or_else(|| {
            ExecutionPlanError::NDCResponseFormat("no rows found for the row set".to_string())
        })?
        .pop()
        .ok_or_else(|| {
            ExecutionPlanError::NDCResponseFormat("expecting at least one row".to_string())
        })?
        .swap_remove(FUNCTION_IR_VALUE_COLUMN_NAME)
        .ok_or_else(|| {
            ExecutionPlanError::NDCResponseFormat(
                "missing field: {FUNCTION_IR_VALUE_COLUMN_NAME}".to_string(),
            )
        })?
        .0;

    let mut decoder =
        datafusion::arrow::json::reader::ReaderBuilder::new(schema.clone()).build_decoder()?;
    match output {
        CommandOutput::Object(_) => {
            let rows = match rows {
                serde_json::Value::Object(v) => {
                    baseline_metrics.record_output(1);
                    Ok(Vec::from([v]))
                }
                serde_json::Value::Null => Ok(Vec::from([])),
                _ => Err(ExecutionPlanError::NDCResponseFormat(
                    "expecting an object for __value field".to_string(),
                )),
            }?;
            decoder.serialize(&rows)?;
        }
        CommandOutput::ListOfObjects(_) => {
            let rows = match rows {
                serde_json::Value::Null => Ok(Vec::from([])),
                serde_json::Value::Array(v) => {
                    baseline_metrics.record_output(v.len());
                    Ok(v)
                }
                _ => Err(ExecutionPlanError::NDCResponseFormat(
                    "expecting a list for __value field".to_string(),
                )),
            }?;
            decoder.serialize(&rows)?;
        }
    }
    // flush will return `None` if there are no rows in the response
    let record_batch = decoder
        .flush()?
        .unwrap_or_else(|| RecordBatch::new_empty(schema));
    Ok(record_batch)
}
