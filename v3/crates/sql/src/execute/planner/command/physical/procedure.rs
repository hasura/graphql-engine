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
use std::{any::Any, collections::BTreeMap, sync::Arc};

use execute::{
    ndc::NdcMutationResponse,
    plan::{
        self,
        field::{NestedArray, NestedField, ResolvedNestedField},
        MutationArgument, Relationship, ResolvedField, ResolvedMutationExecutionPlan,
    },
    HttpContext,
};
use graphql_ir::{NdcFieldAlias, NdcRelationshipName};
use open_dds::{
    commands::ProcedureName, data_connector::DataConnectorColumnName,
    types::DataConnectorArgumentName,
};
use tracing_util::{FutureExt, SpanVisibility, TraceableError};

use crate::execute::planner::common::PhysicalPlanOptions;

use super::{function::extract_result_field, CommandOutput};

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

    #[error("Couldn't fetch physical plan options")]
    PhysicalPlanOptionsNotFound,

    #[error("Mutations are requested to be disallowed as part of the request")]
    MutationsAreDisallowed,
}

impl TraceableError for ExecutionPlanError {
    fn visibility(&self) -> tracing_util::ErrorVisibility {
        tracing_util::ErrorVisibility::Internal
    }
}

// Physical node for calling NDC functions. Ideally, non-datafusion specific parts of this node
// should come from engine's plan but we aren't there yet
#[derive(Debug, Clone)]
pub(crate) struct NDCProcedurePushDown {
    http_context: Arc<execute::HttpContext>,
    procedure_name: ProcedureName,
    arguments: BTreeMap<DataConnectorArgumentName, serde_json::Value>,
    fields: Option<ResolvedNestedField>,
    collection_relationships: BTreeMap<NdcRelationshipName, Relationship>,
    // used to post process a command's output
    output: CommandOutput,
    //  The key from which the response has to be extracted if the command output is not the
    //  same as the ndc type. This happens with response config in a data connector link
    extract_response_from: Option<DataConnectorColumnName>,
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
) -> ResolvedNestedField {
    match command_output {
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
    }
}

impl NDCProcedurePushDown {
    pub fn new(
        http_context: Arc<execute::HttpContext>,
        data_connector: Arc<metadata_resolve::DataConnectorLink>,
        procedure_name: ProcedureName,
        arguments: BTreeMap<DataConnectorArgumentName, serde_json::Value>,
        ndc_fields: IndexMap<NdcFieldAlias, ResolvedField>,
        // schema of the output of the command selection
        schema: &DFSchemaRef,
        output: CommandOutput,
        extract_response_from: Option<DataConnectorColumnName>,
    ) -> NDCProcedurePushDown {
        let metrics = ExecutionPlanMetricsSet::new();
        Self {
            http_context,
            procedure_name,
            arguments,
            fields: Some(wrap_ndc_fields(&output, ndc_fields)),
            collection_relationships: BTreeMap::new(),
            output,
            extract_response_from,
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

impl DisplayAs for NDCProcedurePushDown {
    fn fmt_as(&self, _t: DisplayFormatType, f: &mut fmt::Formatter) -> std::fmt::Result {
        write!(f, "NDCProcedurePushDown")
    }
}

impl ExecutionPlan for NDCProcedurePushDown {
    fn name(&self) -> &'static str {
        "NDCProcedurePushdown"
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
        let physical_plan_options = context
            .session_config()
            .get_extension::<PhysicalPlanOptions>()
            .ok_or_else(|| {
                DataFusionError::External(Box::new(ExecutionPlanError::PhysicalPlanOptionsNotFound))
            })?;

        if physical_plan_options.disallow_mutations {
            return Err(DataFusionError::External(Box::new(
                ExecutionPlanError::MutationsAreDisallowed,
            )));
        }

        let baseline_metrics = BaselineMetrics::new(&self.metrics, partition);

        let execution_plan = ResolvedMutationExecutionPlan {
            procedure_name: self.procedure_name.clone(),
            procedure_arguments: self
                .arguments
                .iter()
                .map(|(argument, value)| {
                    (
                        argument.clone(),
                        MutationArgument::Literal {
                            value: value.clone(),
                        },
                    )
                })
                .collect(),
            procedure_fields: self.fields.clone(),
            collection_relationships: self.collection_relationships.clone(),
            data_connector: &self.data_connector,
        };
        let query_request = plan::ndc_request::make_ndc_mutation_request(execution_plan)
            .map_err(|e| DataFusionError::Internal(format!("error creating ndc request: {e}")))?;

        let fut = fetch_from_data_connector(
            self.projected_schema.clone(),
            self.http_context.clone(),
            query_request,
            self.data_connector.clone(),
            self.output.clone(),
            self.extract_response_from.clone(),
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
    request: execute::ndc::NdcMutationRequest,
    data_connector: Arc<metadata_resolve::DataConnectorLink>,
    output: CommandOutput,
    extract_response_from: Option<DataConnectorColumnName>,
    baseline_metrics: BaselineMetrics,
) -> Result<RecordBatch, ExecutionPlanError> {
    let tracer = tracing_util::global_tracer();

    let ndc_response = execute::ndc::fetch_from_data_connector_mutation(
        &http_context,
        &request,
        &data_connector,
        None,
    )
    .await?;
    let batch = tracer.in_span(
        "ndc_response_to_record_batch",
        "Converts NDC Response into datafusion's RecordBatch",
        SpanVisibility::Internal,
        || {
            ndc_response_to_record_batch(
                schema,
                ndc_response,
                &output,
                extract_response_from.as_ref(),
                &baseline_metrics,
            )
        },
    )?;
    Ok(batch)
}

pub fn ndc_response_to_record_batch(
    schema: SchemaRef,
    ndc_response: NdcMutationResponse,
    output: &CommandOutput,
    extract_response_from: Option<&DataConnectorColumnName>,
    baseline_metrics: &BaselineMetrics,
) -> Result<RecordBatch, ExecutionPlanError> {
    let ndc_models::MutationOperationResults::Procedure { result } = ndc_response
        .as_latest()
        .operation_results
        .pop()
        .ok_or_else(|| {
            ExecutionPlanError::NDCResponseFormat("no operation_results found".to_string())
        })?;

    let result = extract_result_field(result, extract_response_from)
        .map_err(ExecutionPlanError::NDCResponseFormat)?;

    let mut decoder =
        datafusion::arrow::json::reader::ReaderBuilder::new(schema.clone()).build_decoder()?;
    match output {
        CommandOutput::Object(_) => {
            let rows = match result {
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
            let rows = match result {
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
