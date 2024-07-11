use core::fmt;
use std::{any::Any, hash::Hash, sync::Arc};

use datafusion::{
    arrow::{
        array::RecordBatch, datatypes::SchemaRef, error::ArrowError, json::reader as arrow_json,
    },
    common::DFSchemaRef,
    error::DataFusionError,
    logical_expr::{LogicalPlan, UserDefinedLogicalNodeCore},
    physical_expr::EquivalenceProperties,
    physical_plan::{
        stream::RecordBatchStreamAdapter, DisplayAs, DisplayFormatType, ExecutionMode,
        ExecutionPlan, Partitioning, PlanProperties,
    },
    sql::TableReference,
};
use execute::HttpContext;
use futures::TryFutureExt;
use tracing_util::{FutureExt, SpanVisibility, TraceableError};

#[derive(Debug, thiserror::Error)]
pub enum ExecutionPlanError {
    #[error("{0}")]
    NDCDowngradeError(#[from] execute::ndc::migration::NdcDowngradeError),

    #[error("{0}")]
    NDCExecutionError(#[from] execute::ndc::client::Error),

    #[error("NDC Response not as expected: {0}")]
    NDCResponseFormat(String),

    #[error("Arrow error: {0}")]
    ArrowError(#[from] ArrowError),

    #[error("Couldn't construct a RecordBatch: {0}")]
    RecordBatchConstruction(String),

    #[error("Couldn't fetch otel tracing context")]
    TracingContextNotFound,
}

impl TraceableError for ExecutionPlanError {
    fn visibility(&self) -> tracing_util::ErrorVisibility {
        tracing_util::ErrorVisibility::Internal
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct NDCQuery {
    pub(crate) table: TableReference,
    pub(crate) query: ndc_models::QueryRequest,
    pub(crate) data_source_name: Arc<ndc_models::CollectionName>,
    pub(crate) schema: DFSchemaRef,
}

impl Hash for NDCQuery {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.data_source_name.hash(state);
        format!("{:#?}", self.query).hash(state);
        self.schema.hash(state);
    }
}

impl Eq for NDCQuery {}

impl NDCQuery {
    pub(crate) fn project(
        mut self,
        schema: DFSchemaRef,
        projection: &[String],
    ) -> datafusion::error::Result<Self> {
        let mut current_fields = self.query.query.fields.take().ok_or_else(|| {
            DataFusionError::Internal("empty fields found in ndcscan for projection".to_string())
        })?;
        let new_fields = projection
            .iter()
            .map(|projected_field| {
                current_fields
                    .swap_remove(projected_field.as_str())
                    .map(|field| (ndc_models::FieldName::from(projected_field.as_str()), field))
                    .ok_or_else(|| {
                        DataFusionError::Internal(
                            "failed to lookup projectd field in ndcscan".to_string(),
                        )
                    })
            })
            .collect::<Result<_, DataFusionError>>()?;
        let _ = std::mem::replace(&mut self.query.query.fields, Some(new_fields));
        let _ = std::mem::replace(&mut self.schema, schema);
        Ok(self)
    }
}

impl UserDefinedLogicalNodeCore for NDCQuery {
    fn name(&self) -> &str {
        "NDCQuery"
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
        write!(f, "NDCQuery: query={:#?}", self.query)
    }

    fn with_exprs_and_inputs(
        &self,
        _exprs: Vec<datafusion::logical_expr::Expr>,
        _inputs: Vec<LogicalPlan>,
    ) -> datafusion::error::Result<Self> {
        Ok(self.clone())
    }
}

#[derive(Debug, Clone)]
pub(crate) struct NDCPushDown {
    http_context: Arc<execute::HttpContext>,
    query: Arc<ndc_models::QueryRequest>,
    data_connector: Arc<metadata_resolve::DataConnectorLink>,
    projected_schema: SchemaRef,
    cache: PlanProperties,
}

impl NDCPushDown {
    pub(crate) fn new(
        http_context: Arc<HttpContext>,
        schema: SchemaRef,
        query: Arc<ndc_models::QueryRequest>,
        data_connector: Arc<metadata_resolve::DataConnectorLink>,
    ) -> Self {
        let cache = Self::compute_properties(schema.clone());
        Self {
            http_context,
            query,
            data_connector,
            projected_schema: schema,
            cache,
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

impl DisplayAs for NDCPushDown {
    fn fmt_as(&self, _t: DisplayFormatType, f: &mut fmt::Formatter) -> std::fmt::Result {
        write!(f, "NDCPushDown")
    }
}

impl ExecutionPlan for NDCPushDown {
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

    fn with_new_children(
        self: Arc<Self>,
        _: Vec<Arc<dyn ExecutionPlan>>,
    ) -> datafusion::error::Result<Arc<dyn ExecutionPlan>> {
        Ok(self)
    }

    fn execute(
        &self,
        _partition: usize,
        context: Arc<datafusion::execution::TaskContext>,
    ) -> datafusion::error::Result<datafusion::execution::SendableRecordBatchStream> {
        let otel_cx = context
            .session_config()
            .get_extension::<tracing_util::Context>()
            .ok_or_else(|| {
                DataFusionError::External(Box::new(ExecutionPlanError::TracingContextNotFound))
            })?;
        let fut = fetch_from_data_connector(
            self.projected_schema.clone(),
            self.http_context.clone(),
            self.query.clone(),
            self.data_connector.clone(),
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
    query_request: Arc<ndc_models::QueryRequest>,
    data_connector: Arc<metadata_resolve::DataConnectorLink>,
) -> Result<RecordBatch, ExecutionPlanError> {
    let tracer = tracing_util::global_tracer();
    let query_request = match data_connector.capabilities.supported_ndc_version {
        metadata_resolve::data_connectors::NdcVersion::V01 => execute::ndc::NdcQueryRequest::V01(
            execute::ndc::migration::v01::downgrade_v02_query_request(
                query_request.as_ref().clone(),
            )?,
        ),
        metadata_resolve::data_connectors::NdcVersion::V02 => {
            execute::ndc::NdcQueryRequest::V02(query_request.as_ref().clone())
        }
    };
    let ndc_response =
        execute::fetch_from_data_connector(&http_context, &query_request, &data_connector, None)
            .await?;
    let batch = tracer.in_span(
        "ndc_response_to_record_batch",
        "Converts NDC Response into datafusion's RecordBatch",
        SpanVisibility::Internal,
        || {
            let rows = ndc_response
                .as_latest_rowsets()
                .pop()
                .ok_or_else(|| {
                    ExecutionPlanError::NDCResponseFormat("no row sets found".to_string())
                })?
                .rows
                .ok_or_else(|| {
                    ExecutionPlanError::NDCResponseFormat(
                        "no rows found for the row set".to_string(),
                    )
                })?;
            let mut decoder = arrow_json::ReaderBuilder::new(schema.clone()).build_decoder()?;
            decoder.serialize(&rows)?;
            decoder.flush()?.ok_or_else(|| {
                ExecutionPlanError::RecordBatchConstruction(
                    "json to arrow decoder did not return any rows".to_string(),
                )
            })
        },
    )?;
    Ok(batch)
}
