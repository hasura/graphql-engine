use std::sync::Arc;

use datafusion::{
    arrow::{
        array::RecordBatch,
        error::ArrowError,
        json::{writer::JsonArray, WriterBuilder},
    },
    common::tree_node::TreeNode,
    dataframe::DataFrame,
    error::DataFusionError,
};
use hasura_authn_core::Session;
use planner::{
    command::{NDCFunctionPushDown, NDCProcedurePushDown},
    common::PhysicalPlanOptions,
    model::{NDCAggregatePushdown, NDCQueryPushDown},
};
use serde::{Deserialize, Serialize};
use thiserror::Error;

use tracing_util::{
    set_attribute_on_active_span, ErrorVisibility, SpanVisibility, Successful, TraceableError,
};

pub use datafusion::execution::context::SessionContext;

pub(crate) mod optimizer;
pub(crate) mod planner;

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SqlRequest {
    pub sql: String,
    #[serde(default)]
    pub disallow_mutations: bool,
}

impl SqlRequest {
    pub fn new(sql: String) -> Self {
        SqlRequest {
            sql,
            disallow_mutations: false,
        }
    }
}

#[derive(Error, Debug)]
pub enum SqlExecutionError {
    #[error("error in data fusion: {0}")]
    DataFusion(#[from] DataFusionError),
    #[error("error in encoding data: {0}")]
    Arrow(String),
}

impl SqlExecutionError {
    pub fn to_error_response(&self) -> serde_json::Value {
        let message = self.to_string();
        let detail = match self {
            SqlExecutionError::DataFusion(DataFusionError::External(e)) => {
                use planner::command::physical::procedure::ExecutionPlanError;
                if let Some(ExecutionPlanError::MutationsAreDisallowed(command_target)) =
                    e.downcast_ref::<ExecutionPlanError>()
                {
                    serde_json::to_value(command_target).unwrap_or(serde_json::Value::Null)
                } else {
                    serde_json::Value::Null
                }
            }
            _ => serde_json::Value::Null,
        };
        if detail == serde_json::Value::Null {
            serde_json::json!({"error": message})
        } else {
            serde_json::json!({"error": message, "detail": detail})
        }
    }
}

impl From<ArrowError> for SqlExecutionError {
    fn from(e: ArrowError) -> Self {
        Self::Arrow(e.to_string())
    }
}

impl TraceableError for SqlExecutionError {
    fn visibility(&self) -> ErrorVisibility {
        ErrorVisibility::User
    }
}

/// Executes an SQL Request using the Apache DataFusion query engine.
pub async fn execute_sql(
    request_headers: Arc<reqwest::header::HeaderMap>,
    catalog: Arc<crate::catalog::Catalog>,
    session: Arc<Session>,
    http_context: Arc<execute::HttpContext>,
    request: &SqlRequest,
) -> Result<Vec<u8>, SqlExecutionError> {
    let tracer = tracing_util::global_tracer();
    let session_context = tracer
        .in_span(
            "create_session_context",
            "Create a datafusion SessionContext",
            SpanVisibility::Internal,
            || {
                let session =
                    catalog.create_session_context(&request_headers, &session, &http_context);
                Successful::new(session)
            },
        )
        .into_inner();
    let data_frame = tracer
        .in_span_async(
            "create_logical_plan",
            "Creates a Logical Plan for the given SQL statement",
            SpanVisibility::User,
            || {
                Box::pin(async {
                    let logical_plan = session_context.sql(&request.sql).await?;
                    set_attribute_on_active_span(
                        tracing_util::AttributeVisibility::Default,
                        "logical_plan",
                        logical_plan.logical_plan().to_string(),
                    );

                    Ok::<_, SqlExecutionError>(logical_plan)
                })
            },
        )
        .await?;
    let physical_plan_options = PhysicalPlanOptions::new(request.disallow_mutations);
    let batches = tracer
        .in_span_async(
            "execute_logical_plan",
            "Executes the Logical Plan of a query",
            SpanVisibility::User,
            || Box::pin(async { execute_logical_plan(physical_plan_options, data_frame).await }),
        )
        .await?;
    tracer.in_span(
        "serialize_record_batch",
        "Serializes datafusion's RecordBatch into a JSON array",
        SpanVisibility::User,
        || record_batches_to_json_array(&batches),
    )
}

async fn execute_logical_plan(
    physical_plan_options: PhysicalPlanOptions,
    frame: DataFrame,
) -> Result<Vec<RecordBatch>, SqlExecutionError> {
    let tracer = tracing_util::global_tracer();
    let task_ctx = frame.task_ctx();
    let session_config = task_ctx.session_config().clone();
    let plan = tracer
        .in_span_async(
            "create_physical_plan",
            "Creates a physical plan from a logical plan",
            SpanVisibility::User,
            || {
                Box::pin(async {
                    frame
                        .create_physical_plan()
                        .await
                        .map_err(SqlExecutionError::DataFusion)
                })
            },
        )
        .await?;
    let record_batches = tracer
        .in_span_async(
            "execute_physical_plan",
            "Executes a physical plan to collect record batches",
            SpanVisibility::User,
            || {
                let task_ctx = Arc::new(
                    task_ctx.with_session_config(
                        session_config
                            .with_extension(Arc::new(tracing_util::Context::current()))
                            .with_extension(Arc::new(physical_plan_options)),
                    ),
                );
                Box::pin(async {
                    let results =
                        datafusion::physical_plan::collect(plan.clone(), task_ctx).await?;

                    set_attribute_on_active_span(
                        tracing_util::AttributeVisibility::Default,
                        "total_rows",
                        results
                            .iter()
                            .map(|x| i64::try_from(x.num_rows()).unwrap_or(0))
                            .sum::<i64>(),
                    );

                    // NOTE: we must compute these metrics _after_ running
                    // datafusion::physical_plan::collect, because otherwise the execution
                    // metrics will not be populated. Even though 'plan' is not marked
                    // mutable, it uses interior mutability throughout the structure in order
                    // to maintain its metrics.

                    // Compute rows_processed by summing over all subplans -
                    // we want to know if any of our nodes fetched an unusually high number
                    // of rows.
                    let mut rows_fetched: i64 = 0;

                    plan.apply(|node| {
                        if node.as_any().is::<NDCQueryPushDown>()
                            || node.as_any().is::<NDCAggregatePushdown>()
                            || node.as_any().is::<NDCFunctionPushDown>()
                            || node.as_any().is::<NDCProcedurePushDown>()
                        {
                            if let Some(metrics) = node.metrics() {
                                if let Some(output_rows) = metrics.output_rows() {
                                    rows_fetched += i64::try_from(output_rows).unwrap_or(0);
                                }
                            }
                        }
                        Ok(datafusion::common::tree_node::TreeNodeRecursion::Continue)
                    })?;

                    set_attribute_on_active_span(
                        tracing_util::AttributeVisibility::Default,
                        "rows_fetched",
                        rows_fetched,
                    );

                    // Take elapsed_compute metric from the top level node
                    if let Some(metrics) = plan.metrics() {
                        if let Some(value) = metrics.elapsed_compute() {
                            set_attribute_on_active_span(
                                tracing_util::AttributeVisibility::Default,
                                "elapsed_compute",
                                i64::try_from(value).unwrap_or(0),
                            );
                        }
                    }

                    Ok::<_, SqlExecutionError>(results)
                })
            },
        )
        .await?;
    Ok(record_batches)
}

fn record_batches_to_json_array(batches: &[RecordBatch]) -> Result<Vec<u8>, SqlExecutionError> {
    if batches
        .iter()
        .map(datafusion::arrow::array::RecordBatch::num_rows)
        .sum::<usize>()
        == 0
    {
        return Ok(vec![b'[', b']']);
    }
    // Write the record batch out as a JSON array
    let buf = Vec::new();

    let builder = WriterBuilder::new().with_explicit_nulls(true);
    let mut writer = builder.build::<_, JsonArray>(buf);

    for batch in batches {
        writer.write(batch)?;
    }
    writer.finish()?;

    // Get the underlying buffer back,
    Ok(writer.into_inner())
}
