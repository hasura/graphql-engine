use std::sync::Arc;

use datafusion::{
    arrow::{array::RecordBatch, error::ArrowError, json::writer::JsonArray, json::WriterBuilder},
    dataframe::DataFrame,
    error::DataFusionError,
};
use hasura_authn_core::Session;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use tracing_util::{ErrorVisibility, SpanVisibility, Successful, TraceableError};

pub use datafusion::execution::context::SessionContext;

pub(crate) mod analyzer;
pub(crate) mod optimizer;
pub(crate) mod planner;

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SqlRequest {
    sql: String,
}

#[derive(Error, Debug, Clone)]
pub enum SqlExecutionError {
    #[error("error in data fusion: {0}")]
    DataFusion(String),
    #[error("error in encoding data: {0}")]
    Arrow(String),
}

impl From<DataFusionError> for SqlExecutionError {
    fn from(e: DataFusionError) -> Self {
        Self::DataFusion(e.to_string())
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
    context: &crate::catalog::Context,
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
                let session = context.create_session_context(&session, &http_context);
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
                    session_context
                        .sql(&request.sql)
                        .await
                        .map_err(|e| SqlExecutionError::DataFusion(e.to_string()))
                })
            },
        )
        .await?;
    let batches = tracer
        .in_span_async(
            "execute_logical_plan",
            "Executes the Logical Plan of a query",
            SpanVisibility::User,
            || Box::pin(async { execute_logical_plan(data_frame).await }),
        )
        .await?;
    tracer.in_span(
        "serialize_record_batch",
        "Serializes datafusion's RecordBatch into a JSON array",
        SpanVisibility::User,
        || record_batches_to_json_array(&batches),
    )
}

async fn execute_logical_plan(frame: DataFrame) -> Result<Vec<RecordBatch>, SqlExecutionError> {
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
                        .map_err(|e| SqlExecutionError::DataFusion(e.to_string()))
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
                let task_ctx = Arc::new(task_ctx.with_session_config(
                    session_config.with_extension(Arc::new(tracing_util::Context::current())),
                ));
                Box::pin(async {
                    datafusion::physical_plan::collect(plan, task_ctx)
                        .await
                        .map_err(|e| SqlExecutionError::DataFusion(e.to_string()))
                })
            },
        )
        .await?;
    Ok(record_batches)
}

fn record_batches_to_json_array(batches: &[RecordBatch]) -> Result<Vec<u8>, SqlExecutionError> {
    if batches.is_empty() {
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
