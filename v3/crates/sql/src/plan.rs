use core::fmt;
use std::{any::Any, collections::BTreeMap, hash::Hash, sync::Arc};

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
};
use futures::TryFutureExt;
use indexmap::IndexMap;
use open_dds::{
    arguments::ArgumentName,
    query::Alias,
    types::{DataConnectorArgumentName, FieldName},
};
use open_dds::{
    identifier::Identifier,
    query::{
        ModelSelection, ModelTarget, ObjectFieldSelection, ObjectFieldTarget, ObjectSubSelection,
    },
};
use tracing_util::{FutureExt, SpanVisibility, TraceableError};

use execute::{
    plan::{
        self, Argument, Relationship, ResolvedField, ResolvedFilterExpression,
        ResolvedQueryExecutionPlan, ResolvedQueryNode,
    },
    HttpContext,
};
use ir::{NdcFieldAlias, NdcRelationshipName};
use open_dds::data_connector::{CollectionName, DataConnectorColumnName};

#[derive(Debug, thiserror::Error)]
pub enum ExecutionPlanError {
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
                filter: None,
                order_by: vec![],
                limit: None,
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

    pub(crate) fn project(mut self, schema: DFSchemaRef, projection: &[String]) -> Self {
        self.model_selection
            .selection
            .retain(|k, _v| projection.iter().any(|column| column == k.as_str()));
        self.schema = schema;
        self
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
        write!(
            f,
            "ModelQuery: model={}:{}{projection}",
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

#[derive(Debug, Clone)]
pub(crate) struct NDCPushDown {
    http_context: Arc<execute::HttpContext>,
    collection_name: CollectionName,
    arguments: BTreeMap<DataConnectorArgumentName, serde_json::Value>,
    fields: IndexMap<NdcFieldAlias, DataConnectorColumnName>,
    filter: Option<ResolvedFilterExpression>,
    collection_relationships: BTreeMap<NdcRelationshipName, Relationship>,
    data_connector: Arc<metadata_resolve::DataConnectorLink>,
    projected_schema: SchemaRef,
    cache: PlanProperties,
}

impl NDCPushDown {
    pub(crate) fn new(
        http_context: Arc<HttpContext>,
        schema: SchemaRef,
        collection_name: CollectionName,
        arguments: BTreeMap<DataConnectorArgumentName, serde_json::Value>,
        fields: IndexMap<NdcFieldAlias, DataConnectorColumnName>,
        filter: Option<ResolvedFilterExpression>,
        collection_relationships: BTreeMap<NdcRelationshipName, Relationship>,
        data_connector: Arc<metadata_resolve::DataConnectorLink>,
    ) -> Self {
        let cache = Self::compute_properties(schema.clone());
        Self {
            http_context,
            collection_name,
            arguments,
            fields,
            filter,
            collection_relationships,
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

        let query_execution_plan = ResolvedQueryExecutionPlan {
            query_node: ResolvedQueryNode {
                fields: Some(
                    self.fields
                        .iter()
                        .map(|(field_name, connector_column_name)| {
                            (
                                field_name.clone(),
                                ResolvedField::Column {
                                    column: connector_column_name.clone(),
                                    fields: None,
                                    arguments: BTreeMap::new(),
                                },
                            )
                        })
                        .collect(),
                ),
                aggregates: None,
                limit: None,
                offset: None,
                order_by: None,
                predicate: self.filter.clone(),
            },
            collection: self.collection_name.clone(),
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
) -> Result<RecordBatch, ExecutionPlanError> {
    let tracer = tracing_util::global_tracer();

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
