use hasura_authn_core::Session;
use std::{collections::BTreeMap, sync::Arc};

use datafusion::{
    error::{DataFusionError, Result},
    execution::context::{QueryPlanner, SessionState},
    logical_expr::{LogicalPlan, UserDefinedLogicalNode},
    physical_plan::ExecutionPlan,
    physical_planner::{DefaultPhysicalPlanner, ExtensionPlanner, PhysicalPlanner},
};

use metadata_resolve::FilterPermission;
use open_dds::identifier::Identifier;
use open_dds::types::FieldName;

use crate::plan::NDCPushDown;

use async_trait::async_trait;

pub(crate) struct OpenDDQueryPlanner {
    pub(crate) session: Arc<Session>,
    pub(crate) http_context: Arc<execute::HttpContext>,
    pub(crate) catalog: Arc<crate::catalog::Catalog>,
}

#[async_trait]
impl QueryPlanner for OpenDDQueryPlanner {
    /// Given a `LogicalPlan` created from above, create an
    /// `ExecutionPlan` suitable for execution
    async fn create_physical_plan(
        &self,
        logical_plan: &LogicalPlan,
        session_state: &SessionState,
    ) -> Result<Arc<dyn ExecutionPlan>> {
        // Teach the default physical planner how to plan TopK nodes.
        let physical_planner =
            DefaultPhysicalPlanner::with_extension_planners(vec![Arc::new(NDCPushDownPlanner {
                session: self.session.clone(),
                http_context: self.http_context.clone(),
                catalog: self.catalog.clone(),
            })]);
        // Delegate most work of physical planning to the default physical planner
        physical_planner
            .create_physical_plan(logical_plan, session_state)
            .await
    }
}

pub(crate) struct NDCPushDownPlanner {
    pub(crate) session: Arc<Session>,
    pub(crate) http_context: Arc<execute::HttpContext>,
    pub(crate) catalog: Arc<crate::catalog::Catalog>,
}

#[async_trait]
impl ExtensionPlanner for NDCPushDownPlanner {
    /// Create a physical plan for an extension node
    async fn plan_extension(
        &self,
        _planner: &dyn PhysicalPlanner,
        node: &dyn UserDefinedLogicalNode,
        logical_inputs: &[&LogicalPlan],
        physical_inputs: &[Arc<dyn ExecutionPlan>],
        _session_state: &SessionState,
    ) -> Result<Option<Arc<dyn ExecutionPlan>>> {
        if let Some(ndc_node) = node.as_any().downcast_ref::<crate::plan::NDCQuery>() {
            assert_eq!(logical_inputs.len(), 0, "Inconsistent number of inputs");
            assert_eq!(physical_inputs.len(), 0, "Inconsistent number of inputs");
            let type_permissions = self
                .catalog
                .type_permissions
                .get(&self.session.role)
                .ok_or_else(|| {
                    DataFusionError::Plan(format!(
                        "role {} does not have permission to select any fields of model {}",
                        self.session.role, ndc_node.model.name
                    ))
                })?;
            let base_type_allowed_fields = &type_permissions
                .permissions
                .get(&ndc_node.model.data_type)
                .ok_or_else(|| {
                    DataFusionError::Plan(format!(
                        "role {} has permission to select model {} but does not have permission \
                            to select fields of the model's underlying type {}",
                        self.session.role, ndc_node.model.name, ndc_node.model.data_type
                    ))
                })?
                .output
                .allowed_fields;
            for (field_name, _field) in &ndc_node.fields {
                let field_name = {
                    let field_name = Identifier::new(field_name.as_str()).map_err(|e| {
                        DataFusionError::Internal(format!(
                            "field name conversion failed {field_name}: {e}"
                        ))
                    })?;
                    FieldName::new(field_name)
                };
                if base_type_allowed_fields.contains(&field_name) {
                    Ok(())
                } else {
                    Err(DataFusionError::Plan(format!(
                            "role {} does not have permission to select the field {} from type {} of model {}",
                            self.session.role, field_name, ndc_node.model.data_type, ndc_node.model.name
                        )))
                }?;
            }

            let mut usage_counts = ir::UsagesCounts::default();
            let mut relationships = BTreeMap::new();

            let permission_filter = match &ndc_node.permission.filter {
                FilterPermission::AllowAll => Ok::<_, DataFusionError>(None),
                FilterPermission::Filter(filter) => {
                    let filter_ir = ir::process_model_predicate(
                        &ndc_node.model_source.data_connector,
                        &ndc_node.model_source.type_mappings,
                        filter,
                        &self.session.variables,
                        &mut usage_counts,
                    )
                    .map_err(|e| {
                        DataFusionError::Internal(format!(
                            "error when processing model predicate: {e}"
                        ))
                    })?;

                    let filter_plan =
                        execute::plan::plan_expression(&filter_ir, &mut relationships).map_err(
                            |e| {
                                DataFusionError::Internal(format!(
                                    "error constructing permission filter plan: {e}"
                                ))
                            },
                        )?;
                    // TODO: this thing has to change, need to be pushed into the
                    // execution plan. We shouldn't be running this in the planning phase
                    let filter =
                        execute::plan::resolve_expression(filter_plan, &self.http_context.clone())
                            .await
                            .map_err(|e| {
                                DataFusionError::Internal(format!(
                                    "error resolving permission filter plan: {e}"
                                ))
                            })?;
                    Ok(Some(filter))
                }
            }?;

            // ndc_node.filter = permission_filter;
            // ndc_node.collection_relationships = relationships;
            let ndc_pushdown = NDCPushDown::new(
                self.http_context.clone(),
                ndc_node.schema.inner().clone(),
                ndc_node.model_source.collection.clone(),
                ndc_node.arguments.clone(),
                ndc_node.fields.clone(),
                permission_filter,
                relationships,
                Arc::new(ndc_node.model_source.data_connector.clone()),
            );
            Ok(Some(Arc::new(ndc_pushdown)))
        } else {
            Ok(None)
        }
    }
}
