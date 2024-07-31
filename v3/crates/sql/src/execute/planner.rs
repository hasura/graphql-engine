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

pub(crate) struct NDCQueryPlanner {
    pub(crate) default_schema: Option<Arc<String>>,
    pub(crate) catalog: Arc<crate::catalog::OpenDDCatalogProvider>,
}

#[async_trait]
impl QueryPlanner for NDCQueryPlanner {
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
                default_schema: self.default_schema.clone(),
                catalog: self.catalog.clone(),
            })]);
        // Delegate most work of physical planning to the default physical planner
        physical_planner
            .create_physical_plan(logical_plan, session_state)
            .await
    }
}

pub(crate) struct NDCPushDownPlanner {
    pub(crate) default_schema: Option<Arc<String>>,
    pub(crate) catalog: Arc<crate::catalog::OpenDDCatalogProvider>,
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
            let table = self
                .catalog
                .get(
                    self.default_schema.as_ref().map(|s| s.as_str()),
                    &ndc_node.table,
                )
                .ok_or_else(|| {
                    DataFusionError::Internal(format!(
                        "table provider not found for replace_table_scan: {}",
                        &ndc_node.table
                    ))
                })?;
            let model_source = table.source.as_ref().ok_or_else(|| {
                DataFusionError::Plan(format!(
                    "model source should be configured for {}",
                    table.name
                ))
            })?;
            let select_permission = table.select_permission.as_ref().ok_or_else(|| {
                DataFusionError::Plan(format!(
                    "role {} does not have select permission for model {}",
                    table.session.role, table.name
                ))
            })?;
            let type_permissions = table.type_permissions.as_ref().ok_or_else(|| {
                DataFusionError::Plan(format!(
                    "role {} does not have permission to select any fields of model {}",
                    table.session.role, table.name
                ))
            })?;
            let base_type_allowed_fields = &type_permissions
                .permissions
                .get(&table.data_type)
                .ok_or_else(|| {
                    DataFusionError::Plan(format!(
                        "role {} has permission to select model {} but does not have permission \
                            to select fields of the model's underlying type {}",
                        table.session.role, table.name, table.data_type
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
                        table.session.role, field_name, table.data_type, table.name
                    )))
                }?;
            }

            let mut usage_counts = ir::UsagesCounts::default();
            let mut relationships = BTreeMap::new();

            let permission_filter = match &select_permission.filter {
                FilterPermission::AllowAll => Ok::<_, DataFusionError>(None),
                FilterPermission::Filter(filter) => {
                    let filter_ir = ir::process_model_predicate(
                        &model_source.data_connector,
                        &model_source.type_mappings,
                        filter,
                        &table.session.variables,
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
                    let filter =
                        execute::plan::resolve_expression(filter_plan, &table.http_context.clone())
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
                table.http_context.clone(),
                ndc_node.schema.inner().clone(),
                ndc_node.data_source_name.as_ref().clone(),
                ndc_node.fields.clone(),
                permission_filter,
                relationships,
                Arc::new(model_source.data_connector.clone()),
            );
            Ok(Some(Arc::new(ndc_pushdown)))
        } else {
            Ok(None)
        }
    }
}
