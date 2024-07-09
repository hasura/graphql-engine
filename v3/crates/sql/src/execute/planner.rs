use std::{collections::BTreeMap, sync::Arc};

use datafusion::{
    error::{DataFusionError, Result},
    execution::context::{QueryPlanner, SessionState},
    logical_expr::{LogicalPlan, UserDefinedLogicalNode},
    physical_plan::ExecutionPlan,
    physical_planner::{DefaultPhysicalPlanner, ExtensionPlanner, PhysicalPlanner},
};

use execute::{ndc_expression, process_model_relationship_definition};
use indexmap::IndexMap;
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
            for (field_name, _field) in ndc_node
                .query
                .query
                .fields
                .as_ref()
                .unwrap_or(&IndexMap::new())
            {
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

            let mut usage_counts = execute::model_tracking::UsagesCounts::default();
            let mut relationships = BTreeMap::new();

            let permission_filter = match &select_permission.filter {
                FilterPermission::AllowAll => Ok(None),
                FilterPermission::Filter(filter) => {
                    execute::ir::permissions::process_model_predicate(
                        filter,
                        &table.session.variables,
                        &mut relationships,
                        &mut usage_counts,
                    )
                    .map(Some)
                    .map_err(|e| {
                        DataFusionError::Internal(format!(
                            "error when processing model predicate: {e}"
                        ))
                    })
                }
            }?;

            let relationships = relationships
                .into_values()
                .map(|v| {
                    process_model_relationship_definition(&v)
                        .map(|r| {
                            (
                                ndc_models::RelationshipName::from(v.relationship_name.as_str()),
                                r,
                            )
                        })
                        .map_err(|e| {
                            DataFusionError::Internal(format!(
                                "error constructing ndc relationship definition: {e}"
                            ))
                        })
                })
                .collect::<Result<
                    BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
                    DataFusionError,
                >>()?;
            let mut query = ndc_node.query.clone();
            query.query.predicate = permission_filter.map(|expr| ndc_expression(&expr));
            query.collection_relationships = relationships;
            let ndc_pushdown = NDCPushDown::new(
                table.http_context.clone(),
                ndc_node.schema.inner().clone(),
                Arc::new(query),
                Arc::new(model_source.data_connector.clone()),
            );
            Ok(Some(Arc::new(ndc_pushdown)))
        } else {
            Ok(None)
        }
    }
}
