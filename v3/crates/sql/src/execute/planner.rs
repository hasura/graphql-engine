use hasura_authn_core::Session;
use indexmap::IndexMap;
use ir::NdcFieldAlias;
use std::{collections::BTreeMap, sync::Arc};

use datafusion::{
    common::{internal_err, plan_err},
    error::{DataFusionError, Result},
    execution::context::{QueryPlanner, SessionState},
    logical_expr::{LogicalPlan, UserDefinedLogicalNode},
    physical_plan::ExecutionPlan,
    physical_planner::{DefaultPhysicalPlanner, ExtensionPlanner, PhysicalPlanner},
};

use metadata_resolve::FilterPermission;
use open_dds::query::ObjectSubSelection;

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
        if let Some(model_query) = node.as_any().downcast_ref::<crate::plan::ModelQuery>() {
            assert_eq!(logical_inputs.len(), 0, "Inconsistent number of inputs");
            assert_eq!(physical_inputs.len(), 0, "Inconsistent number of inputs");

            let model_target = &model_query.model_selection.target;
            let qualified_model_name = metadata_resolve::Qualified::new(
                model_target.subgraph.clone(),
                model_target.model_name.clone(),
            );

            let model = self
                .catalog
                .metadata
                .models
                .get(&qualified_model_name)
                .ok_or_else(|| {
                    DataFusionError::Internal(format!(
                        "model {qualified_model_name} not found in metadata"
                    ))
                })?;

            let model_source = model.model.source.as_ref().ok_or_else(|| {
                DataFusionError::Internal(format!("model {qualified_model_name} has no source"))
            })?;

            let metadata_resolve::TypeMapping::Object { field_mappings, .. } = model_source
                .type_mappings
                .get(&model.model.data_type)
                .ok_or_else(|| {
                    DataFusionError::Internal(format!(
                        "couldn't fetch type_mapping of type {} for model {}",
                        model.model.data_type, qualified_model_name
                    ))
                })?;

            let model_object_type = self
                .catalog
                .metadata
                .object_types
                .get(&model.model.data_type)
                .ok_or_else(|| {
                    DataFusionError::Internal(format!(
                        "object type {} not found in metadata",
                        model.model.data_type
                    ))
                })?;

            let type_permissions = model_object_type
                .type_output_permissions
                .get(&self.session.role)
                .ok_or_else(|| {
                    DataFusionError::Plan(format!(
                        "role {} does not have permission to select any fields of model {}",
                        self.session.role, qualified_model_name
                    ))
                })?;

            let mut ndc_fields = IndexMap::new();

            for (field_alias, object_sub_selection) in &model_query.model_selection.selection {
                let ObjectSubSelection::Field(field_selection) = object_sub_selection else {
                    return internal_err!(
                        "only normal field selections are supported in NDCPushDownPlanner."
                    );
                };
                if !type_permissions
                    .allowed_fields
                    .contains(&field_selection.target.field_name)
                {
                    return plan_err!("role {} does not have permission to select the field {} from type {} of model {}",
                            self.session.role, field_selection.target.field_name, model.model.data_type, qualified_model_name);
                }

                let ndc_column = field_mappings
                    .get(&field_selection.target.field_name)
                    .map(|field_mapping| field_mapping.column.clone())
                    .ok_or_else(|| {
                        DataFusionError::Internal(format!(
                            "couldn't fetch field mapping of field {} in type {} for model {}",
                            field_selection.target.field_name,
                            model.model.data_type,
                            qualified_model_name
                        ))
                    })?;

                ndc_fields.insert(NdcFieldAlias::from(field_alias.as_str()), ndc_column);
            }

            let model_select_permission = model
                .select_permissions
                .get(&self.session.role)
                .ok_or_else(|| {
                    DataFusionError::Plan(format!(
                        "role {} does not have select permission for model {}",
                        self.session.role, qualified_model_name
                    ))
                })?;

            let mut usage_counts = ir::UsagesCounts::default();
            let mut relationships: BTreeMap<ir::NdcRelationshipName, execute::plan::Relationship> =
                BTreeMap::new();

            let permission_filter = match &model_select_permission.filter {
                FilterPermission::AllowAll => Ok::<_, DataFusionError>(None),
                FilterPermission::Filter(filter) => {
                    let filter_ir = ir::process_model_predicate(
                        &model_source.data_connector,
                        &model_source.type_mappings,
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

            let mut ndc_arguments = BTreeMap::new();
            for (argument_name, argument_value) in &model_query.model_selection.target.arguments {
                let ndc_argument_name = model_source.argument_mappings.get(argument_name).ok_or_else(|| DataFusionError::Internal(format!("couldn't fetch argument mapping for argument {argument_name} of model {qualified_model_name}")))?;
                let ndc_argument_value = match argument_value {
                    open_dds::query::Value::BooleanExpression(_) => {
                        return internal_err!("unexpected boolean expression as value for argument {argument_name} of model {qualified_model_name}");
                    }
                    open_dds::query::Value::Literal(value) => value,
                };
                ndc_arguments.insert(ndc_argument_name.clone(), ndc_argument_value.clone());
            }

            let ndc_pushdown = NDCPushDown::new(
                self.http_context.clone(),
                model_query.schema.inner().clone(),
                model_source.collection.clone(),
                ndc_arguments,
                ndc_fields,
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
