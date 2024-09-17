use datafusion::{
    common::{internal_err, plan_err, DFSchemaRef},
    error::{DataFusionError, Result},
    physical_plan::ExecutionPlan,
};
use indexmap::IndexMap;
use std::{collections::BTreeMap, sync::Arc};

use execute::plan::ResolvedField;
use graphql_ir::NdcFieldAlias;
use hasura_authn_core::Session;
use open_dds::query::{CommandSelection, ObjectSubSelection};

mod function;
mod procedure;
pub(crate) use function::CommandOutput;
pub(crate) use function::NDCFunctionPushDown;
use open_dds::commands::DataConnectorCommand;
pub(crate) use procedure::NDCProcedurePushDown;

pub fn build_execution_plan(
    request_headers: &reqwest::header::HeaderMap,
    metadata: &metadata_resolve::Metadata,
    http_context: &Arc<execute::HttpContext>,
    session: &Arc<Session>,
    command_selection: &CommandSelection,
    // schema of the output of the command selection
    schema: &DFSchemaRef,
    output: &CommandOutput,
) -> Result<Arc<dyn ExecutionPlan>> {
    let command_target = &command_selection.target;
    let qualified_command_name = metadata_resolve::Qualified::new(
        command_target.subgraph.clone(),
        command_target.command_name.clone(),
    );

    let command = metadata
        .commands
        .get(&qualified_command_name)
        .ok_or_else(|| {
            DataFusionError::Internal(format!(
                "command {qualified_command_name} not found in metadata"
            ))
        })?;

    let command_source = command.command.source.as_ref().ok_or_else(|| {
        DataFusionError::Internal(format!("command {qualified_command_name} has no source"))
    })?;

    let output_object_type_name = match &output {
        CommandOutput::ListOfObjects(ty) | CommandOutput::Object(ty) => ty,
    };

    let metadata_resolve::TypeMapping::Object { field_mappings, .. } = command_source
            .type_mappings
            .get(output_object_type_name)
            .ok_or_else(|| {
                DataFusionError::Internal(format!(
                    "couldn't fetch type_mapping of type {output_object_type_name} for command {qualified_command_name}",
                ))
            })?;

    let output_object_type = metadata
        .object_types
        .get(output_object_type_name)
        .ok_or_else(|| {
            DataFusionError::Internal(format!(
                "object type {output_object_type_name} not found in metadata",
            ))
        })?;

    let type_permissions = output_object_type
        .type_output_permissions
        .get(&session.role)
        .ok_or_else(|| {
            DataFusionError::Plan(format!(
                "role {} does not have permission to select any fields of command {}",
                session.role, qualified_command_name
            ))
        })?;

    let mut ndc_fields = IndexMap::new();

    for (field_alias, object_sub_selection) in command_selection.selection.iter().flatten() {
        let ObjectSubSelection::Field(field_selection) = object_sub_selection else {
            return internal_err!(
                "only normal field selections are supported in NDCPushDownPlanner."
            );
        };
        if !type_permissions
            .allowed_fields
            .contains(&field_selection.target.field_name)
        {
            return plan_err!(
                "role {} does not have permission to select the field {} from type {} of command {}",
                session.role,
                field_selection.target.field_name,
                &output_object_type_name,
                qualified_command_name
            );
        }

        let field_mapping = field_mappings
            .get(&field_selection.target.field_name)
            // .map(|field_mapping| field_mapping.column.clone())
            .ok_or_else(|| {
                DataFusionError::Internal(format!(
                    "couldn't fetch field mapping of field {} in type {} for command {}",
                    field_selection.target.field_name,
                    output_object_type_name,
                    qualified_command_name
                ))
            })?;

        let field_type = &output_object_type
            .object_type
            .fields
            .get(&field_selection.target.field_name)
            .ok_or_else(|| {
                DataFusionError::Internal(format!(
                    "could not look up type of field {}",
                    field_selection.target.field_name
                ))
            })?
            .field_type;

        let fields = crate::execute::planner::model::ndc_nested_field_selection_for(
            metadata,
            field_type,
            &command_source.type_mappings,
        )?;

        let ndc_field = ResolvedField::Column {
            column: field_mapping.column.clone(),
            fields,
            arguments: BTreeMap::new(),
        };

        ndc_fields.insert(NdcFieldAlias::from(field_alias.as_str()), ndc_field);
    }

    let (ndc_fields, extract_response_from) = match &command_source.data_connector.response_config {
        // if the data connector has 'responseHeaders' configured, we'll need to wrap the ndc fields
        // under the 'result' field if the command's response at opendd layer refers to the 'result'
        // field's type. Note that we aren't requesting the 'header's field as we don't forward the
        // response headers in the SQL layer yet
        Some(response_config) if !command_source.ndc_type_opendd_type_same => {
            let result_field_name = NdcFieldAlias::from(response_config.result_field.as_str());
            let result_field = ResolvedField::Column {
                column: response_config.result_field.clone(),
                fields: Some(execute::plan::field::NestedField::Object(
                    execute::plan::field::NestedObject { fields: ndc_fields },
                )),
                arguments: BTreeMap::new(),
            };
            let fields = IndexMap::from_iter([(result_field_name, result_field)]);
            (fields, Some(response_config.result_field.clone()))
        }
        _ => (ndc_fields, None),
    };

    if !command
        .permissions
        .get(&session.role)
        .map_or(false, |permission| permission.allow_execution)
    {
        Err(DataFusionError::Plan(format!(
            "role {} does not have permission for command {}",
            session.role, qualified_command_name
        )))?;
    };

    let mut ndc_arguments = BTreeMap::new();
    for (argument_name, argument_value) in &command_selection.target.arguments {
        let ndc_argument_name = command_source.argument_mappings.get(argument_name).ok_or_else(|| DataFusionError::Internal(format!("couldn't fetch argument mapping for argument {argument_name} of command {qualified_command_name}")))?;
        let ndc_argument_value = match argument_value {
            open_dds::query::Value::BooleanExpression(_) => {
                return internal_err!("unexpected boolean expression as value for argument {argument_name} of command {qualified_command_name}");
            }
            open_dds::query::Value::Literal(value) => value,
        };
        ndc_arguments.insert(ndc_argument_name.clone(), ndc_argument_value.clone());
    }

    // preset arguments from `DataConnectorLink` argument presets
    for (argument_name, value) in graphql_ir::process_connector_link_presets(
        &command_source.data_connector_link_argument_presets,
        &session.variables,
        request_headers,
    )
    .map_err(|e| DataFusionError::External(Box::new(e)))?
    {
        ndc_arguments.insert(argument_name, value);
    }

    match &command_source.source {
        DataConnectorCommand::Function(function_name) => {
            let ndc_pushdown = NDCFunctionPushDown::new(
                http_context.clone(),
                Arc::new(command_source.data_connector.clone()),
                function_name.clone(),
                ndc_arguments,
                ndc_fields,
                schema,
                output.clone(),
                extract_response_from,
            );
            Ok(Arc::new(ndc_pushdown))
        }
        DataConnectorCommand::Procedure(procedure_name) => {
            let ndc_pushdown = NDCProcedurePushDown::new(
                http_context.clone(),
                Arc::new(command_source.data_connector.clone()),
                procedure_name.clone(),
                ndc_arguments,
                ndc_fields,
                schema,
                output.clone(),
                extract_response_from,
            );
            Ok(Arc::new(ndc_pushdown))
        }
    }
}
