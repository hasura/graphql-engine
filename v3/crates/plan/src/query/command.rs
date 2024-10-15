use indexmap::IndexMap;
use std::collections::BTreeMap;
use std::sync::Arc;

use super::field_selection;
use crate::PlanError;
use crate::{NDCFunction, NDCProcedure};
use execute::ndc::FUNCTION_IR_VALUE_COLUMN_NAME;
use execute::plan::{
    field::{NestedArray, NestedField, ResolvedField, ResolvedNestedField},
    Argument, MutationArgument, ResolvedMutationExecutionPlan, ResolvedQueryExecutionPlan,
    ResolvedQueryNode,
};
use hasura_authn_core::Session;
use metadata_resolve::{
    unwrap_custom_type_name, Metadata, Qualified, QualifiedBaseType, QualifiedTypeName,
    QualifiedTypeReference,
};
use open_dds::query::{CommandSelection, ObjectSubSelection};
use open_dds::{
    commands::DataConnectorCommand,
    data_connector::{CollectionName, DataConnectorColumnName},
    types::CustomTypeName,
};
use plan_types::NdcFieldAlias;

#[derive(Debug)]
pub enum CommandPlan {
    Function(NDCFunction),
    Procedure(NDCProcedure),
}

pub struct FromCommand {
    pub command_plan: CommandPlan,
    pub output_object_type_name: Qualified<CustomTypeName>,
    pub extract_response_from: Option<DataConnectorColumnName>,
}

pub fn from_command(
    command_selection: &CommandSelection,
    metadata: &Metadata,
    session: &Arc<Session>,
    request_headers: &reqwest::header::HeaderMap,
) -> Result<FromCommand, PlanError> {
    let command_target = &command_selection.target;
    let qualified_command_name = metadata_resolve::Qualified::new(
        command_target.subgraph.clone(),
        command_target.command_name.clone(),
    );

    let command = metadata
        .commands
        .get(&qualified_command_name)
        .ok_or_else(|| {
            PlanError::Internal(format!(
                "command {qualified_command_name} not found in metadata"
            ))
        })?;

    let command_source = command.command.source.as_ref().ok_or_else(|| {
        PlanError::Internal(format!("command {qualified_command_name} has no source"))
    })?;

    let output_object_type_name = unwrap_custom_type_name(&command.command.output_type).unwrap();

    let metadata_resolve::TypeMapping::Object { field_mappings, .. } = command_source
            .type_mappings
            .get(output_object_type_name)
            .ok_or_else(|| {
                PlanError::Internal(format!(
                    "couldn't fetch type_mapping of type {output_object_type_name} for command {qualified_command_name}",
                ))
            })?;

    let output_object_type = metadata
        .object_types
        .get(output_object_type_name)
        .ok_or_else(|| {
            PlanError::Internal(format!(
                "object type {output_object_type_name} not found in metadata",
            ))
        })?;

    let type_permissions = output_object_type
        .type_output_permissions
        .get(&session.role)
        .ok_or_else(|| {
            PlanError::Permission(format!(
                "role {} does not have permission to select any fields of command {}",
                session.role, qualified_command_name
            ))
        })?;

    let mut ndc_fields = IndexMap::new();

    for (field_alias, object_sub_selection) in command_selection.selection.iter().flatten() {
        let ObjectSubSelection::Field(field_selection) = object_sub_selection else {
            return Err(PlanError::Internal(
                "only normal field selections are supported in NDCPushDownPlanner.".to_string(),
            ));
        };
        if !type_permissions
            .allowed_fields
            .contains(&field_selection.target.field_name)
        {
            return Err(PlanError::Permission(format!(
                "role {} does not have permission to select the field {} from type {} of command {}",
                session.role,
                field_selection.target.field_name,
                &output_object_type_name,
                qualified_command_name
            )));
        }

        let field_mapping = field_mappings
            .get(&field_selection.target.field_name)
            // .map(|field_mapping| field_mapping.column.clone())
            .ok_or_else(|| {
                PlanError::Internal(format!(
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
                PlanError::Internal(format!(
                    "could not look up type of field {}",
                    field_selection.target.field_name
                ))
            })?
            .field_type;

        let fields = field_selection::ndc_nested_field_selection_for(
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
        Err(PlanError::Permission(format!(
            "role {} does not have permission for command {}",
            session.role, qualified_command_name
        )))?;
    };

    let mut ndc_arguments = BTreeMap::new();
    for (argument_name, argument_value) in &command_selection.target.arguments {
        let ndc_argument_name = command_source.argument_mappings.get(argument_name).ok_or_else(|| PlanError::Internal(format!("couldn't fetch argument mapping for argument {argument_name} of command {qualified_command_name}")))?;
        let ndc_argument_value = match argument_value {
            open_dds::query::Value::BooleanExpression(_) => {
                return Err(PlanError::Internal(format!("unexpected boolean expression as value for argument {argument_name} of command {qualified_command_name}")));
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
    .map_err(|e| PlanError::External(Box::new(e)))?
    {
        ndc_arguments.insert(argument_name, value);
    }

    let output_shape = return_type_shape(&command.command.output_type).ok_or_else(|| {
        PlanError::Internal(
            "Expected object or array of objects as command return type".to_string(),
        )
    })?;

    let command_plan = match &command_source.source {
        DataConnectorCommand::Function(function_name) => CommandPlan::Function(NDCFunction {
            function_name: function_name.clone(),
            arguments: ndc_arguments,
            data_connector: command_source.data_connector.clone(),
            fields: wrap_function_ndc_fields(&output_shape, ndc_fields),
            collection_relationships: BTreeMap::new(),
        }),
        DataConnectorCommand::Procedure(procedure_name) => CommandPlan::Procedure(NDCProcedure {
            procedure_name: procedure_name.clone(),
            arguments: ndc_arguments,
            data_connector: command_source.data_connector.clone(),
            fields: Some(wrap_procedure_ndc_fields(&output_shape, ndc_fields)),
            collection_relationships: BTreeMap::new(),
        }),
    };

    Ok(FromCommand {
        command_plan,
        output_object_type_name: output_object_type_name.clone(),
        extract_response_from,
    })
}

fn wrap_procedure_ndc_fields(
    output_shape: &OutputShape,
    ndc_fields: IndexMap<NdcFieldAlias, ResolvedField>,
) -> ResolvedNestedField {
    match output_shape {
        OutputShape::Object => {
            NestedField::Object(execute::plan::field::NestedObject { fields: ndc_fields })
        }
        OutputShape::ListOfObjects => {
            let nested_fields =
                NestedField::Object(execute::plan::field::NestedObject { fields: ndc_fields });
            NestedField::Array(NestedArray {
                fields: Box::new(nested_fields),
            })
        }
    }
}

// temp to keep the ship afloat
enum OutputShape {
    Object,
    ListOfObjects,
}

fn wrap_function_ndc_fields(
    output_shape: &OutputShape,
    ndc_fields: IndexMap<NdcFieldAlias, ResolvedField>,
) -> IndexMap<NdcFieldAlias, ResolvedField> {
    let value_field = match output_shape {
        OutputShape::Object => {
            NestedField::Object(execute::plan::field::NestedObject { fields: ndc_fields })
        }
        OutputShape::ListOfObjects => {
            let nested_fields =
                NestedField::Object(execute::plan::field::NestedObject { fields: ndc_fields });
            NestedField::Array(NestedArray {
                fields: Box::new(nested_fields),
            })
        }
    };
    IndexMap::from([(
        NdcFieldAlias::from(FUNCTION_IR_VALUE_COLUMN_NAME),
        execute::plan::field::Field::Column {
            column: open_dds::data_connector::DataConnectorColumnName::from(
                FUNCTION_IR_VALUE_COLUMN_NAME,
            ),
            fields: Some(value_field),
            arguments: BTreeMap::new(),
        },
    )])
}

// The conversion is as follows:
// 1. If the types is a list of objects, then it would be a table of those entities.
// 2. If the type is an object, it would be a table that returns a single row.
// The columns of the table are the fields of the type.
// 3. If the type is anything else, it'll be a table that returns one row
// and one column named 'result' (TODO)
//
// This is somewhat a duplicate of a similar function in the `sql` catalog, but with the catalog
// specific parts removed. We should consider bringing them together if possible.
fn return_type_shape(output_type: &QualifiedTypeReference) -> Option<OutputShape> {
    match &output_type.underlying_type {
        QualifiedBaseType::Named(QualifiedTypeName::Inbuilt(_)) => None,
        QualifiedBaseType::Named(QualifiedTypeName::Custom(_custom_type)) => {
            Some(OutputShape::Object)
        }
        QualifiedBaseType::List(_element_type) => Some(OutputShape::ListOfObjects),
    }
}

pub fn execute_plan_from_function(function: &NDCFunction) -> ResolvedQueryExecutionPlan {
    ResolvedQueryExecutionPlan {
        query_node: ResolvedQueryNode {
            fields: Some(
                function
                    .fields
                    .iter()
                    .map(|(field_name, field)| (field_name.clone(), field.clone()))
                    .collect(),
            ),
            aggregates: None,
            limit: None,
            offset: None,
            order_by: None,
            predicate: None,
        },
        collection: CollectionName::from(function.function_name.as_str()),
        arguments: function
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
        collection_relationships: function.collection_relationships.clone(),
        variables: None,
        data_connector: function.data_connector.clone(),
    }
}

pub fn execute_plan_from_procedure(procedure: &NDCProcedure) -> ResolvedMutationExecutionPlan {
    ResolvedMutationExecutionPlan {
        procedure_name: procedure.procedure_name.clone(),
        procedure_arguments: procedure
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
        procedure_fields: procedure.fields.clone(),
        collection_relationships: procedure.collection_relationships.clone(),
        data_connector: procedure.data_connector.clone(),
    }
}
