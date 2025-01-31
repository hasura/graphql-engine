use super::arguments::{get_unresolved_arguments, resolve_arguments};
use super::{field_selection, process_argument_presets_for_command};
use crate::PlanError;
use hasura_authn_core::Session;
use indexmap::IndexMap;
use metadata_resolve::{
    Metadata, Qualified, QualifiedBaseType, QualifiedTypeName, QualifiedTypeReference,
};
use open_dds::commands::CommandName;
use open_dds::query::CommandSelection;
use open_dds::{
    commands::DataConnectorCommand,
    data_connector::{CollectionName, DataConnectorColumnName},
    types::CustomTypeName,
};
use plan_types::{
    Argument, ExecutionTree, Field, JoinLocations, MutationArgument, MutationExecutionPlan,
    NdcFieldAlias, NdcRelationshipName, NestedArray, NestedField, NestedObject,
    PredicateQueryTrees, QueryExecutionPlan, QueryNodeNew, Relationship,
};
use plan_types::{UniqueNumber, FUNCTION_IR_VALUE_COLUMN_NAME};
use std::collections::BTreeMap;

#[derive(Debug)]
pub enum CommandPlan {
    Function(ExecutionTree),
    Procedure(MutationExecutionPlan),
}

pub struct FromCommand {
    pub command_plan: CommandPlan,
    pub extract_response_from: Option<DataConnectorColumnName>,
}

pub fn from_command(
    command_selection: &CommandSelection,
    metadata: &Metadata,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    unique_number: &mut UniqueNumber,
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

    from_command_selection(
        command_selection,
        metadata,
        session,
        request_headers,
        &qualified_command_name,
        command,
        command_source,
        unique_number,
    )
}

fn from_command_output_type(
    output_shape: &OutputShape,
    command_selection: &CommandSelection,
    metadata: &Metadata,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    command_source: &metadata_resolve::CommandSource,
    relationships: &mut BTreeMap<NdcRelationshipName, Relationship>,
    remote_join_executions: &mut JoinLocations,
    remote_predicates: &mut PredicateQueryTrees,
    unique_number: &mut UniqueNumber,
) -> Result<
    (
        IndexMap<NdcFieldAlias, Field>,
        Option<DataConnectorColumnName>,
    ),
    PlanError,
> {
    match &output_shape {
        OutputShape::ScalarType { .. } => Ok((IndexMap::new(), None)),
        OutputShape::Array { inner } => from_command_output_type(
            inner,
            command_selection,
            metadata,
            session,
            request_headers,
            command_source,
            relationships,
            remote_join_executions,
            remote_predicates,
            unique_number,
        ),
        OutputShape::Object {
            object: output_object_type,
            object_name: output_object_type_name,
        } => {
            let command_selection_set = match &command_selection.selection {
                Some(selection_set) => selection_set,
                None => &IndexMap::new(),
            };
            let ndc_fields = field_selection::resolve_field_selection(
                metadata,
                session,
                request_headers,
                output_object_type_name,
                output_object_type,
                &command_source.type_mappings,
                &command_source.data_connector,
                command_selection_set,
                relationships,
                remote_join_executions,
                remote_predicates,
                unique_number,
            )?;

            match &command_source.data_connector.response_config {
                // if the data connector has 'responseHeaders' configured, we'll need to wrap the ndc fields
                // under the 'result' field if the command's response at opendd layer refers to the 'result'
                // field's type. Note that we aren't requesting the 'header's field as we don't forward the
                // response headers in the SQL layer yet
                Some(response_config) if !command_source.ndc_type_opendd_type_same => {
                    let result_field_name =
                        NdcFieldAlias::from(response_config.result_field.as_str());
                    let result_field = Field::Column {
                        column: response_config.result_field.clone(),
                        fields: Some(NestedField::Object(NestedObject { fields: ndc_fields })),
                        arguments: BTreeMap::new(),
                    };
                    let fields = IndexMap::from_iter([(result_field_name, result_field)]);
                    Ok((fields, Some(response_config.result_field.clone())))
                }
                _ => Ok((ndc_fields, None)),
            }
        }
    }
}

pub(crate) fn from_command_selection(
    command_selection: &CommandSelection,
    metadata: &Metadata,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    qualified_command_name: &Qualified<CommandName>,
    command: &metadata_resolve::CommandWithPermissions,
    command_source: &metadata_resolve::CommandSource,
    unique_number: &mut UniqueNumber,
) -> Result<FromCommand, PlanError> {
    let mut relationships = BTreeMap::new();
    let mut usage_counts = plan_types::UsagesCounts::default();
    let mut remote_join_executions = JoinLocations::new();
    let mut remote_predicates = PredicateQueryTrees::new();

    let output_shape = return_type_shape(&command.command.output_type, metadata)?;

    let (ndc_fields, extract_response_from) = from_command_output_type(
        &output_shape,
        command_selection,
        metadata,
        session,
        request_headers,
        command_source,
        &mut relationships,
        &mut remote_join_executions,
        &mut remote_predicates,
        unique_number,
    )?;

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

    // resolve arguments, adding in presets
    let unresolved_arguments = get_unresolved_arguments(
        &command_selection.target.arguments,
        &command.command.arguments,
        &command_source.argument_mappings,
        metadata,
        &command_source.type_mappings,
        &command_source.data_connector,
    )?;

    // add any preset arguments from model permissions
    let unresolved_arguments = process_argument_presets_for_command(
        unresolved_arguments,
        command,
        &metadata.object_types,
        session,
        request_headers,
        &mut usage_counts,
    )
    .map_err(|e| PlanError::Internal(e.to_string()))?;
    let resolved_arguments =
        resolve_arguments(unresolved_arguments, &mut relationships, unique_number)?;

    let command_plan = match &command_source.source {
        DataConnectorCommand::Function(function_name) => CommandPlan::Function(ExecutionTree {
            remote_predicates,
            remote_join_executions,
            query_execution_plan: QueryExecutionPlan {
                query_node: QueryNodeNew {
                    fields: Some(plan_types::FieldsSelection {
                        fields: wrap_scalar_select(wrap_function_ndc_field(
                            &output_shape,
                            ndc_fields,
                        )),
                    }),
                    aggregates: None,
                    limit: None,
                    offset: None,
                    order_by: None,
                    predicate: None,
                    group_by: None,
                },
                collection: CollectionName::from(function_name.as_str()),
                arguments: resolved_arguments,
                collection_relationships: relationships.clone(),
                variables: None,
                data_connector: command_source.data_connector.clone(),
            },
        }),
        DataConnectorCommand::Procedure(procedure_name) => {
            CommandPlan::Procedure(MutationExecutionPlan {
                procedure_name: procedure_name.clone(),
                procedure_arguments: resolved_arguments
                    .into_iter()
                    .map(|(name, argument)| {
                        (
                            name,
                            match argument {
                                Argument::Literal { value } => MutationArgument::Literal { value },
                                Argument::BooleanExpression { predicate } => {
                                    MutationArgument::BooleanExpression { predicate }
                                }
                                Argument::Variable { name: _ } => {
                                    todo!("variable in mutation argument")
                                }
                            },
                        )
                    })
                    .collect(),
                procedure_fields: Some(wrap_procedure_ndc_fields(&output_shape, ndc_fields)),
                collection_relationships: relationships.clone(),
                data_connector: command_source.data_connector.clone(),
            })
        }
    };
    Ok(FromCommand {
        command_plan,
        extract_response_from,
    })
}

fn wrap_procedure_ndc_fields(
    output_shape: &OutputShape,
    ndc_fields: IndexMap<NdcFieldAlias, Field>,
) -> NestedField {
    match output_shape {
        OutputShape::Object { .. } => NestedField::Object(NestedObject { fields: ndc_fields }),
        OutputShape::Array { inner } => {
            let nested_fields = wrap_procedure_ndc_fields(inner, ndc_fields);
            NestedField::Array(NestedArray {
                fields: Box::new(nested_fields),
            })
        }
        OutputShape::ScalarType { .. } => NestedField::Object(NestedObject {
            fields: wrap_scalar_select(None),
        }),
    }
}

enum OutputShape {
    Object {
        object_name: Qualified<CustomTypeName>,
        object: metadata_resolve::ObjectTypeWithRelationships,
    },
    Array {
        inner: Box<OutputShape>,
    },
    ScalarType {
        _custom_scalar_type: Option<metadata_resolve::ScalarTypeRepresentation>,
    },
}

fn wrap_function_ndc_field(
    output_shape: &OutputShape,
    ndc_fields: IndexMap<NdcFieldAlias, Field>,
) -> Option<NestedField> {
    match output_shape {
        OutputShape::Object { .. } => {
            Some(NestedField::Object(NestedObject { fields: ndc_fields }))
        }
        OutputShape::Array { inner } => {
            let nested_fields = wrap_function_ndc_field(inner, ndc_fields)?;
            Some(NestedField::Array(NestedArray {
                fields: Box::new(nested_fields),
            }))
        }
        OutputShape::ScalarType { .. } => None,
    }
}

fn wrap_scalar_select(nested_fields: Option<NestedField>) -> IndexMap<NdcFieldAlias, Field> {
    IndexMap::from([(
        NdcFieldAlias::from(FUNCTION_IR_VALUE_COLUMN_NAME),
        Field::Column {
            column: open_dds::data_connector::DataConnectorColumnName::from(
                FUNCTION_IR_VALUE_COLUMN_NAME,
            ),
            fields: nested_fields,
            arguments: BTreeMap::new(),
        },
    )])
}

// The conversion is as follows:
// 1. If the types is a list of objects, then it would be a table of those entities.
// 2. If the type is an object, it would be a table that returns a single row.
// The columns of the table are the fields of the type.
// 3. If the type is anything else, it'll be a table that returns one row
// and one column named 'result'
//
// This is somewhat a duplicate of a similar function in the `sql` catalog, but with the catalog
// specific parts removed. We should consider bringing them together if possible.
fn return_type_shape(
    output_type: &QualifiedTypeReference,
    metadata: &Metadata,
) -> Result<OutputShape, PlanError> {
    match &output_type.underlying_type {
        QualifiedBaseType::Named(QualifiedTypeName::Inbuilt(_)) => Ok(OutputShape::ScalarType {
            _custom_scalar_type: None,
        }),
        QualifiedBaseType::Named(QualifiedTypeName::Custom(custom_type)) => {
            match metadata.object_types.get(custom_type) {
                Some(output_object_type) => Ok(OutputShape::Object {
                    object_name: custom_type.clone(),
                    object: output_object_type.clone(),
                }),
                None => match metadata.scalar_types.get(custom_type) {
                    Some(output_scalar_type) => Ok(OutputShape::ScalarType {
                        _custom_scalar_type: Some(output_scalar_type.clone()),
                    }),
                    None => Err(PlanError::Internal(format!(
                        "Did not recognise type {custom_type}"
                    ))),
                },
            }
        }
        QualifiedBaseType::List(type_reference) => {
            let inner = return_type_shape(type_reference, metadata)?;
            Ok(OutputShape::Array {
                inner: Box::new(inner),
            })
        }
    }
}
