mod command_permission;
use indexmap::IndexMap;

use open_dds::{
    commands::CommandName, data_connector::DataConnectorName, models::ModelName,
    types::CustomTypeName,
};

use crate::stages::{
    boolean_expressions, commands, data_connector_scalar_types, models_graphql,
    object_relationships, scalar_types,
};
use crate::types::error::Error;
use crate::types::subgraph::Qualified;

use std::collections::BTreeMap;
mod types;
pub use types::{CommandPermissionIssue, CommandPermissionsOutput, CommandWithPermissions};

/// resolve command permissions
pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    commands: &IndexMap<Qualified<CommandName>, commands::Command>,
    object_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_relationships::ObjectTypeWithRelationships,
    >,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
    models: &IndexMap<Qualified<ModelName>, models_graphql::ModelWithGraphql>,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars,
    >,
) -> Result<CommandPermissionsOutput, Error> {
    let mut issues = Vec::new();
    let mut commands_with_permissions: IndexMap<Qualified<CommandName>, CommandWithPermissions> =
        commands
            .iter()
            .map(|(command_name, command)| {
                (
                    command_name.clone(),
                    CommandWithPermissions {
                        command: command.clone(),
                        permissions: BTreeMap::new(),
                    },
                )
            })
            .collect();

    for open_dds::accessor::QualifiedObject {
        path: _,
        subgraph,
        object: command_permissions,
    } in &metadata_accessor.command_permissions
    {
        let command_name = &command_permissions.command_name;
        let qualified_command_name = Qualified::new(subgraph.clone(), command_name.to_owned());
        let command = commands_with_permissions
            .get_mut(&qualified_command_name)
            .ok_or_else(|| Error::UnknownCommandInCommandPermissions {
                command_name: qualified_command_name.clone(),
            })?;
        if command.permissions.is_empty() {
            command.permissions = command_permission::resolve_command_permissions(
                &metadata_accessor.flags,
                &command.command,
                command_permissions,
                object_types,
                scalar_types,
                boolean_expression_types,
                models,
                data_connector_scalars,
                subgraph,
                &mut issues,
            )?;
        } else {
            return Err(Error::DuplicateCommandPermission {
                command_name: qualified_command_name.clone(),
            });
        }
    }
    Ok(CommandPermissionsOutput {
        permissions: commands_with_permissions,
        issues,
    })
}
