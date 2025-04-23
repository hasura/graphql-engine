mod command_permission;
use indexmap::IndexMap;

use open_dds::identifier::SubgraphName;
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
) -> Result<CommandPermissionsOutput, Vec<Error>> {
    let mut issues = Vec::new();
    let mut results = vec![];

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
        results.push(resolve_command_permission(
            metadata_accessor,
            object_types,
            scalar_types,
            boolean_expression_types,
            models,
            data_connector_scalars,
            subgraph,
            command_permissions,
            &mut issues,
            &mut commands_with_permissions,
        ));
    }

    partition_eithers::collect_any_errors(results).map(|_| CommandPermissionsOutput {
        permissions: commands_with_permissions,
        issues,
    })
}

fn resolve_command_permission(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
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
    subgraph: &SubgraphName,
    command_permissions: &open_dds::permissions::CommandPermissionsV1,
    issues: &mut Vec<CommandPermissionIssue>,
    commands_with_permissions: &mut IndexMap<Qualified<CommandName>, CommandWithPermissions>,
) -> Result<(), Error> {
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
            issues,
        )?;
    } else {
        return Err(Error::DuplicateCommandPermission {
            command_name: qualified_command_name.clone(),
        });
    }
    Ok(())
}
