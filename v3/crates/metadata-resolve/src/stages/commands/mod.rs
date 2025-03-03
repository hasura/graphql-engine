use std::sync::Arc;

mod command;
mod error;
mod source;
mod types;
pub use error::CommandsError;
use open_dds::data_connector::DataConnectorName;

use crate::helpers::types::TrackGraphQLRootFields;
use crate::stages::{
    boolean_expressions, data_connector_scalar_types, data_connectors, scalar_types,
    type_permissions,
};
use crate::types::subgraph::Qualified;
use indexmap::IndexMap;

use open_dds::commands::CommandName;
pub use types::{Command, CommandSource, CommandsIssue, CommandsOutput};

use open_dds::types::CustomTypeName;

use std::collections::BTreeMap;

/// resolve commands
pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    data_connectors: &data_connectors::DataConnectors,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars,
    >,
    object_types: &type_permissions::ObjectTypesWithPermissions,
    track_root_fields: &mut TrackGraphQLRootFields,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
) -> Result<CommandsOutput, CommandsError> {
    let mut commands: IndexMap<Qualified<CommandName>, Command> = IndexMap::new();
    let mut issues = vec![];
    for open_dds::accessor::QualifiedObject {
        path: _,
        subgraph,
        object: command,
    } in &metadata_accessor.commands
    {
        let mut resolved_command = command::resolve_command(
            command,
            subgraph,
            object_types,
            track_root_fields,
            scalar_types,
            boolean_expression_types,
            &mut issues,
        )?;
        if let Some(command_source) = &command.source {
            let (command_source, command_source_issues) = source::resolve_command_source(
                command_source,
                &resolved_command,
                subgraph,
                data_connectors,
                data_connector_scalars,
                object_types,
                scalar_types,
                boolean_expression_types,
            )?;
            resolved_command.source = Some(Arc::new(command_source));
            issues.extend(command_source_issues);
        }
        let qualified_command_name = Qualified::new(subgraph.clone(), command.name.clone());
        if commands
            .insert(qualified_command_name.clone(), resolved_command)
            .is_some()
        {
            return Err(CommandsError::DuplicateCommandDefinition {
                name: qualified_command_name,
            });
        }
    }
    Ok(CommandsOutput { commands, issues })
}
