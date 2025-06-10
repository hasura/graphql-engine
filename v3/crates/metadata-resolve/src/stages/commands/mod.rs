use std::sync::Arc;

mod command;
mod error;
mod source;
mod types;
pub use error::CommandsError;
use open_dds::data_connector::DataConnectorName;
use open_dds::identifier::SubgraphName;

use crate::helpers::types::TrackGraphQLRootFields;
use crate::stages::{
    boolean_expressions, data_connector_scalar_types, data_connectors, scalar_types,
    type_permissions,
};
use crate::types::subgraph::Qualified;
use indexmap::IndexMap;

use open_dds::commands::CommandName;
pub use types::{Command, CommandGraphQlApi, CommandSource, CommandsIssue, CommandsOutput};

use open_dds::types::CustomTypeName;

use std::collections::BTreeMap;

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
) -> Result<CommandsOutput, Vec<CommandsError>> {
    let mut commands: IndexMap<Qualified<CommandName>, Command> = IndexMap::new();
    let mut issues = vec![];
    let mut results = vec![];

    for open_dds::accessor::QualifiedObject {
        path: _,
        subgraph,
        object: command,
    } in &metadata_accessor.commands
    {
        results.push(resolve_command(
            subgraph,
            command,
            object_types,
            track_root_fields,
            scalar_types,
            boolean_expression_types,
            data_connectors,
            data_connector_scalars,
            &mut commands,
            &mut issues,
        ));
    }

    partition_eithers::collect_any_errors(results).map(|_| CommandsOutput { commands, issues })
}

/// resolve commands
fn resolve_command(
    subgraph: &SubgraphName,
    command: &open_dds::commands::CommandV1,
    object_types: &type_permissions::ObjectTypesWithPermissions,
    track_root_fields: &mut TrackGraphQLRootFields,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
    data_connectors: &data_connectors::DataConnectors,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars,
    >,
    commands: &mut IndexMap<Qualified<CommandName>, Command>,
    issues: &mut Vec<CommandsIssue>,
) -> Result<(), CommandsError> {
    let mut resolved_command = command::resolve_command(
        command,
        subgraph,
        object_types,
        track_root_fields,
        scalar_types,
        boolean_expression_types,
        issues,
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

    Ok(())
}
