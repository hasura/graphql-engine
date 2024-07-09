mod command;
mod source;
mod types;

use crate::stages::{
    boolean_expressions, data_connectors, object_boolean_expressions, scalar_types,
    type_permissions,
};
use crate::types::error::Error;
use crate::types::subgraph::Qualified;
use indexmap::IndexMap;

use open_dds::commands::CommandName;
pub use types::{Command, CommandSource};

use open_dds::types::CustomTypeName;

use std::collections::BTreeMap;

/// resolve commands
pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    data_connectors: &data_connectors::DataConnectors,
    object_types: &type_permissions::ObjectTypesWithPermissions,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    object_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_boolean_expressions::ObjectBooleanExpressionType,
    >,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
) -> Result<IndexMap<Qualified<CommandName>, Command>, Error> {
    let mut commands: IndexMap<Qualified<CommandName>, Command> = IndexMap::new();
    for open_dds::accessor::QualifiedObject {
        subgraph,
        object: command,
    } in &metadata_accessor.commands
    {
        let mut resolved_command = command::resolve_command(
            command,
            subgraph,
            object_types,
            scalar_types,
            object_boolean_expression_types,
            boolean_expression_types,
        )?;
        if let Some(command_source) = &command.source {
            let command_source = source::resolve_command_source(
                command_source,
                &resolved_command,
                subgraph,
                data_connectors,
                object_types,
                scalar_types,
                object_boolean_expression_types,
                boolean_expression_types,
            )?;
            resolved_command.source = Some(command_source);
        }
        let qualified_command_name = Qualified::new(subgraph.to_string(), command.name.clone());
        if commands
            .insert(qualified_command_name.clone(), resolved_command)
            .is_some()
        {
            return Err(Error::DuplicateCommandDefinition {
                name: qualified_command_name,
            });
        }
    }
    Ok(commands)
}
