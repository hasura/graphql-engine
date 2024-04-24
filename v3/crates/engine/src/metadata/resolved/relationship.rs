use super::command::Command;

use super::data_connector::DataConnectorLink;
use super::error::{Error, RelationshipError};
use super::model::get_ndc_column_for_comparison;
use super::model::Model;
use super::stages::data_connector_scalar_types;
use super::subgraph::Qualified;
use super::subgraph::QualifiedTypeReference;
use super::types::mk_name;
use super::types::NdcColumnForComparison;
use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use open_dds::arguments::ArgumentName;
use open_dds::commands::CommandName;

use crate::metadata::resolved::stages::type_permissions;
use open_dds::models::ModelName;
use open_dds::relationships::{
    self, FieldAccess, RelationshipName, RelationshipType, RelationshipV1,
};
use open_dds::types::CustomTypeName;
use open_dds::types::Deprecated;
use serde::{Deserialize, Serialize};

use std::collections::HashSet;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum RelationshipTarget {
    Model {
        // TODO(Abhinav): Refactor resolved types to contain denormalized data (eg: actual resolved model)
        model_name: Qualified<ModelName>,
        relationship_type: RelationshipType,
        target_typename: Qualified<CustomTypeName>,
        mappings: Vec<RelationshipModelMapping>,
    },
    Command {
        command_name: Qualified<CommandName>,
        target_type: QualifiedTypeReference,
        mappings: Vec<RelationshipCommandMapping>,
    },
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum RelationshipTargetName {
    Model(Qualified<ModelName>),
    Command(Qualified<CommandName>),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct RelationshipModelMapping {
    pub source_field: FieldAccess,
    pub target_field: FieldAccess,
    // Optional because we allow building schema without specifying a data source
    pub target_ndc_column: Option<NdcColumnForComparison>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct RelationshipCommandMapping {
    pub source_field: FieldAccess,
    pub argument_name: ArgumentName,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct Relationship {
    pub name: RelationshipName,
    // `ast::Name` representation of `RelationshipName`. This is used to avoid
    // the recurring conversion between `RelationshipName` to `ast::Name` during
    // relationship IR generation
    pub field_name: ast::Name,
    pub source: Qualified<CustomTypeName>,
    pub target: RelationshipTarget,
    pub target_capabilities: Option<RelationshipCapabilities>,
    pub description: Option<String>,
    pub deprecated: Option<Deprecated>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct RelationshipCapabilities {
    // TODO: We don't handle relationships without foreach.
    // Change this to a bool, when we support that
    pub foreach: (),
    pub relationships: bool,
}

pub enum RelationshipExecutionCategory {
    // Push down relationship definition to the data connector
    Local,
    // Use foreach in the data connector to fetch related rows for multiple objects in a single request
    RemoteForEach,
}

#[allow(clippy::match_single_binding)]
pub fn relationship_execution_category(
    source_connector: &DataConnectorLink,
    target_connector: &DataConnectorLink,
    target_source_relationship_capabilities: &RelationshipCapabilities,
) -> RelationshipExecutionCategory {
    // It's a local relationship if the source and target connectors are the same and
    // the connector supports relationships.
    if target_connector.name == source_connector.name
        && target_source_relationship_capabilities.relationships
    {
        RelationshipExecutionCategory::Local
    } else {
        match target_source_relationship_capabilities.foreach {
            // TODO: When we support naive relationships for connectors not implementing foreach,
            // add another match arm / return enum variant
            () => RelationshipExecutionCategory::RemoteForEach,
        }
    }
}

fn resolve_relationship_source_mapping<'a>(
    relationship_name: &'a RelationshipName,
    source_type_name: &'a Qualified<CustomTypeName>,
    source_type: &type_permissions::ObjectTypeWithPermissions,
    relationship_mapping: &'a open_dds::relationships::RelationshipMapping,
) -> Result<&'a FieldAccess, Error> {
    match &relationship_mapping.source {
        relationships::RelationshipMappingSource::Value(_v) => Err(Error::NotSupported {
            reason: "Relationship mappings from value expressions are not supported yet."
                .to_string(),
        }),
        relationships::RelationshipMappingSource::FieldPath(field_path) => match &field_path[..] {
            [] => Err(Error::EmptyFieldPath {
                location: "source".to_string(),
                type_name: source_type_name.clone(),
                relationship_name: relationship_name.clone(),
            }),
            [field_access] => {
                if !source_type
                    .object_type
                    .fields
                    .contains_key(&field_access.field_name)
                {
                    return Err(Error::RelationshipError {
                        relationship_error:
                            RelationshipError::UnknownSourceFieldInRelationshipMapping {
                                relationship_name: relationship_name.clone(),
                                source_type: source_type_name.clone(),
                                field_name: field_access.field_name.clone(),
                            },
                    });
                };
                Ok(field_access)
            }
            _ => Err(Error::NotSupported {
                reason: "Relationships with nested field paths are not supported yet.".to_string(),
            }),
        },
    }
}

fn resolve_relationship_mappings_model(
    relationship: &RelationshipV1,
    source_type_name: &Qualified<CustomTypeName>,
    source_type: &type_permissions::ObjectTypeWithPermissions,
    target_model: &Model,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
) -> Result<Vec<RelationshipModelMapping>, Error> {
    let mut resolved_relationship_mappings = Vec::new();
    let mut field_mapping_hashset_for_validation: HashSet<&String> = HashSet::new();
    for relationship_mapping in &relationship.mapping {
        let resolved_relationship_source_mapping = resolve_relationship_source_mapping(
            &relationship.name,
            source_type_name,
            source_type,
            relationship_mapping,
        )?;

        let resolved_relationship_target_mapping = match &relationship_mapping.target {
            relationships::RelationshipMappingTarget::Argument(_argument_mapping_target) => {
                return Err(Error::NotSupported {
                    reason: "Relationship mappings to model arguments expressions are not supported yet."
                        .to_string(),
                })
            }
            relationships::RelationshipMappingTarget::ModelField(field_path) => {
                match &field_path[..] {
                    [] => {
                        return Err(Error::EmptyFieldPath {
                            location: "target".to_string(),
                            type_name: source_type_name.clone(),
                            relationship_name: relationship.name.clone(),
                        })
                    }
                    [t] => t,
                    _ => {
                        return Err(Error::NotSupported {
                            reason: "Relationships with nested field paths are not supported yet."
                                .to_string(),
                        })
                    }
                }
            }
        };

        // Check if the target field exists in the target model.
        if !target_model
            .type_fields
            .contains_key(&resolved_relationship_target_mapping.field_name)
        {
            return Err(Error::RelationshipError {
                relationship_error: RelationshipError::UnknownTargetFieldInRelationshipMapping {
                    relationship_name: relationship.name.clone(),
                    source_type: source_type_name.clone(),
                    model_name: target_model.name.clone(),
                    field_name: resolved_relationship_source_mapping.field_name.clone(),
                },
            });
        }

        // Check if the source field is already mapped to a target field
        let resolved_relationship_mapping = {
            if field_mapping_hashset_for_validation
                .insert(&resolved_relationship_source_mapping.field_name.0)
            {
                let target_ndc_column = target_model
                    .source
                    .as_ref()
                    .map(|target_model_source| {
                        get_ndc_column_for_comparison(
                            &target_model.name,
                            &target_model.data_type,
                            target_model_source,
                            &resolved_relationship_target_mapping.field_name,
                            data_connectors,
                            || {
                                format!(
                                    "the mapping for relationship {} on type {}",
                                    relationship.name, source_type_name
                                )
                            },
                        )
                    })
                    .transpose()?;
                Ok(RelationshipModelMapping {
                    source_field: resolved_relationship_source_mapping.clone(),
                    target_field: resolved_relationship_target_mapping.clone(),
                    target_ndc_column,
                })
            } else {
                Err(Error::RelationshipError {
                    relationship_error: RelationshipError::MappingExistsInRelationship {
                        type_name: source_type_name.clone(),
                        field_name: resolved_relationship_source_mapping.field_name.clone(),
                        relationship_name: relationship.name.clone(),
                    },
                })
            }
        }?;
        resolved_relationship_mappings.push(resolved_relationship_mapping);
    }

    Ok(resolved_relationship_mappings)
}

fn resolve_relationship_mappings_command(
    relationship: &RelationshipV1,
    source_type_name: &Qualified<CustomTypeName>,
    source_type: &type_permissions::ObjectTypeWithPermissions,
    target_command: &Command,
) -> Result<Vec<RelationshipCommandMapping>, Error> {
    let mut resolved_relationship_mappings = Vec::new();
    let mut field_mapping_hashset_for_validation: HashSet<&String> = HashSet::new();
    let mut target_command_arguments_hashset_for_validation: HashSet<&String> = HashSet::new();

    for relationship_mapping in &relationship.mapping {
        let resolved_relationship_source_mapping = resolve_relationship_source_mapping(
            &relationship.name,
            source_type_name,
            source_type,
            relationship_mapping,
        )?;
        let target_argument_name = match &relationship_mapping.target {
            relationships::RelationshipMappingTarget::Argument(argument_mapping_target) => {
                &argument_mapping_target.argument_name
            }
            relationships::RelationshipMappingTarget::ModelField(_field_path) => {
                return Err(Error::ModelFieldCannotBeUsedInCommandRelationship {
                    relationship_name: relationship.name.clone(),
                    type_name: source_type_name.clone(),
                })
            }
        };

        // Check if the target argument exists in the target command.
        if !target_command.arguments.contains_key(target_argument_name) {
            return Err(Error::RelationshipError {
                relationship_error: RelationshipError::UnknownTargetArgumentInRelationshipMapping {
                    relationship_name: relationship.name.clone(),
                    source_type: source_type_name.clone(),
                    command_name: target_command.name.clone(),
                    argument_name: target_argument_name.clone(),
                },
            });
        }

        // Check if the target argument is already mapped to a field in the source type.
        if !target_command_arguments_hashset_for_validation.insert(&target_argument_name.0) {
            return Err(Error::RelationshipError {
                relationship_error: RelationshipError::ArgumentMappingExistsInRelationship {
                    argument_name: target_argument_name.clone(),
                    command_name: target_command.name.clone(),
                    relationship_name: relationship.name.clone(),
                    type_name: source_type_name.clone(),
                },
            });
        };

        // Check if the source field is already mapped to a target argument
        let resolved_relationship_mapping = {
            if field_mapping_hashset_for_validation
                .insert(&resolved_relationship_source_mapping.field_name.0)
            {
                Ok(RelationshipCommandMapping {
                    source_field: resolved_relationship_source_mapping.clone(),
                    argument_name: target_argument_name.clone(),
                })
            } else {
                Err(Error::RelationshipError {
                    relationship_error: RelationshipError::MappingExistsInRelationship {
                        type_name: source_type_name.clone(),
                        field_name: resolved_relationship_source_mapping.field_name.clone(),
                        relationship_name: relationship.name.clone(),
                    },
                })
            }
        }?;
        resolved_relationship_mappings.push(resolved_relationship_mapping);
    }

    Ok(resolved_relationship_mappings)
}

fn get_relationship_capabilities(
    type_name: &Qualified<CustomTypeName>,
    relationship_name: &RelationshipName,
    source_data_connector: &Option<DataConnectorLink>,
    target_name: &RelationshipTargetName,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
) -> Result<Option<RelationshipCapabilities>, Error> {
    let data_connector = if let Some(data_connector) = &source_data_connector {
        data_connector
    } else {
        return Ok(None);
    };

    let resolved_data_connector = data_connectors
        .data_connectors_with_scalars
        .get(&data_connector.name)
        .ok_or_else(|| match target_name {
            RelationshipTargetName::Model(model_name) => Error::UnknownModelDataConnector {
                model_name: model_name.clone(),
                data_connector: data_connector.name.clone(),
            },
            RelationshipTargetName::Command(command_name) => Error::UnknownCommandDataConnector {
                command_name: command_name.clone(),
                data_connector: data_connector.name.clone(),
            },
        })?;
    let capabilities = &resolved_data_connector.inner.capabilities.capabilities;

    if capabilities.query.variables.is_none() {
        return Err(Error::RelationshipError {
            relationship_error: RelationshipError::RelationshipTargetDoesNotSupportForEach {
                type_name: type_name.clone(),
                relationship_name: relationship_name.clone(),
                data_connector_name: data_connector.name.clone(),
            },
        });
    };

    let relationships = capabilities.relationships.is_some();

    Ok(Some(RelationshipCapabilities {
        foreach: (),
        relationships,
    }))
}

pub fn resolve_relationship(
    relationship: &RelationshipV1,
    subgraph: &str,
    models: &IndexMap<Qualified<ModelName>, Model>,
    commands: &IndexMap<Qualified<CommandName>, Command>,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    source_type: &type_permissions::ObjectTypeWithPermissions,
) -> Result<Relationship, Error> {
    let source_type_name = Qualified::new(subgraph.to_string(), relationship.source.clone());
    let (relationship_target, source_data_connector, target_name) = match &relationship.target {
        relationships::RelationshipTarget::Model(target_model) => {
            let qualified_target_model_name = Qualified::new(
                target_model
                    .subgraph()
                    .to_owned()
                    .unwrap_or(subgraph)
                    .to_string(),
                target_model.name.to_owned(),
            );
            let resolved_target_model =
                models.get(&qualified_target_model_name).ok_or_else(|| {
                    Error::UnknownTargetModelUsedInRelationship {
                        type_name: source_type_name.clone(),
                        relationship_name: relationship.name.clone(),
                        model_name: qualified_target_model_name.clone(),
                    }
                })?;
            let source_data_connector = resolved_target_model
                .source
                .as_ref()
                .map(|source| source.data_connector.clone());
            (
                RelationshipTarget::Model {
                    model_name: qualified_target_model_name,
                    relationship_type: target_model.relationship_type.clone(),
                    target_typename: resolved_target_model.data_type.clone(),
                    mappings: resolve_relationship_mappings_model(
                        relationship,
                        &source_type_name,
                        source_type,
                        resolved_target_model,
                        data_connectors,
                    )?,
                },
                source_data_connector,
                RelationshipTargetName::Model(resolved_target_model.name.clone()),
            )
        }
        relationships::RelationshipTarget::Command(target_command) => {
            let qualified_target_command_name = Qualified::new(
                target_command
                    .subgraph
                    .as_deref()
                    .unwrap_or(subgraph)
                    .to_string(),
                target_command.name.to_owned(),
            );
            let resolved_target_command =
                commands
                    .get(&qualified_target_command_name)
                    .ok_or_else(|| Error::UnknownTargetCommandUsedInRelationship {
                        type_name: source_type_name.clone(),
                        relationship_name: relationship.name.clone(),
                        command_name: qualified_target_command_name.clone(),
                    })?;

            let source_data_connector = resolved_target_command
                .source
                .as_ref()
                .map(|source| source.data_connector.clone());
            (
                RelationshipTarget::Command {
                    command_name: qualified_target_command_name,
                    target_type: resolved_target_command.output_type.clone(),
                    mappings: resolve_relationship_mappings_command(
                        relationship,
                        &source_type_name,
                        source_type,
                        resolved_target_command,
                    )?,
                },
                source_data_connector,
                RelationshipTargetName::Command(resolved_target_command.name.clone()),
            )
        }
    };

    let target_capabilities = get_relationship_capabilities(
        &source_type_name,
        &relationship.name,
        &source_data_connector,
        &target_name,
        data_connectors,
    )?;

    let field_name = mk_name(&relationship.name.0)?;
    Ok(Relationship {
        name: relationship.name.clone(),
        field_name,
        source: source_type_name,
        target: relationship_target,
        target_capabilities,
        description: relationship.description.clone(),
        deprecated: relationship.deprecated.clone(),
    })
}
