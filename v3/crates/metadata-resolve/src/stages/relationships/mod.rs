mod types;
pub use types::{
    ObjectTypeWithRelationships, Relationship, RelationshipCapabilities,
    RelationshipCommandMapping, RelationshipExecutionCategory, RelationshipModelMapping,
    RelationshipTarget, RelationshipTargetName,
};

use std::collections::BTreeMap;

use indexmap::IndexMap;

use open_dds::{commands::CommandName, models::ModelName, types::CustomTypeName};

use crate::types::error::{Error, RelationshipError};
use crate::types::subgraph::Qualified;

use crate::helpers::types::mk_name;
use crate::stages::{
    commands, data_connector_scalar_types, data_connectors, models, object_types, type_permissions,
};

use open_dds::relationships::{self, FieldAccess, RelationshipName, RelationshipV1};

use std::collections::BTreeSet;

/// resolve relationships
/// returns updated `types` value
pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    object_types_with_permissions: &BTreeMap<
        Qualified<CustomTypeName>,
        type_permissions::ObjectTypeWithPermissions,
    >,
    models: &IndexMap<Qualified<ModelName>, models::Model>,
    commands: &IndexMap<Qualified<CommandName>, commands::Command>,
) -> Result<BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>, Error> {
    let mut object_types_with_relationships = BTreeMap::new();
    for (
        object_type_name,
        type_permissions::ObjectTypeWithPermissions {
            type_output_permissions,
            type_input_permissions,
            object_type,
            type_mappings,
        },
    ) in object_types_with_permissions
    {
        object_types_with_relationships.insert(
            object_type_name.clone(),
            ObjectTypeWithRelationships {
                object_type: object_type.clone(),
                type_output_permissions: type_output_permissions.clone(),
                type_input_permissions: type_input_permissions.clone(),
                relationships: IndexMap::new(),
                type_mappings: type_mappings.clone(),
            },
        );
    }
    for open_dds::accessor::QualifiedObject {
        subgraph,
        object: relationship,
    } in &metadata_accessor.relationships
    {
        let qualified_relationship_source_type_name =
            Qualified::new(subgraph.to_string(), relationship.source_type.to_owned());
        let object_representation = object_types_with_relationships
            .get_mut(&qualified_relationship_source_type_name)
            .ok_or_else(|| Error::RelationshipDefinedOnUnknownType {
                relationship_name: relationship.name.clone(),
                type_name: qualified_relationship_source_type_name.clone(),
            })?;

        let resolved_relationship = resolve_relationship(
            relationship,
            subgraph,
            models,
            commands,
            data_connectors,
            &object_representation.object_type,
        )?;

        if object_representation
            .relationships
            .insert(
                resolved_relationship.field_name.clone(),
                resolved_relationship,
            )
            .is_some()
        {
            return Err(Error::DuplicateRelationshipInSourceType {
                type_name: qualified_relationship_source_type_name,
                relationship_name: relationship.name.clone(),
            });
        }
    }

    Ok(object_types_with_relationships)
}

#[allow(clippy::match_single_binding)]
pub fn relationship_execution_category(
    source_connector: &data_connectors::DataConnectorLink,
    target_connector: &data_connectors::DataConnectorLink,
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
    source_type: &object_types::ObjectTypeRepresentation,
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
                if !source_type.fields.contains_key(&field_access.field_name) {
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
    source_type: &object_types::ObjectTypeRepresentation,
    target_model: &models::Model,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
) -> Result<Vec<RelationshipModelMapping>, Error> {
    let mut resolved_relationship_mappings = Vec::new();
    let mut field_mapping_btree_set_for_validation: BTreeSet<&String> = BTreeSet::new();
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
            if field_mapping_btree_set_for_validation
                .insert(&resolved_relationship_source_mapping.field_name.0)
            {
                let target_ndc_column = target_model
                    .source
                    .as_ref()
                    .map(|target_model_source| {
                        models::get_ndc_column_for_comparison(
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
    source_type: &object_types::ObjectTypeRepresentation,
    target_command: &commands::Command,
) -> Result<Vec<RelationshipCommandMapping>, Error> {
    let mut resolved_relationship_mappings = Vec::new();
    let mut field_mapping_btree_set_for_validation: BTreeSet<&String> = BTreeSet::new();
    let mut target_command_arguments_btree_set_for_validation: BTreeSet<&String> = BTreeSet::new();

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
        if !target_command_arguments_btree_set_for_validation.insert(&target_argument_name.0) {
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
            if field_mapping_btree_set_for_validation
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
    source_data_connector: &Option<data_connectors::DataConnectorLink>,
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
    models: &IndexMap<Qualified<ModelName>, models::Model>,
    commands: &IndexMap<Qualified<CommandName>, commands::Command>,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    source_type: &object_types::ObjectTypeRepresentation,
) -> Result<Relationship, Error> {
    let source_type_name = Qualified::new(subgraph.to_string(), relationship.source_type.clone());
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
