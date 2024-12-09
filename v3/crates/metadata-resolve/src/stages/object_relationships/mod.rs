use std::sync::Arc;
mod types;

use std::collections::{BTreeMap, BTreeSet};

use indexmap::IndexMap;

use lang_graphql::ast::common::{self as ast};
use open_dds::aggregates::AggregateExpressionName;
use open_dds::relationships::{FieldAccess, RelationshipName, RelationshipType, RelationshipV1};
use open_dds::{
    commands::CommandName, data_connector::DataConnectorName, models::ModelName,
    types::CustomTypeName,
};

use crate::helpers::types::mk_name;
use crate::stages::{
    aggregates, commands, data_connector_scalar_types, data_connectors, graphql_config, models,
    object_types, relationships, type_permissions,
};
use crate::types::error::{Error, RelationshipError};
use crate::types::subgraph::Qualified;

pub use types::{
    CommandRelationshipTarget, FieldNestedness, ModelAggregateRelationshipTarget,
    ModelRelationshipTarget, ObjectTypeWithRelationships, RelationshipCapabilities,
    RelationshipCommandMapping, RelationshipExecutionCategory, RelationshipField,
    RelationshipModelMapping, RelationshipTarget, RelationshipTargetName,
};

/// resolve relationships
/// returns updated `types` value
pub fn resolve(
    object_types_with_permissions: type_permissions::ObjectTypesWithPermissions,
    relationships: &relationships::Relationships,
    data_connectors: &data_connectors::DataConnectors,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars,
    >,
    models: &IndexMap<Qualified<ModelName>, models::Model>,
    commands: &IndexMap<Qualified<CommandName>, commands::Command>,
    aggregate_expressions: &BTreeMap<
        Qualified<AggregateExpressionName>,
        aggregates::AggregateExpression,
    >,
    graphql_config: &graphql_config::GraphqlConfig,
) -> Result<BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>, Error> {
    // For each object type, get all its relationships and resolve them into the fields
    // we add to the object type that represent the relationship navigation
    let mut object_type_relationship_fields = object_types_with_permissions
        .iter()
        .map(|(object_type_name, object_type_with_permissions)| {
            let object_type_relationships =
                relationships.get_relationships_for_type(object_type_name);

            let mut relationship_fields = IndexMap::new();
            for (relationship_name, relationship) in object_type_relationships {
                let resolved_relationships_fields = match relationship {
                    relationships::Relationship::Relationship(relationship) => {
                        resolve_relationship_fields(
                            relationship,
                            object_type_name,
                            models,
                            commands,
                            data_connectors,
                            data_connector_scalars,
                            aggregate_expressions,
                            &object_types_with_permissions,
                            graphql_config,
                            &object_type_with_permissions.object_type,
                        )?
                    }
                    // If the relationship is to an unknown subgraph, we ignore it since we're
                    // running in allow unknown subgraphs mode
                    relationships::Relationship::RelationshipToUnknownSubgraph => vec![],
                };

                for resolved_relationship_field in resolved_relationships_fields {
                    let field_name = resolved_relationship_field.field_name.clone();
                    if relationship_fields
                        .insert(
                            resolved_relationship_field.field_name.clone(),
                            resolved_relationship_field,
                        )
                        .is_some()
                    {
                        return Err(Error::DuplicateRelationshipFieldInSourceType {
                            field_name,
                            type_name: object_type_name.clone(),
                            relationship_name: relationship_name.clone(),
                        });
                    }
                }
            }

            Ok((object_type_name.clone(), relationship_fields))
        })
        .collect::<Result<BTreeMap<_, _>, _>>()?;

    // Create the ObjectTypeWithRelationships by consuming the ObjectTypeWithPermissions and adding
    // the newly resolved relationship fields
    let object_types_with_relationships = object_types_with_permissions
        .0
        .into_iter()
        .map(|(object_type_name, object_type_with_permissions)| {
            // Take ownership of the relationship fields out of the map and leave behind an empty value in its place
            let relationship_fields = object_type_relationship_fields
                .get_mut(&object_type_name)
                .map(std::mem::take)
                .unwrap_or_default();

            let object_type_with_relationships = ObjectTypeWithRelationships {
                object_type: object_type_with_permissions.object_type,
                type_output_permissions: object_type_with_permissions.type_output_permissions,
                type_input_permissions: object_type_with_permissions.type_input_permissions,
                relationship_fields,
                type_mappings: object_type_with_permissions.type_mappings,
            };

            (object_type_name, object_type_with_relationships)
        })
        .collect::<BTreeMap<_, _>>();

    Ok(object_types_with_relationships)
}

#[allow(clippy::match_single_binding)]
pub fn relationship_execution_category(
    relationship_field_nestedness: FieldNestedness,
    source_connector: &data_connectors::DataConnectorLink,
    target_connector: &data_connectors::DataConnectorLink,
    target_source_relationship_capabilities: &RelationshipCapabilities,
) -> RelationshipExecutionCategory {
    // It's a local relationship if the source and target connectors are the same and
    // the connector supports relationships.
    match &target_source_relationship_capabilities.supports_relationships {
        Some(supports_relationships) if target_connector.name == source_connector.name => {
            match relationship_field_nestedness {
                // If the relationship is not nested, we support it ...
                FieldNestedness::NotNested => RelationshipExecutionCategory::Local,
                // ... but we only support relationships nested inside objects if the connector declares it does ...
                FieldNestedness::ObjectNested
                    if supports_relationships
                        .supports_nested_relationships
                        .is_some() =>
                {
                    RelationshipExecutionCategory::Local
                }
                // ... and we only support relationships nested inside arrays if the connector declares it does ...
                FieldNestedness::ArrayNested
                    if supports_relationships
                        .supports_nested_relationships
                        .as_ref()
                        .is_some_and(|n| n.supports_nested_array_selection) =>
                {
                    RelationshipExecutionCategory::Local
                }
                // ... otherwise we fall back to remote joins
                _ => RelationshipExecutionCategory::RemoteForEach,
            }
        }
        _ => {
            match target_source_relationship_capabilities.foreach {
                // TODO: When we support naive relationships for connectors not implementing foreach,
                // add another match arm / return enum variant
                () => RelationshipExecutionCategory::RemoteForEach,
            }
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
        open_dds::relationships::RelationshipMappingSource::Value(_v) => Err(Error::NotSupported {
            reason: "Relationship mappings from value expressions are not supported yet."
                .to_string(),
        }),
        open_dds::relationships::RelationshipMappingSource::FieldPath(field_path) => {
            match &field_path[..] {
                [] => Err(Error::EmptyFieldPath {
                    location: "source".to_string(),
                    type_name: source_type_name.clone(),
                    relationship_name: relationship_name.clone(),
                }),
                [field_access] => {
                    if !source_type.fields.contains_key(&field_access.field_name) {
                        return Err(Error::ObjectRelationshipError {
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
                    reason: "Relationships with nested field paths are not supported yet."
                        .to_string(),
                }),
            }
        }
    }
}

fn resolve_relationship_mappings_model(
    relationship: &RelationshipV1,
    source_type_name: &Qualified<CustomTypeName>,
    source_type: &object_types::ObjectTypeRepresentation,
    target_model: &models::Model,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars,
    >,
) -> Result<Vec<RelationshipModelMapping>, Error> {
    let mut resolved_relationship_mappings = Vec::new();
    let mut field_mapping_btree_set_for_validation = BTreeSet::new();
    for relationship_mapping in &relationship.mapping {
        let resolved_relationship_source_mapping = resolve_relationship_source_mapping(
            &relationship.name,
            source_type_name,
            source_type,
            relationship_mapping,
        )?;

        let resolved_relationship_target_mapping = match &relationship_mapping.target {
            open_dds::relationships::RelationshipMappingTarget::Argument(
                _argument_mapping_target,
            ) => return Err(Error::NotSupported {
                reason:
                    "Relationship mappings to model arguments expressions are not supported yet."
                        .to_string(),
            }),
            open_dds::relationships::RelationshipMappingTarget::ModelField(field_path) => {
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
            return Err(Error::ObjectRelationshipError {
                relationship_error: RelationshipError::UnknownTargetFieldInRelationshipMapping {
                    relationship_name: relationship.name.clone(),
                    source_type: source_type_name.clone(),
                    model_name: target_model.name.clone(),
                    field_name: resolved_relationship_target_mapping.field_name.clone(),
                },
            });
        }

        // Check if the source field is already mapped to a target field
        let resolved_relationship_mapping = {
            if field_mapping_btree_set_for_validation
                .insert(&resolved_relationship_source_mapping.field_name)
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
                            data_connector_scalars,
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
                Err(Error::ObjectRelationshipError {
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
    let mut field_mapping_btree_set_for_validation = BTreeSet::new();
    let mut target_command_arguments_btree_set_for_validation = BTreeSet::new();

    for relationship_mapping in &relationship.mapping {
        let resolved_relationship_source_mapping = resolve_relationship_source_mapping(
            &relationship.name,
            source_type_name,
            source_type,
            relationship_mapping,
        )?;
        let target_argument_name = match &relationship_mapping.target {
            open_dds::relationships::RelationshipMappingTarget::Argument(
                argument_mapping_target,
            ) => &argument_mapping_target.argument_name,
            open_dds::relationships::RelationshipMappingTarget::ModelField(_field_path) => {
                return Err(Error::ModelFieldCannotBeUsedInCommandRelationship {
                    relationship_name: relationship.name.clone(),
                    type_name: source_type_name.clone(),
                })
            }
        };

        // Check if the target argument exists in the target command.
        if !target_command.arguments.contains_key(target_argument_name) {
            return Err(Error::ObjectRelationshipError {
                relationship_error: RelationshipError::UnknownTargetArgumentInRelationshipMapping {
                    relationship_name: relationship.name.clone(),
                    source_type: source_type_name.clone(),
                    command_name: target_command.name.clone(),
                    argument_name: target_argument_name.clone(),
                },
            });
        }

        // Check if the target argument is already mapped to a field in the source type.
        if !target_command_arguments_btree_set_for_validation.insert(target_argument_name) {
            return Err(Error::ObjectRelationshipError {
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
                .insert(&resolved_relationship_source_mapping.field_name)
            {
                Ok(RelationshipCommandMapping {
                    source_field: resolved_relationship_source_mapping.clone(),
                    argument_name: target_argument_name.clone(),
                })
            } else {
                Err(Error::ObjectRelationshipError {
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
    source_data_connector: Option<&Arc<data_connectors::DataConnectorLink>>,
    target_name: &RelationshipTargetName,
    data_connectors: &data_connectors::DataConnectors,
    models: &IndexMap<Qualified<ModelName>, models::Model>,
    commands: &IndexMap<Qualified<CommandName>, commands::Command>,
) -> Result<Option<RelationshipCapabilities>, Error> {
    let Some(data_connector) = source_data_connector else {
        return Ok(None);
    };

    let resolved_data_connector =
        data_connectors
            .0
            .get(&data_connector.name)
            .ok_or_else(|| match target_name {
                RelationshipTargetName::Model(model_name) => {
                    Error::from(models::ModelsError::UnknownModelDataConnector {
                        model_name: model_name.clone(),
                        data_connector: data_connector.name.clone(),
                    })
                }
                RelationshipTargetName::Command(command_name) => {
                    Error::from(commands::CommandsError::UnknownCommandDataConnector {
                        command_name: command_name.clone(),
                        data_connector: data_connector.name.clone(),
                    })
                }
            })?;

    // which data connector is the target using?
    let target_data_connector = match target_name {
        RelationshipTargetName::Model(model_name) => models
            .get(model_name)
            .as_ref()
            .and_then(|model| model.source.as_ref())
            .map(|source| source.data_connector.name.clone()),
        RelationshipTargetName::Command(command_name) => commands
            .get(command_name)
            .and_then(|command| command.source.as_ref())
            .map(|source| source.data_connector.name.clone()),
    };

    let capabilities = &resolved_data_connector.capabilities;

    // if relationship is remote, error if `foreach` capability is not available
    if capabilities.supports_query_variables
        && Some(&data_connector.name) != target_data_connector.as_ref()
    {
        return Err(Error::ObjectRelationshipError {
            relationship_error: RelationshipError::RelationshipTargetDoesNotSupportForEach {
                type_name: type_name.clone(),
                relationship_name: relationship_name.clone(),
                data_connector_name: data_connector.name.clone(),
            },
        });
    };

    Ok(Some(RelationshipCapabilities {
        foreach: (),
        supports_relationships: capabilities.supports_relationships.clone(),
    }))
}

fn resolve_aggregate_relationship_field(
    model_relationship_target: &open_dds::relationships::ModelRelationshipTarget,
    resolved_target_model: &models::Model,
    resolved_relationship_mappings: &[RelationshipModelMapping],
    resolved_target_capabilities: Option<&RelationshipCapabilities>,
    relationship: &RelationshipV1,
    source_type_name: &Qualified<CustomTypeName>,
    aggregate_expressions: &BTreeMap<
        Qualified<AggregateExpressionName>,
        aggregates::AggregateExpression,
    >,
    object_types: &type_permissions::ObjectTypesWithPermissions,
    graphql_config: &graphql_config::GraphqlConfig,
) -> Result<Option<RelationshipField>, Error> {
    // If an aggregate has been specified
    let aggregate_expression_name_and_description = model_relationship_target
        .aggregate
        .as_ref()
        .map(|aggregate| -> Result<_, Error> {
            // Ensure the relationship is an array relationship
            if model_relationship_target.relationship_type != RelationshipType::Array {
                return Err(
                    RelationshipError::AggregateIsOnlyAllowedOnArrayRelationships {
                        type_name: source_type_name.clone(),
                        relationship_name: relationship.name.clone(),
                    }
                    .into(),
                );
            }

            // Validate its usage with the relationship's target model
            let aggregate_expression_name = models::resolve_aggregate_expression(
                &Qualified::new(
                    resolved_target_model.name.subgraph.clone(),
                    aggregate.aggregate_expression.clone(),
                ),
                &resolved_target_model.name,
                &resolved_target_model.data_type,
                resolved_target_model.source.as_ref(),
                aggregate_expressions,
                object_types,
            )
            .map_err(|error| RelationshipError::ModelAggregateExpressionError {
                type_name: source_type_name.clone(),
                relationship_name: relationship.name.clone(),
                error,
            })?;

            Ok((aggregate_expression_name, &aggregate.description))
        })
        .transpose()?;

    let field_name = relationship
        .graphql
        .as_ref()
        .and_then(|g| g.aggregate_field_name.as_ref())
        .map(|f| mk_name(f.as_str()))
        .transpose()?;

    // We only get an aggregate if both the expression and a field name has been specified.
    // Without the field name, the aggregate is inaccessible
    aggregate_expression_name_and_description
        .zip(field_name)
        .map(|((aggregate_expression, description), field_name)| {
            // Check that the filter input field name is configured in graphql config
            let filter_input_field_name = graphql_config
                .query
                .aggregate_config
                .as_ref()
                .map(|agg| agg.filter_input_field_name.clone())
                .ok_or_else::<Error, _>(|| Error::GraphqlConfigError {
                    graphql_config_error:
                        graphql_config::GraphqlConfigError::MissingAggregateFilterInputFieldNameInGraphqlConfig,
                })?;

            Ok(RelationshipField {
                field_name,
                relationship_name: relationship.name.clone(),
                source: source_type_name.clone(),
                target: RelationshipTarget::ModelAggregate(ModelAggregateRelationshipTarget {
                    model_name: resolved_target_model.name.clone(),
                    target_typename: resolved_target_model.data_type.clone(),
                    mappings: resolved_relationship_mappings.to_vec(),
                    aggregate_expression,
                    filter_input_field_name,
                }),
                target_capabilities: resolved_target_capabilities.cloned(),
                description: description.clone(),
                deprecated: relationship.deprecated.clone(),
            })
        })
        .transpose()
}

fn resolve_model_relationship_fields(
    target_model: &open_dds::relationships::ModelRelationshipTarget,
    models: &IndexMap<Qualified<ModelName>, crate::Model>,
    data_connectors: &data_connectors::DataConnectors,
    source_type_name: &Qualified<CustomTypeName>,
    relationship: &RelationshipV1,
    source_type: &object_types::ObjectTypeRepresentation,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars,
    >,
    aggregate_expressions: &BTreeMap<
        Qualified<AggregateExpressionName>,
        aggregates::AggregateExpression,
    >,
    object_types: &type_permissions::ObjectTypesWithPermissions,
    graphql_config: &graphql_config::GraphqlConfig,
) -> Result<Vec<RelationshipField>, Error> {
    let qualified_target_model_name = Qualified::new(
        target_model
            .subgraph()
            .unwrap_or(source_type_name.subgraph.clone()),
        target_model.name.clone(),
    );
    let resolved_target_model = models.get(&qualified_target_model_name).ok_or_else(|| {
        Error::UnknownTargetModelUsedInRelationship {
            type_name: source_type_name.clone(),
            relationship_name: relationship.name.clone(),
            model_name: qualified_target_model_name.clone(),
        }
    })?;

    let source_data_connector = resolved_target_model
        .source
        .as_ref()
        .map(|source| &source.data_connector);

    let target_capabilities = get_relationship_capabilities(
        source_type_name,
        &relationship.name,
        source_data_connector,
        &RelationshipTargetName::Model(resolved_target_model.name.clone()),
        data_connectors,
        models,
        &IndexMap::new(),
    )?;

    let mappings = resolve_relationship_mappings_model(
        relationship,
        source_type_name,
        source_type,
        resolved_target_model,
        data_connector_scalars,
    )?;

    let aggregate_relationship_field = resolve_aggregate_relationship_field(
        target_model,
        resolved_target_model,
        &mappings,
        target_capabilities.as_ref(),
        relationship,
        source_type_name,
        aggregate_expressions,
        object_types,
        graphql_config,
    )?;

    let regular_relationship_field = RelationshipField {
        field_name: make_relationship_field_name(&relationship.name)?,
        relationship_name: relationship.name.clone(),
        source: source_type_name.clone(),
        target: RelationshipTarget::Model(ModelRelationshipTarget {
            model_name: qualified_target_model_name,
            relationship_type: target_model.relationship_type.clone(),
            target_typename: resolved_target_model.data_type.clone(),
            mappings,
        }),
        target_capabilities,
        description: relationship.description.clone(),
        deprecated: relationship.deprecated.clone(),
    };

    Ok(std::iter::once(regular_relationship_field)
        .chain(aggregate_relationship_field)
        .collect())
}

// create the graphql name for a non-aggregate relationship field name
pub fn make_relationship_field_name(
    relationship_name: &RelationshipName,
) -> Result<ast::Name, Error> {
    mk_name(relationship_name.as_str()).map_err(Error::from)
}

fn resolve_command_relationship_field(
    target_command: &open_dds::relationships::CommandRelationshipTarget,
    commands: &IndexMap<Qualified<CommandName>, commands::Command>,
    data_connectors: &data_connectors::DataConnectors,
    source_type_name: &Qualified<CustomTypeName>,
    relationship: &RelationshipV1,
    source_type: &object_types::ObjectTypeRepresentation,
) -> Result<RelationshipField, Error> {
    let qualified_target_command_name = Qualified::new(
        target_command
            .subgraph()
            .unwrap_or(source_type_name.subgraph.clone()),
        target_command.name.clone(),
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
        .map(|source| &source.data_connector);

    let target = RelationshipTarget::Command(CommandRelationshipTarget {
        command_name: qualified_target_command_name,
        target_type: resolved_target_command.output_type.clone(),
        mappings: resolve_relationship_mappings_command(
            relationship,
            source_type_name,
            source_type,
            resolved_target_command,
        )?,
    });

    let target_capabilities = get_relationship_capabilities(
        source_type_name,
        &relationship.name,
        source_data_connector,
        &RelationshipTargetName::Command(resolved_target_command.name.clone()),
        data_connectors,
        &IndexMap::new(),
        commands,
    )?;

    let field_name = mk_name(relationship.name.as_str())?;
    Ok(RelationshipField {
        field_name,
        relationship_name: relationship.name.clone(),
        source: source_type_name.clone(),
        target,
        target_capabilities,
        description: relationship.description.clone(),
        deprecated: relationship.deprecated.clone(),
    })
}

fn resolve_relationship_fields(
    relationship: &RelationshipV1,
    source_type_name: &Qualified<CustomTypeName>,
    models: &IndexMap<Qualified<ModelName>, models::Model>,
    commands: &IndexMap<Qualified<CommandName>, commands::Command>,
    data_connectors: &data_connectors::DataConnectors,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars,
    >,
    aggregate_expressions: &BTreeMap<
        Qualified<AggregateExpressionName>,
        aggregates::AggregateExpression,
    >,
    object_types: &type_permissions::ObjectTypesWithPermissions,
    graphql_config: &graphql_config::GraphqlConfig,
    source_type: &object_types::ObjectTypeRepresentation,
) -> Result<Vec<RelationshipField>, Error> {
    match &relationship.target {
        open_dds::relationships::RelationshipTarget::Model(target_model) => {
            resolve_model_relationship_fields(
                target_model,
                models,
                data_connectors,
                source_type_name,
                relationship,
                source_type,
                data_connector_scalars,
                aggregate_expressions,
                object_types,
                graphql_config,
            )
        }
        open_dds::relationships::RelationshipTarget::Command(target_command) => {
            let command_relationship_field = resolve_command_relationship_field(
                target_command,
                commands,
                data_connectors,
                source_type_name,
                relationship,
                source_type,
            )?;
            Ok(vec![command_relationship_field])
        }
    }
}
