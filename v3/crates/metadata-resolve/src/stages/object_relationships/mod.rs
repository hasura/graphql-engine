use std::sync::Arc;
mod error;
mod types;

use std::collections::{BTreeMap, BTreeSet};

use indexmap::IndexMap;

use lang_graphql::ast::common::{self as ast};
use open_dds::aggregates::AggregateExpressionName;
use open_dds::commands::DataConnectorCommand;
use open_dds::query::ArgumentName;
use open_dds::relationships::{
    ArgumentMappingTarget, FieldAccess, RelationshipName, RelationshipType, RelationshipV1,
};
use open_dds::types::FieldName;
use open_dds::{
    commands::CommandName, data_connector::DataConnectorName, models::ModelName,
    types::CustomTypeName,
};

use crate::helpers::types::mk_name;
use crate::stages::{
    aggregates, commands, data_connector_scalar_types, data_connectors, graphql_config, models,
    object_types, relationships, type_permissions,
};
use crate::types::subgraph::{Qualified, QualifiedBaseType, QualifiedTypeReference};
pub use error::RelationshipError;

pub use types::{
    AggregateRelationship, CommandRelationshipTarget, FieldNestedness, ModelRelationshipTarget,
    ObjectRelationshipsIssue, ObjectRelationshipsOutput, ObjectTypeWithRelationships,
    RelationshipCapabilities, RelationshipCommandMapping, RelationshipExecutionCategory,
    RelationshipField, RelationshipFieldAccess, RelationshipModelMapping,
    RelationshipModelMappingFieldTarget, RelationshipModelMappingTarget, RelationshipTarget,
    RelationshipTargetName,
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
) -> Result<ObjectRelationshipsOutput, Vec<RelationshipError>> {
    let mut issues = Vec::new();

    // For each object type, get all its relationships and resolve them into the fields
    // we add to the object type that represent the relationship navigation
    let object_type_relationship_fields = object_types_with_permissions
        .iter()
        .map(|(object_type_name, object_type_with_permissions)| {
            let object_type_relationships =
                relationships.get_relationships_for_type(object_type_name);

            let mut relationship_fields = IndexMap::new();
            for (relationship_name, relationship) in object_type_relationships {
                match relationship {
                    relationships::Relationship::Relationship(relationship) => {
                        let resolved_relationship_field = resolve_relationship_field(
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
                            &mut issues,
                        )?;
                        let field_name = resolved_relationship_field.field_name.clone();
                        if relationship_fields
                            .insert(
                                resolved_relationship_field.relationship_name.clone(),
                                resolved_relationship_field,
                            )
                            .is_some()
                        {
                            return Err(
                                RelationshipError::DuplicateRelationshipFieldInSourceType {
                                    field_name,
                                    type_name: object_type_name.clone(),
                                    relationship_name: relationship_name.clone(),
                                },
                            );
                        }
                    }
                    // If the relationship is to an unknown subgraph, we ignore it since we're
                    // running in allow unknown subgraphs mode
                    relationships::Relationship::RelationshipToUnknownSubgraph => {}
                }
            }

            Ok((object_type_name.clone(), relationship_fields))
        })
        .collect::<Vec<Result<_, _>>>();

    partition_eithers::collect_any_errors(object_type_relationship_fields).map(
        |object_type_relationship_fields| {
            let mut object_type_relationship_fields = object_type_relationship_fields
                .into_iter()
                .collect::<BTreeMap<_, _>>();

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
                        type_output_permissions: object_type_with_permissions
                            .type_output_permissions,
                        type_input_permissions: object_type_with_permissions.type_input_permissions,
                        relationship_fields,
                        type_mappings: object_type_with_permissions.type_mappings,
                    };

                    (object_type_name, object_type_with_relationships)
                })
                .collect::<BTreeMap<_, _>>();

            ObjectRelationshipsOutput {
                object_types: object_types_with_relationships,
                issues,
            }
        },
    )
}

/// Tests whether a relationship in a field selection should be evaluated as a local or a remote relationship.
/// Note that this function is not suitable to be used to evaluate if a relationship is local or remote
/// in filtering or ordering contexts, only for in field selection!
#[allow(clippy::match_single_binding)]
pub fn field_selection_relationship_execution_category(
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

fn resolve_relationship_source_mapping<'a, 'b>(
    relationship_name: &RelationshipName,
    source_type_name: &Qualified<CustomTypeName>,
    source_type: &'b object_types::ObjectTypeRepresentation,
    relationship_mapping: &'a open_dds::relationships::RelationshipMapping,
) -> Result<(&'a FieldAccess, &'b QualifiedTypeReference), RelationshipError> {
    match &relationship_mapping.source {
        open_dds::relationships::RelationshipMappingSource::Value(_v) => {
            Err(RelationshipError::ValueExpressionMappingsNotSupportedYet)
        }
        open_dds::relationships::RelationshipMappingSource::FieldPath(field_path) => {
            match &field_path[..] {
                [] => Err(RelationshipError::EmptyFieldPath {
                    location: "source".to_string(),
                    type_name: source_type_name.clone(),
                    relationship_name: relationship_name.clone(),
                }),
                [field_access] => {
                    let Some(field_definition) =
                        source_type.fields.get(&field_access.field_name.value)
                    else {
                        return Err(RelationshipError::UnknownSourceFieldInRelationshipMapping {
                            relationship_name: relationship_name.clone(),
                            source_type: source_type_name.clone(),
                            field_name: field_access.field_name.value.clone(),
                        });
                    };

                    Ok((field_access, &field_definition.field_type))
                }
                _ => Err(RelationshipError::NestedFieldPathsNotSupportedYet),
            }
        }
    }
}

fn resolve_relationship_mappings_model(
    relationship: &RelationshipV1,
    source_type_name: &Qualified<CustomTypeName>,
    source_type: &object_types::ObjectTypeRepresentation,
    target_model: &models::Model,
) -> Result<Vec<RelationshipModelMapping>, RelationshipError> {
    let mut resolved_relationship_mappings = Vec::new();
    let mut source_fields_already_mapped = BTreeSet::new();
    let mut target_arguments_already_mapped = BTreeSet::new();

    for relationship_mapping in &relationship.mapping {
        let (resolved_relationship_source_field, source_field_type) =
            resolve_relationship_source_mapping(
                &relationship.name,
                source_type_name,
                source_type,
                relationship_mapping,
            )?;

        if !source_fields_already_mapped
            .insert(&resolved_relationship_source_field.field_name.value)
        {
            return Err(RelationshipError::MappingExistsInRelationship {
                type_name: source_type_name.clone(),
                field_name: resolved_relationship_source_field.field_name.value.clone(),
                relationship_name: relationship.name.clone(),
            });
        }

        let resolved_relationship_mapping_target = match &relationship_mapping.target {
            open_dds::relationships::RelationshipMappingTarget::ModelField(field_path) => {
                resolve_relationship_mappings_model_field_target(
                    field_path,
                    target_model,
                    relationship,
                    source_type_name,
                )?
            }
            open_dds::relationships::RelationshipMappingTarget::Argument(
                argument_mapping_target,
            ) => resolve_relationship_mappings_model_argument_target(
                argument_mapping_target,
                target_model,
                relationship,
                &resolved_relationship_source_field.field_name,
                source_field_type,
                source_type_name,
                &mut target_arguments_already_mapped,
            )?,
        };

        let resolved_relationship_mapping = RelationshipModelMapping {
            source_field: RelationshipFieldAccess {
                field_name: resolved_relationship_source_field.field_name.value.clone(),
            },
            target: resolved_relationship_mapping_target,
        };

        resolved_relationship_mappings.push(resolved_relationship_mapping);
    }

    Ok(resolved_relationship_mappings)
}

fn resolve_relationship_mappings_model_field_target(
    resolved_relationship_target_model_field_path: &[FieldAccess],
    target_model: &models::Model,
    relationship: &RelationshipV1,
    source_type_name: &Qualified<CustomTypeName>,
) -> Result<RelationshipModelMappingTarget, RelationshipError> {
    // Check that the target field path is only one field deep,
    // since we don't support nested field targets yet
    let resolved_relationship_target_mapping = match resolved_relationship_target_model_field_path {
        [t] => t,
        [] => {
            return Err(RelationshipError::EmptyFieldPath {
                location: "target".to_string(),
                type_name: source_type_name.clone(),
                relationship_name: relationship.name.clone(),
            });
        }
        _ => return Err(RelationshipError::NestedFieldPathsNotSupportedYet),
    };

    // Make sure the target model contains the target field
    if !target_model
        .type_fields
        .contains_key(&resolved_relationship_target_mapping.field_name.value)
    {
        return Err(RelationshipError::UnknownTargetFieldInRelationshipMapping {
            relationship_name: relationship.name.clone(),
            source_type: source_type_name.clone(),
            model_name: target_model.name.clone(),
            field_name: resolved_relationship_target_mapping
                .field_name
                .value
                .clone(),
        });
    }

    // Get the NDC column name for the target field
    let target_ndc_column = target_model
        .source
        .as_ref()
        .map(|target_model_source| {
            models::get_ndc_column_for_comparison(
                &target_model.name,
                &target_model.data_type,
                target_model_source,
                &resolved_relationship_target_mapping.field_name,
                || {
                    format!(
                        "the mapping for relationship {} on type {}",
                        relationship.name, source_type_name
                    )
                },
            )
        })
        .transpose()?;

    let resolved_relationship_mapping_target =
        RelationshipModelMappingTarget::ModelField(RelationshipModelMappingFieldTarget {
            target_field: RelationshipFieldAccess {
                field_name: resolved_relationship_target_mapping
                    .field_name
                    .value
                    .clone(),
            },
            target_ndc_column,
        });

    Ok(resolved_relationship_mapping_target)
}

fn resolve_relationship_mappings_model_argument_target<'a>(
    argument_mapping_target: &'a ArgumentMappingTarget,
    target_model: &models::Model,
    relationship: &RelationshipV1,
    source_field_name: &FieldName,
    source_field_type: &QualifiedTypeReference,
    source_type_name: &Qualified<CustomTypeName>,
    target_arguments_already_mapped: &mut BTreeSet<&'a ArgumentName>,
) -> Result<RelationshipModelMappingTarget, RelationshipError> {
    // Check if the target argument exists in the target model.
    let Some(target_argument) = target_model
        .arguments
        .get(&argument_mapping_target.argument_name)
    else {
        return Err(
            RelationshipError::UnknownTargetModelArgumentInRelationshipMapping {
                relationship_name: relationship.name.clone(),
                source_type: source_type_name.clone(),
                model_name: target_model.name.clone(),
                argument_name: argument_mapping_target.argument_name.clone(),
            },
        );
    };

    // Check if the target argument is already mapped to a field in the source type.
    if !target_arguments_already_mapped.insert(&argument_mapping_target.argument_name) {
        return Err(
            RelationshipError::ModelArgumentMappingExistsInRelationship {
                argument_name: argument_mapping_target.argument_name.clone(),
                model_name: target_model.name.clone(),
                relationship_name: relationship.name.clone(),
                type_name: source_type_name.clone(),
            },
        );
    }

    // Check that the source field's type matches the argument's type
    if !target_argument_type_is_compatible(source_field_type, &target_argument.argument_type) {
        return Err(RelationshipError::ModelArgumentTargetMappingTypeMismatch {
            source_type: source_type_name.clone(),
            relationship_name: relationship.name.clone(),
            source_field_name: source_field_name.clone(),
            source_field_type: source_field_type.clone(),
            target_model_name: target_model.name.clone(),
            target_argument_name: argument_mapping_target.argument_name.clone(),
            target_argument_type: target_argument.argument_type.clone(),
        });
    }

    Ok(RelationshipModelMappingTarget::Argument(
        argument_mapping_target.argument_name.clone(),
    ))
}

fn target_argument_type_is_compatible(
    source_field_type: &QualifiedTypeReference,
    target_argument_type: &QualifiedTypeReference,
) -> bool {
    // If the source field is nullable, the target argument must also be nullable.
    // But the target argument can be nullable if the source field is not, that's fine
    if source_field_type.nullable && !target_argument_type.nullable {
        return false;
    }

    match (
        &source_field_type.underlying_type,
        &target_argument_type.underlying_type,
    ) {
        (
            QualifiedBaseType::Named(source_qualified_type_name),
            QualifiedBaseType::Named(target_qualified_type_name),
        ) => source_qualified_type_name == target_qualified_type_name,
        (
            QualifiedBaseType::List(source_qualified_type_reference),
            QualifiedBaseType::List(target_qualified_type_reference),
        ) => target_argument_type_is_compatible(
            source_qualified_type_reference,
            target_qualified_type_reference,
        ),
        _ => false,
    }
}

fn resolve_relationship_mappings_command(
    relationship: &RelationshipV1,
    source_type_name: &Qualified<CustomTypeName>,
    source_type: &object_types::ObjectTypeRepresentation,
    target_command: &commands::Command,
) -> Result<Vec<RelationshipCommandMapping>, RelationshipError> {
    let mut resolved_relationship_mappings = Vec::new();
    let mut source_fields_already_mapped = BTreeSet::new();
    let mut target_arguments_already_mapped = BTreeSet::new();

    for relationship_mapping in &relationship.mapping {
        let (resolved_relationship_source_mapping, _source_field_type) =
            resolve_relationship_source_mapping(
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
                return Err(
                    RelationshipError::ModelFieldCannotBeUsedInCommandRelationship {
                        relationship_name: relationship.name.clone(),
                        type_name: source_type_name.clone(),
                    },
                );
            }
        };

        // Check if the target argument exists in the target command.
        if !target_command.arguments.contains_key(target_argument_name) {
            return Err(
                RelationshipError::UnknownTargetCommandArgumentInRelationshipMapping {
                    relationship_name: relationship.name.clone(),
                    source_type: source_type_name.clone(),
                    command_name: target_command.name.clone(),
                    argument_name: target_argument_name.clone(),
                },
            );
        }

        // Check if the target argument is already mapped to a field in the source type.
        if !target_arguments_already_mapped.insert(target_argument_name) {
            return Err(
                RelationshipError::CommandArgumentMappingExistsInRelationship {
                    argument_name: target_argument_name.clone(),
                    command_name: target_command.name.clone(),
                    relationship_name: relationship.name.clone(),
                    type_name: source_type_name.clone(),
                },
            );
        };

        // Check if the source field is already mapped to a target argument
        let resolved_relationship_mapping = {
            if source_fields_already_mapped
                .insert(&resolved_relationship_source_mapping.field_name.value)
            {
                Ok(RelationshipCommandMapping {
                    source_field: RelationshipFieldAccess {
                        field_name: resolved_relationship_source_mapping
                            .field_name
                            .value
                            .clone(),
                    },
                    argument_name: target_argument_name.clone(),
                })
            } else {
                Err(RelationshipError::MappingExistsInRelationship {
                    type_name: source_type_name.clone(),
                    field_name: resolved_relationship_source_mapping
                        .field_name
                        .value
                        .clone(),
                    relationship_name: relationship.name.clone(),
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
    issues: &mut Vec<ObjectRelationshipsIssue>,
) -> Result<Option<RelationshipCapabilities>, RelationshipError> {
    let Some(data_connector) = source_data_connector else {
        return Ok(None);
    };

    let resolved_data_connector =
        data_connectors
            .0
            .get(&data_connector.name)
            .ok_or_else(|| match target_name {
                RelationshipTargetName::Model(model_name) => {
                    RelationshipError::ModelError(models::ModelsError::UnknownModelDataConnector {
                        model_name: model_name.clone(),
                        data_connector: data_connector.name.clone(),
                        data_connector_path: None,
                    })
                }
                RelationshipTargetName::Command(command_name) => RelationshipError::CommandError(
                    commands::CommandsError::UnknownCommandDataConnector {
                        command_name: command_name.clone(),
                        data_connector: data_connector.name.clone(),
                    },
                ),
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
        return Err(RelationshipError::RelationshipTargetDoesNotSupportForEach {
            type_name: type_name.clone(),
            relationship_name: relationship_name.clone(),
            data_connector_name: data_connector.name.clone(),
        });
    };

    // if relationship is local, error if relationship and variables capabilities are not available
    if Some(&data_connector.name) == target_data_connector.as_ref()
        && !capabilities.supports_query_variables
        && capabilities.supports_relationships.is_none()
    {
        issues.push(ObjectRelationshipsIssue::LocalRelationshipDataConnectorDoesNotSupportRelationshipsOrVariables {
            type_name: type_name.clone(),
            relationship_name: relationship_name.clone(),
            data_connector_name: data_connector.name.clone(),
        });
    }

    Ok(Some(RelationshipCapabilities {
        foreach: (),
        supports_relationships: capabilities.supports_relationships.clone(),
    }))
}

fn resolve_aggregate_relationship(
    model_relationship_target: &open_dds::relationships::ModelRelationshipTarget,
    resolved_target_model: &models::Model,
    relationship: &RelationshipV1,
    source_type_name: &Qualified<CustomTypeName>,
    aggregate_expressions: &BTreeMap<
        Qualified<AggregateExpressionName>,
        aggregates::AggregateExpression,
    >,
    object_types: &type_permissions::ObjectTypesWithPermissions,
    graphql_config: &graphql_config::GraphqlConfig,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars,
    >,
) -> Result<Option<AggregateRelationship>, RelationshipError> {
    // If an aggregate has been specified
    let aggregate_expression_name_and_description = model_relationship_target
        .aggregate
        .as_ref()
        .map(|aggregate| -> Result<_, RelationshipError> {
            // Ensure the relationship is an array relationship
            if model_relationship_target.relationship_type != RelationshipType::Array {
                return Err(
                    RelationshipError::AggregateIsOnlyAllowedOnArrayRelationships {
                        type_name: source_type_name.clone(),
                        relationship_name: relationship.name.clone(),
                    },
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
                data_connector_scalars,
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
                .ok_or_else(||
                        RelationshipError::GraphqlError(graphql_config::GraphqlConfigError::MissingAggregateFilterInputFieldNameInGraphqlConfig),
                )?;

            Ok(AggregateRelationship {
                field_name,
                aggregate_expression,
                filter_input_field_name,
                description: description.clone(),
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
    issues: &mut Vec<ObjectRelationshipsIssue>,
) -> Result<RelationshipField, RelationshipError> {
    let qualified_target_model_name = Qualified::new(
        target_model
            .subgraph()
            .unwrap_or(source_type_name.subgraph.clone()),
        target_model.name.clone(),
    );
    let resolved_target_model = models.get(&qualified_target_model_name).ok_or_else(|| {
        RelationshipError::UnknownTargetModelUsedInRelationship {
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
        issues,
    )?;

    let mappings = resolve_relationship_mappings_model(
        relationship,
        source_type_name,
        source_type,
        resolved_target_model,
    )?;

    let relationship_aggregate = resolve_aggregate_relationship(
        target_model,
        resolved_target_model,
        relationship,
        source_type_name,
        aggregate_expressions,
        object_types,
        graphql_config,
        data_connector_scalars,
    )?;

    let relationship_field = RelationshipField {
        field_name: make_relationship_field_name(&relationship.name)?,
        relationship_name: relationship.name.clone(),
        source: source_type_name.clone(),
        target: RelationshipTarget::Model(Box::new(ModelRelationshipTarget {
            model_name: qualified_target_model_name,
            relationship_type: target_model.relationship_type.clone(),
            target_typename: resolved_target_model.data_type.clone(),
            mappings,
            relationship_aggregate,
        })),
        target_capabilities,
        description: relationship.description.clone(),
        deprecated: relationship.deprecated.clone(),
    };

    Ok(relationship_field)
}

// create the graphql name for a non-aggregate relationship field name
pub fn make_relationship_field_name(
    relationship_name: &RelationshipName,
) -> Result<ast::Name, RelationshipError> {
    mk_name(relationship_name.as_str()).map_err(RelationshipError::from)
}

fn resolve_command_relationship_field(
    target_command: &open_dds::relationships::CommandRelationshipTarget,
    commands: &IndexMap<Qualified<CommandName>, commands::Command>,
    data_connectors: &data_connectors::DataConnectors,
    source_type_name: &Qualified<CustomTypeName>,
    relationship: &RelationshipV1,
    source_type: &object_types::ObjectTypeRepresentation,
    issues: &mut Vec<ObjectRelationshipsIssue>,
) -> Result<RelationshipField, RelationshipError> {
    let qualified_target_command_name = Qualified::new(
        target_command
            .subgraph()
            .unwrap_or(source_type_name.subgraph.clone()),
        target_command.name.clone(),
    );
    let resolved_target_command =
        commands
            .get(&qualified_target_command_name)
            .ok_or_else(
                || RelationshipError::UnknownTargetCommandUsedInRelationship {
                    type_name: source_type_name.clone(),
                    relationship_name: relationship.name.clone(),
                    command_name: qualified_target_command_name.clone(),
                },
            )?;

    // validate the command source
    if let Some(command_source) = &resolved_target_command.source {
        issues.extend(validate_command_source(
            source_type_name,
            &relationship.name,
            &qualified_target_command_name,
            command_source,
        ));
    }

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
        issues,
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

// validate the command source
fn validate_command_source(
    source_type_name: &Qualified<CustomTypeName>,
    relationship_name: &RelationshipName,
    command_name: &Qualified<CommandName>,
    source: &commands::CommandSource,
) -> Vec<ObjectRelationshipsIssue> {
    let mut issues = Vec::new();
    // Procedure based commands are operations that modify data and should not be used in relationships
    // since relationships are meant to be defined between read-only data points.
    // Using these commands in relationships would cause unintended side effects during data fetching.
    if let DataConnectorCommand::Procedure(procedure) = &source.source {
        issues.push(
            ObjectRelationshipsIssue::ProcedureCommandRelationshipsNotSupported {
                type_name: source_type_name.clone(),
                relationship_name: relationship_name.clone(),
                command_name: command_name.clone(),
                procedure_name: procedure.clone(),
            },
        );
    }
    issues
}

fn resolve_relationship_field(
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
    issues: &mut Vec<ObjectRelationshipsIssue>,
) -> Result<RelationshipField, RelationshipError> {
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
                issues,
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
                issues,
            )?;
            Ok(command_relationship_field)
        }
    }
}
