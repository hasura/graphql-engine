use crate::types::{PlanError, RelationshipError};
use indexmap::IndexMap;
use metadata_resolve::{
    Qualified, RelationshipCommandMapping, RelationshipModelMapping, TypeMapping,
};
use open_dds::{
    arguments::ArgumentName,
    commands::CommandName,
    data_connector::{CollectionName, DataConnectorColumnName},
    models::ModelName,
    relationships::{RelationshipName, RelationshipType},
    types::{CustomTypeName, DataConnectorArgumentName, FieldName},
};
use plan_types::{
    Argument, ComparisonTarget, ComparisonValue, Field, LocalCommandRelationshipInfo,
    LocalFieldComparison, LocalModelRelationshipInfo, NdcFieldAlias, Relationship,
    ResolvedFilterExpression, SourceFieldAlias, TargetField,
};
use std::collections::{BTreeMap, HashMap};

pub fn process_model_relationship_definition(
    relationship_info: &LocalModelRelationshipInfo,
) -> Result<Relationship, PlanError> {
    let &LocalModelRelationshipInfo {
        relationship_name,
        relationship_type,
        source_type,
        source_data_connector: _,
        source_type_mappings,
        target_model_name,
        target_source,
        target_type: _,
        mappings,
    } = relationship_info;

    let mut column_mapping = BTreeMap::new();
    let mut arguments = BTreeMap::new();
    for metadata_resolve::RelationshipModelMapping {
        source_field: source_field_path,
        target,
    } in mappings
    {
        let source_column = get_relationship_field_mapping_of_field_name(
            source_type_mappings,
            source_type,
            relationship_name,
            &source_field_path.field_name,
        )
        .map_err(RelationshipError::RelationshipFieldMappingError)?;

        match target {
            metadata_resolve::RelationshipModelMappingTarget::ModelField(
                metadata_resolve::RelationshipModelMappingFieldTarget {
                    target_field: _,
                    target_ndc_column,
                },
            ) => {
                let target_column = target_ndc_column.as_ref().ok_or_else(|| {
                    PlanError::Internal(format!(
                        "No column mapping for relationship {relationship_name} on {source_type}"
                    ))
                })?;

                if column_mapping
                    .insert(source_column.column, target_column.column.clone())
                    .is_some()
                {
                    Err(PlanError::Relationship(
                        RelationshipError::MappingExistsInRelationship {
                            source_column: source_field_path.field_name.clone(),
                            relationship_name: relationship_name.clone(),
                        },
                    ))?;
                }
            }
            metadata_resolve::RelationshipModelMappingTarget::Argument(argument_name) => {
                let relationship_argument = plan_types::RelationshipArgument::Column {
                    name: source_column.column,
                };

                let connector_argument_name = target_source
                    .argument_mappings
                    .get(argument_name)
                    .ok_or_else(|| {
                        PlanError::Relationship(
                            RelationshipError::MissingArgumentMappingInModelRelationship {
                                source_type: source_type.clone(),
                                relationship_name: relationship_name.clone(),
                                model_name: target_model_name.clone(),
                                argument_name: argument_name.clone(),
                            },
                        )
                    })?;

                if arguments
                    .insert(
                        DataConnectorArgumentName::from(connector_argument_name.as_str()),
                        relationship_argument,
                    )
                    .is_some()
                {
                    Err(PlanError::Relationship(
                        RelationshipError::MappingExistsInRelationship {
                            source_column: source_field_path.field_name.clone(),
                            relationship_name: relationship_name.clone(),
                        },
                    ))?;
                }
            }
        }
    }
    let relationship = Relationship {
        column_mapping,
        relationship_type: relationship_type.clone(),
        target_collection: target_source.collection.clone(),
        arguments,
    };
    Ok(relationship)
}

pub fn process_command_relationship_definition(
    relationship_info: &LocalCommandRelationshipInfo,
) -> Result<Relationship, PlanError> {
    let &LocalCommandRelationshipInfo {
        relationship_name,
        source_type,
        source_type_mappings,
        command_name,
        argument_mappings,
        function_name,
        mappings,
    } = relationship_info;

    let mut arguments = BTreeMap::new();
    for metadata_resolve::RelationshipCommandMapping {
        source_field: source_field_path,
        argument_name: target_argument,
    } in mappings
    {
        let source_column = get_relationship_field_mapping_of_field_name(
            source_type_mappings,
            source_type,
            relationship_name,
            &source_field_path.field_name,
        )
        .map_err(RelationshipError::RelationshipFieldMappingError)?;

        let relationship_argument = plan_types::RelationshipArgument::Column {
            name: source_column.column,
        };

        let connector_argument_name = argument_mappings.get(target_argument).ok_or_else(|| {
            PlanError::Relationship(
                RelationshipError::MissingArgumentMappingInCommandRelationship {
                    source_type: source_type.clone(),
                    relationship_name: relationship_name.clone(),
                    command_name: command_name.clone(),
                    argument_name: target_argument.clone(),
                },
            )
        })?;

        if arguments
            .insert(
                DataConnectorArgumentName::from(connector_argument_name.as_str()),
                relationship_argument,
            )
            .is_some()
        {
            Err(PlanError::Relationship(
                RelationshipError::ArgumentMappingExistsInRelationship {
                    argument_name: target_argument.clone(),
                    relationship_name: relationship_name.clone(),
                },
            ))?;
        }
    }

    let relationship = Relationship {
        column_mapping: BTreeMap::new(),
        relationship_type: RelationshipType::Object,
        target_collection: CollectionName::from(function_name.as_str()),
        arguments,
    };
    Ok(relationship)
}

pub struct CommandRemoteRelationshipParts {
    pub join_mapping: HashMap<FieldName, (SourceFieldAlias, TargetField)>,
    pub phantom_fields: BTreeMap<NdcFieldAlias, Field>,
    pub arguments: IndexMap<DataConnectorArgumentName, Argument>,
}

pub fn calculate_remote_relationship_fields_for_command_target(
    object_type_name: &Qualified<CustomTypeName>,
    relationship_name: &RelationshipName,
    command_name: &Qualified<CommandName>,
    relationship_command_mappings: &Vec<RelationshipCommandMapping>,
    source_type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    target_argument_mappings: &BTreeMap<ArgumentName, DataConnectorArgumentName>,
) -> Result<CommandRemoteRelationshipParts, RelationshipError> {
    let mut join_mapping = HashMap::new();
    let mut phantom_fields = BTreeMap::new();
    let mut arguments = IndexMap::new();

    for metadata_resolve::RelationshipCommandMapping {
        source_field,
        argument_name,
    } in relationship_command_mappings
    {
        let source_column = get_relationship_field_mapping_of_field_name(
            source_type_mappings,
            object_type_name,
            relationship_name,
            &source_field.field_name,
        )
        .map_err(RelationshipError::RelationshipFieldMappingError)?;

        let ProcessedRemoteRelationship {
            source_field_alias: ndc_field_alias,
            field: processed_field,
        } = process_remote_relationship_field_mapping(&source_column.column);

        phantom_fields.insert(processed_field.0, processed_field.1);

        let target_argument = TargetField::Argument(argument_name.clone());

        // add argument referencing variable
        let target_value_variable = target_argument.make_variable_name();

        let data_connector_argument_name =
            target_argument_mappings.get(argument_name).ok_or_else(|| {
                RelationshipError::MissingArgumentMappingInCommandRelationship {
                    source_type: object_type_name.clone(),
                    relationship_name: relationship_name.clone(),
                    command_name: command_name.clone(),
                    argument_name: argument_name.clone(),
                }
            })?;

        // collect these as we go
        arguments.insert(
            data_connector_argument_name.clone(),
            Argument::Variable {
                name: target_value_variable,
            },
        );

        // add join mapping
        join_mapping.insert(
            source_field.field_name.clone(),
            (ndc_field_alias, target_argument),
        );
    }

    Ok(CommandRemoteRelationshipParts {
        join_mapping,
        phantom_fields,
        arguments,
    })
}

pub struct ModelRemoteRelationshipParts {
    pub join_mapping: HashMap<FieldName, (SourceFieldAlias, TargetField)>,
    pub phantom_fields: BTreeMap<NdcFieldAlias, Field>,
    pub relationship_join_filter_expressions: Vec<ResolvedFilterExpression>,
    pub arguments: IndexMap<DataConnectorArgumentName, Argument>,
}

pub fn calculate_remote_relationship_fields_for_model_target(
    object_type_name: &Qualified<CustomTypeName>,
    relationship_name: &RelationshipName,
    model_name: &Qualified<ModelName>,
    relationship_model_mappings: &Vec<RelationshipModelMapping>,
    source_type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    target_argument_mappings: &BTreeMap<ArgumentName, DataConnectorArgumentName>,
) -> Result<ModelRemoteRelationshipParts, RelationshipError> {
    let mut join_mapping = HashMap::new();
    let mut phantom_fields = BTreeMap::new();
    let mut relationship_join_filter_expressions = vec![];
    let mut arguments = IndexMap::new();

    for metadata_resolve::RelationshipModelMapping {
        source_field,
        target,
    } in relationship_model_mappings
    {
        let source_column = get_relationship_field_mapping_of_field_name(
            source_type_mappings,
            object_type_name,
            relationship_name,
            &source_field.field_name,
        )
        .map_err(RelationshipError::RelationshipFieldMappingError)?;

        let ProcessedRemoteRelationship {
            source_field_alias: ndc_field_alias,
            field: processed_field,
        } = process_remote_relationship_field_mapping(&source_column.column);

        phantom_fields.insert(processed_field.0, processed_field.1);

        match target {
            metadata_resolve::RelationshipModelMappingTarget::ModelField(
                metadata_resolve::RelationshipModelMappingFieldTarget {
                    target_ndc_column,
                    target_field,
                },
            ) => {
                let target_ndc_column = target_ndc_column.clone().ok_or_else(|| {
                    RelationshipError::MissingTargetColumn {
                        source_field: source_field.field_name.clone(),
                        target_field: target_field.field_name.clone(),
                        relationship_name: relationship_name.clone(),
                    }
                })?;

                // add join mapping
                let target_model_field = TargetField::ModelField(
                    target_field.field_name.clone(),
                    target_ndc_column.clone(),
                );
                let target_value_variable = target_model_field.make_variable_name();
                join_mapping.insert(
                    source_field.field_name.clone(),
                    (ndc_field_alias, target_model_field),
                );

                // add extra comparison to filter for target model
                let comparison_exp = LocalFieldComparison::BinaryComparison {
                    column: ComparisonTarget::Column {
                        name: target_ndc_column.column.clone(),
                        field_path: vec![],
                    },
                    operator: target_ndc_column.equal_operator.clone(),
                    value: ComparisonValue::Variable {
                        name: target_value_variable,
                    },
                };

                // collect these as we go
                relationship_join_filter_expressions.push(
                    ResolvedFilterExpression::LocalFieldComparison(comparison_exp),
                );
            }
            metadata_resolve::RelationshipModelMappingTarget::Argument(argument_name) => {
                // add argument referencing variable
                let target_argument = TargetField::Argument(argument_name.clone());
                let target_value_variable = target_argument.make_variable_name();

                let data_connector_argument_name =
                    target_argument_mappings.get(argument_name).ok_or_else(|| {
                        RelationshipError::MissingArgumentMappingInModelRelationship {
                            source_type: object_type_name.clone(),
                            relationship_name: relationship_name.clone(),
                            model_name: model_name.clone(),
                            argument_name: argument_name.clone(),
                        }
                    })?;

                // collect these as we go
                arguments.insert(
                    data_connector_argument_name.clone(),
                    Argument::Variable {
                        name: target_value_variable,
                    },
                );

                // add join mapping
                join_mapping.insert(
                    source_field.field_name.clone(),
                    (ndc_field_alias, target_argument),
                );
            }
        }
    }

    Ok(ModelRemoteRelationshipParts {
        join_mapping,
        phantom_fields,
        relationship_join_filter_expressions,
        arguments,
    })
}

struct ProcessedRemoteRelationship {
    source_field_alias: SourceFieldAlias,
    field: (NdcFieldAlias, Field),
}

/// Processes a remote relationship field mapping, and returns the alias used in
/// the NDC IR for that field
fn process_remote_relationship_field_mapping(
    ndc_column_name: &DataConnectorColumnName,
) -> ProcessedRemoteRelationship {
    let internal_alias = make_hasura_phantom_field(ndc_column_name);
    ProcessedRemoteRelationship {
        source_field_alias: SourceFieldAlias(internal_alias.clone()),
        field: (
            NdcFieldAlias::from(internal_alias.as_str()),
            Field::Column {
                column: ndc_column_name.clone(),
                fields: None,
                arguments: BTreeMap::new(),
            },
        ),
    }
}

fn make_hasura_phantom_field(ndc_column_name: &DataConnectorColumnName) -> String {
    format!("__hasura_phantom_field__{}", ndc_column_name.as_str())
}

#[derive(Debug, thiserror::Error)]
pub enum RelationshipFieldMappingError {
    #[error("Type mapping not found for the type name {type_name:} while executing the relationship {relationship_name:}")]
    TypeMappingNotFoundForRelationship {
        type_name: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
    },

    #[error("Field mapping not found for the field {field_name:} of type {type_name:} while executing the relationship {relationship_name:}")]
    FieldMappingNotFoundForRelationship {
        type_name: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
        field_name: FieldName,
    },
}

pub fn get_relationship_field_mapping_of_field_name(
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    type_name: &Qualified<CustomTypeName>,
    relationship_name: &RelationshipName,
    field_name: &FieldName,
) -> Result<metadata_resolve::FieldMapping, RelationshipFieldMappingError> {
    let type_mapping = type_mappings.get(type_name).ok_or_else(|| {
        RelationshipFieldMappingError::TypeMappingNotFoundForRelationship {
            type_name: type_name.clone(),
            relationship_name: relationship_name.clone(),
        }
    })?;
    match type_mapping {
        metadata_resolve::TypeMapping::Object { field_mappings, .. } => Ok(field_mappings
            .get(field_name)
            .ok_or_else(
                || RelationshipFieldMappingError::FieldMappingNotFoundForRelationship {
                    type_name: type_name.clone(),
                    relationship_name: relationship_name.clone(),
                    field_name: field_name.clone(),
                },
            )?
            .clone()),
    }
}
