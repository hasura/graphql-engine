use crate::metadata_accessor::OutputObjectTypeView;
use crate::types::{PlanError, RelationshipError};
use hasura_authn_core::Session;
use indexmap::IndexMap;
use metadata_resolve::{
    Metadata, Qualified, QualifiedTypeReference, RelationshipCommandMapping,
    RelationshipModelMapping, TypeMapping,
};
use open_dds::{
    arguments::ArgumentName,
    commands::CommandName,
    data_connector::{CollectionName, DataConnectorColumnName},
    models::ModelName,
    query::ObjectSubSelection,
    relationships::{RelationshipName, RelationshipType},
    types::{CustomTypeName, DataConnectorArgumentName, FieldName},
};
use plan_types::{
    Argument, ComparisonTarget, ComparisonValue, Field, LocalCommandRelationshipInfo,
    LocalFieldComparison, LocalModelRelationshipInfo, NdcFieldAlias, Relationship,
    RemoteJoinFieldMapping, RemoteJoinObjectFieldMapping, RemoteJoinObjectTargetField,
    ResolvedFilterExpression, SourceFieldAlias, TargetField,
};
use std::collections::BTreeMap;
use std::collections::VecDeque;

pub fn process_model_relationship_definition(
    relationship_info: &LocalModelRelationshipInfo,
) -> Result<Relationship, PlanError> {
    let &LocalModelRelationshipInfo {
        relationship_name,
        relationship_type,
        source_type,
        source_type_mappings,
        target_model_name,
        target_source,
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
    pub join_mapping: BTreeMap<FieldName, RemoteJoinFieldMapping>,
    pub object_type_field_mappings:
        BTreeMap<Qualified<CustomTypeName>, RemoteJoinObjectFieldMapping>,
    pub arguments: IndexMap<DataConnectorArgumentName, Argument>,
}

pub fn calculate_remote_relationship_fields_for_command_target(
    session: &Session,
    metadata: &Metadata,
    object_type: &OutputObjectTypeView,
    relationship_name: &RelationshipName,
    command_name: &Qualified<CommandName>,
    relationship_command_mappings: &Vec<RelationshipCommandMapping>,
    source_type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    target_type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    target_argument_mappings: &BTreeMap<ArgumentName, DataConnectorArgumentName>,
    selection: &IndexMap<open_dds::query::Alias, ObjectSubSelection>,
    ndc_fields: &mut IndexMap<NdcFieldAlias, Field>,
) -> Result<CommandRemoteRelationshipParts, RelationshipError> {
    let mut join_mapping = BTreeMap::new();
    let mut arguments = IndexMap::new();
    let mut object_type_field_mappings = BTreeMap::new();

    for metadata_resolve::RelationshipCommandMapping {
        source_field,
        argument_name,
    } in relationship_command_mappings
    {
        let source_field_type = object_type
            .get_field(&source_field.field_name, &session.role)
            .map_err(|_| RelationshipError::MissingSourceField {
                relationship_name: relationship_name.clone(),
                source_field: source_field.field_name.clone(),
            })?
            .field_type;

        collect_remote_join_object_type_field_mappings(
            source_field_type,
            &mut object_type_field_mappings,
            relationship_name,
            source_type_mappings,
            target_type_mappings,
            &metadata.object_types,
        )?;

        let source_column = get_relationship_field_mapping_of_field_name(
            source_type_mappings,
            object_type.object_type_name,
            relationship_name,
            &source_field.field_name,
        )
        .map_err(RelationshipError::RelationshipFieldMappingError)?;

        let ndc_field_alias = process_remote_relationship_field_mapping(
            selection,
            &source_column.column,
            &source_field.field_name,
            ndc_fields,
        );

        let target_argument = TargetField::Argument(argument_name.clone());

        // add argument referencing variable
        let target_value_variable = target_argument.make_variable_name();

        let data_connector_argument_name =
            target_argument_mappings.get(argument_name).ok_or_else(|| {
                RelationshipError::MissingArgumentMappingInCommandRelationship {
                    source_type: object_type.object_type_name.clone(),
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
            RemoteJoinFieldMapping {
                source_field_alias: ndc_field_alias.clone(),
                source_field_type: source_field_type.clone(),
                target_field: target_argument.clone(),
            },
        );
    }

    Ok(CommandRemoteRelationshipParts {
        join_mapping,
        object_type_field_mappings,
        arguments,
    })
}

pub struct ModelRemoteRelationshipParts {
    pub join_mapping: BTreeMap<FieldName, RemoteJoinFieldMapping>,
    pub object_type_field_mappings:
        BTreeMap<Qualified<CustomTypeName>, RemoteJoinObjectFieldMapping>,
    pub relationship_join_filter_expressions: VecDeque<ResolvedFilterExpression>,
    pub arguments: IndexMap<DataConnectorArgumentName, Argument>,
}

pub fn calculate_remote_relationship_fields_for_model_target(
    session: &Session,
    metadata: &Metadata,
    object_type: &OutputObjectTypeView,
    relationship_name: &RelationshipName,
    model_name: &Qualified<ModelName>,
    relationship_model_mappings: &Vec<RelationshipModelMapping>,
    source_type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    target_type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    target_argument_mappings: &BTreeMap<ArgumentName, DataConnectorArgumentName>,
    source_selection: &IndexMap<open_dds::query::Alias, ObjectSubSelection>,
    ndc_fields: &mut IndexMap<NdcFieldAlias, Field>,
) -> Result<ModelRemoteRelationshipParts, RelationshipError> {
    let mut join_mapping = BTreeMap::new();
    let mut relationship_join_filter_expressions = VecDeque::new();
    let mut arguments = IndexMap::new();
    let mut object_type_field_mappings = BTreeMap::new();

    for metadata_resolve::RelationshipModelMapping {
        source_field,
        target,
    } in relationship_model_mappings
    {
        let source_field_type = object_type
            .get_field(&source_field.field_name, &session.role)
            .map_err(|_| RelationshipError::MissingSourceField {
                relationship_name: relationship_name.clone(),
                source_field: source_field.field_name.clone(),
            })?
            .field_type;

        collect_remote_join_object_type_field_mappings(
            source_field_type,
            &mut object_type_field_mappings,
            relationship_name,
            source_type_mappings,
            target_type_mappings,
            &metadata.object_types,
        )?;

        let source_column = get_relationship_field_mapping_of_field_name(
            source_type_mappings,
            object_type.object_type_name,
            relationship_name,
            &source_field.field_name,
        )
        .map_err(RelationshipError::RelationshipFieldMappingError)?;

        let ndc_field_alias = process_remote_relationship_field_mapping(
            source_selection,
            &source_column.column,
            &source_field.field_name,
            ndc_fields,
        );

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
                    RemoteJoinFieldMapping {
                        source_field_alias: ndc_field_alias.clone(),
                        source_field_type: source_field_type.clone(),
                        target_field: target_model_field.clone(),
                    },
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
                relationship_join_filter_expressions.push_back(
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
                            source_type: object_type.object_type_name.clone(),
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
                    RemoteJoinFieldMapping {
                        source_field_alias: ndc_field_alias.clone(),
                        source_field_type: source_field_type.clone(),
                        target_field: target_argument.clone(),
                    },
                );
            }
        }
    }

    Ok(ModelRemoteRelationshipParts {
        join_mapping,
        object_type_field_mappings,
        relationship_join_filter_expressions,
        arguments,
    })
}

pub fn collect_remote_join_object_type_field_mappings(
    source_field_type: &QualifiedTypeReference,
    object_type_field_mappings: &mut BTreeMap<
        Qualified<CustomTypeName>,
        RemoteJoinObjectFieldMapping,
    >,
    relationship_name: &RelationshipName,
    source_type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    target_type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    object_types: &BTreeMap<
        Qualified<CustomTypeName>,
        metadata_resolve::ObjectTypeWithRelationships,
    >,
) -> Result<(), RelationshipError> {
    // Get the object type of the field (if it is one)
    let (source_field_type_name, field_object_type) =
        match source_field_type.get_underlying_type_name() {
            // Scalar type, no field mapping required
            metadata_resolve::QualifiedTypeName::Inbuilt(_) => return Ok(()),

            metadata_resolve::QualifiedTypeName::Custom(source_field_type_name) => {
                // Check if already mapped
                if object_type_field_mappings.contains_key(source_field_type_name) {
                    // Already mapped, no need to do it again
                    return Ok(());
                }

                let Some(field_object_type) = object_types.get(source_field_type_name) else {
                    // Field is not an object type - no field mapping required
                    return Ok(());
                };

                (source_field_type_name, field_object_type)
            }
        };

    // Get the field mappings from the source
    let TypeMapping::Object {
        field_mappings: source_field_mappings,
        ..
    } = source_type_mappings
        .get(source_field_type_name)
        .ok_or_else(|| {
            RelationshipError::RelationshipFieldMappingError(
                RelationshipFieldMappingError::TypeMappingNotFoundForRelationship {
                    type_name: source_field_type_name.clone(),
                    relationship_name: relationship_name.clone(),
                },
            )
        })?;

    // Get the field mappings from target
    let TypeMapping::Object {
        field_mappings: target_field_mappings,
        ..
    } = target_type_mappings
        .get(source_field_type_name)
        .ok_or_else(|| {
            RelationshipError::RelationshipFieldMappingError(
                RelationshipFieldMappingError::TypeMappingNotFoundForRelationship {
                    type_name: source_field_type_name.clone(),
                    relationship_name: relationship_name.clone(),
                },
            )
        })?;

    // Construct a mapping from source data connector column names to target data connector column names
    let object_field_mapping = field_object_type
        .object_type
        .fields
        .iter()
        .map(|(field_name, field_definition)| {
            let source_field_mapping = source_field_mappings.get(field_name).ok_or_else(|| {
                RelationshipError::RelationshipFieldMappingError(
                    RelationshipFieldMappingError::FieldMappingNotFoundForRelationship {
                        type_name: source_field_type_name.clone(),
                        relationship_name: relationship_name.clone(),
                        field_name: field_name.clone(),
                    },
                )
            })?;

            let target_field_mapping = target_field_mappings.get(field_name).ok_or_else(|| {
                RelationshipError::RelationshipFieldMappingError(
                    RelationshipFieldMappingError::FieldMappingNotFoundForRelationship {
                        type_name: source_field_type_name.clone(),
                        relationship_name: relationship_name.clone(),
                        field_name: field_name.clone(),
                    },
                )
            })?;

            Ok((
                source_field_mapping.column.clone(),
                RemoteJoinObjectTargetField {
                    name: target_field_mapping.column.clone(),
                    field_type: field_definition.field_type.clone(),
                },
            ))
        })
        .collect::<Result<RemoteJoinObjectFieldMapping, RelationshipError>>()?;

    object_type_field_mappings.insert(source_field_type_name.clone(), object_field_mapping);

    // Loop through all the fields of this object type and collect any mappings required
    // for fields that also use object types
    for field in field_object_type.object_type.fields.values() {
        collect_remote_join_object_type_field_mappings(
            &field.field_type,
            object_type_field_mappings,
            relationship_name,
            source_type_mappings,
            target_type_mappings,
            object_types,
        )?;
    }

    Ok(())
}

/// Processes a remote relationship field mapping, and returns the alias used in
/// the NDC IR for that field
///
/// - if the selection set DOES NOT contain the field, insert it into the NDC IR
///   (with an internal alias), and return the alias
/// - if the selection set already contains the field, but it is a nested field
///   selection, we can't use it because it might be a partial select of the type
///   and we need the whole value for the join, so we insert a fresh selection into
///   the NDC IR (with an internal alias) and return the alias
/// - if the selection set already contains the field, do not insert the field
///   in NDC IR, and return the existing alias
///
fn process_remote_relationship_field_mapping(
    source_selection: &IndexMap<open_dds::query::Alias, open_dds::query::ObjectSubSelection>,
    ndc_column_name: &DataConnectorColumnName,
    field_name: &FieldName,
    ndc_fields: &mut IndexMap<NdcFieldAlias, Field>,
) -> SourceFieldAlias {
    let found = source_selection
        .iter()
        .find_map(|(alias, column)| match column {
            open_dds::query::ObjectSubSelection::Field(field) => {
                if field.target.field_name == *field_name && field.selection.is_none() {
                    Some(alias)
                } else {
                    None
                }
            }
            _ => None,
        });

    // if we're not already selecting this field, add it to selection and generate a field name for
    // it
    match found {
        None => {
            let internal_alias = make_hasura_phantom_field(ndc_column_name);
            ndc_fields.insert(
                NdcFieldAlias::from(internal_alias.as_str()),
                Field::Column {
                    column: ndc_column_name.clone(),
                    fields: None,
                    arguments: BTreeMap::new(),
                },
            );
            SourceFieldAlias(internal_alias)
        }
        Some(field_alias) => SourceFieldAlias(field_alias.to_string()),
    }
}

fn make_hasura_phantom_field(ndc_column_name: &DataConnectorColumnName) -> String {
    format!("__hasura_phantom_field__{}", ndc_column_name.as_str())
}

#[derive(Debug, thiserror::Error)]
pub enum RelationshipFieldMappingError {
    #[error(
        "Type mapping not found for the type name {type_name:} while executing the relationship {relationship_name:}"
    )]
    TypeMappingNotFoundForRelationship {
        type_name: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
    },

    #[error(
        "Field mapping not found for the field {field_name:} of type {type_name:} while executing the relationship {relationship_name:}"
    )]
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
