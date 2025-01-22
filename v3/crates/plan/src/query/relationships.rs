use crate::types::{PlanError, RelationshipError};
use indexmap::IndexMap;
use metadata_resolve::{Qualified, RelationshipModelMapping, TypeMapping};
use open_dds::{
    data_connector::{CollectionName, DataConnectorColumnName},
    query::{Alias, ModelSelection, ObjectSubSelection},
    relationships::{RelationshipName, RelationshipType},
    types::{CustomTypeName, DataConnectorArgumentName, FieldName},
};
use plan_types::{
    ComparisonTarget, ComparisonValue, Field, LocalCommandRelationshipInfo, LocalFieldComparison,
    LocalModelRelationshipInfo, NdcFieldAlias, Relationship, ResolvedFilterExpression,
    SourceFieldAlias, TargetField, VariableName,
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
        target_source,
        target_type: _,
        mappings,
    } = relationship_info;

    let mut column_mapping = BTreeMap::new();
    for metadata_resolve::RelationshipModelMapping {
        source_field: source_field_path,
        target_field: _,
        target_ndc_column,
    } in mappings
    {
        let target_column = target_ndc_column.as_ref().ok_or_else(|| {
            PlanError::Internal(format!(
                "No column mapping for relationship {relationship_name} on {source_type}"
            ))
        })?;

        let source_column = metadata_resolve::get_field_mapping_of_field_name(
            source_type_mappings,
            source_type,
            relationship_name,
            &source_field_path.field_name,
        )
        .map_err(|e| PlanError::Internal(e.to_string()))?;

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
    let relationship = Relationship {
        column_mapping,
        relationship_type: relationship_type.clone(),
        target_collection: target_source.collection.clone(),
        arguments: BTreeMap::new(),
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
        let source_column = metadata_resolve::get_field_mapping_of_field_name(
            source_type_mappings,
            source_type,
            relationship_name,
            &source_field_path.field_name,
        )
        .map_err(|e| PlanError::Internal(e.to_string()))?;

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
                RelationshipError::MappingExistsInRelationship {
                    source_column: source_field_path.field_name.clone(),
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

pub struct RemoteRelationshipParts {
    pub join_mapping: HashMap<FieldName, (SourceFieldAlias, TargetField)>,
    pub phantom_fields: BTreeMap<NdcFieldAlias, Field>,
    pub relationship_join_filter_expressions: Vec<ResolvedFilterExpression>,
}

pub fn calculate_remote_relationship_fields_for_model_target(
    object_type_name: &Qualified<CustomTypeName>,
    relationship_name: &RelationshipName,
    relationship_model_mappings: &Vec<RelationshipModelMapping>,
    source_type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    relationship_target_model_selection: &ModelSelection,
) -> Result<RemoteRelationshipParts, RelationshipError> {
    let mut join_mapping = HashMap::new();
    let mut phantom_fields = BTreeMap::new();
    let mut relationship_join_filter_expressions = vec![];

    for metadata_resolve::RelationshipModelMapping {
        source_field,
        target_field,
        target_ndc_column,
    } in relationship_model_mappings
    {
        let source_column = metadata_resolve::get_field_mapping_of_field_name(
            source_type_mappings,
            object_type_name,
            relationship_name,
            &source_field.field_name,
        )
        .map_err(RelationshipError::RelationshipFieldMappingError)?;

        let ProcessedRemoteRelationship {
            source_field_alias: ndc_field_alias,
            field: processed_field,
        } = process_remote_relationship_field_mapping(
            &relationship_target_model_selection.selection,
            &source_column.column,
        );

        if let Some((ndc_field_alias, processed_field)) = processed_field {
            phantom_fields.insert(ndc_field_alias, processed_field);
        }

        let target_ndc_column =
            target_ndc_column
                .clone()
                .ok_or_else(|| RelationshipError::MissingTargetColumn {
                    source_field: source_field.field_name.clone(),
                    target_field: target_field.field_name.clone(),
                    relationship_name: relationship_name.clone(),
                })?;

        // add join mapping
        join_mapping.insert(
            source_field.field_name.clone(),
            (
                ndc_field_alias,
                TargetField::ModelField((
                    target_field.field_name.clone(),
                    target_ndc_column.clone(),
                )),
            ),
        );

        // add extra comparison to filter for target model
        let target_value_variable = format!("${}", &target_ndc_column.column);
        let comparison_exp = LocalFieldComparison::BinaryComparison {
            column: ComparisonTarget::Column {
                name: target_ndc_column.column.clone(),
                field_path: vec![],
            },
            operator: target_ndc_column.equal_operator.clone(),
            value: ComparisonValue::Variable {
                name: VariableName(target_value_variable),
            },
        };

        // collect these as we go
        relationship_join_filter_expressions.push(ResolvedFilterExpression::LocalFieldComparison(
            comparison_exp,
        ));
    }

    Ok(RemoteRelationshipParts {
        join_mapping,
        phantom_fields,
        relationship_join_filter_expressions,
    })
}

struct ProcessedRemoteRelationship {
    source_field_alias: SourceFieldAlias,
    field: Option<(NdcFieldAlias, Field)>,
}

/// Processes a remote relationship field mapping, and returns the alias used in
/// the NDC IR for that field
///
/// - if the selection set DOES NOT contain the field, insert it into the NDC IR
///   (with an internal alias), and return the alias
/// - if the selection set already contains the field, do not insert the field
///   in NDC IR, and return the existing alias
///
/// TODO: I don't think looking in selection_fields will ever find anything, we can probably
/// simplify this, let's do this once more tests are passing though
fn process_remote_relationship_field_mapping(
    selection_fields: &IndexMap<Alias, ObjectSubSelection>,
    ndc_column_name: &DataConnectorColumnName,
) -> ProcessedRemoteRelationship {
    let found = selection_fields
        .iter()
        .find_map(|(alias, field)| match field {
            ObjectSubSelection::Field(column) => {
                if column.target.field_name.as_str() == ndc_column_name.as_str() {
                    Some(alias.clone())
                } else {
                    None
                }
            }
            _ => None,
        });

    match found {
        None => {
            let internal_alias = make_hasura_phantom_field(ndc_column_name);
            ProcessedRemoteRelationship {
                source_field_alias: SourceFieldAlias(internal_alias.clone()),
                field: Some((
                    NdcFieldAlias::from(internal_alias.as_str()),
                    Field::Column {
                        column: ndc_column_name.clone(),
                        fields: None,
                        arguments: BTreeMap::new(),
                    },
                )),
            }
        }
        Some(field_alias) => ProcessedRemoteRelationship {
            source_field_alias: SourceFieldAlias(field_alias.as_str().to_owned()),
            field: None,
        },
    }
}

fn make_hasura_phantom_field(ndc_column_name: &DataConnectorColumnName) -> String {
    format!("__hasura_phantom_field__{}", ndc_column_name.as_str())
}
