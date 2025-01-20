use crate::types::{PlanError, RelationshipError};
use open_dds::{
    data_connector::CollectionName, relationships::RelationshipType,
    types::DataConnectorArgumentName,
};
use plan_types::{LocalCommandRelationshipInfo, LocalModelRelationshipInfo, Relationship};
use std::collections::BTreeMap;

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
