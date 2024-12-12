//! NDC query generation from 'ModelSelection' IR for relationships.

use open_dds::data_connector::CollectionName;
use open_dds::relationships::RelationshipType;
use open_dds::types::DataConnectorArgumentName;
use std::collections::BTreeMap;

use super::error;
use crate::LocalCommandRelationshipInfo;
use crate::ModelSelection;
use plan::process_model_relationship_definition;
use plan_types::{NdcRelationshipName, Relationship, RelationshipArgument};

/// collect relationships from OrderBy IR component containing relationships.
pub(crate) fn collect_relationships_from_order_by(
    ir: &ModelSelection<'_>,
    relationships: &mut BTreeMap<NdcRelationshipName, Relationship>,
) -> Result<(), error::Error> {
    // from order by clause
    if let Some(order_by) = &ir.order_by {
        for (name, relationship) in &order_by.relationships {
            let result =
                process_model_relationship_definition(relationship).map_err(|plan_error| {
                    error::Error::Internal(error::InternalError::InternalGeneric {
                        description: plan_error.to_string(),
                    })
                })?;
            relationships.insert(name.clone(), result);
        }
    };
    Ok(())
}

pub(crate) fn process_command_relationship_definition(
    relationship_info: &LocalCommandRelationshipInfo,
) -> Result<Relationship, error::Error> {
    let &LocalCommandRelationshipInfo {
        annotation,
        source_data_connector: _,
        source_type_mappings,
        target_source,
    } = relationship_info;

    let mut arguments = BTreeMap::new();
    for metadata_resolve::RelationshipCommandMapping {
        source_field: source_field_path,
        argument_name: target_argument,
    } in &annotation.mappings
    {
        let source_column = metadata_resolve::get_field_mapping_of_field_name(
            source_type_mappings,
            &annotation.source_type,
            &annotation.relationship_name,
            &source_field_path.field_name,
        )
        .map_err(|e| error::InternalError::InternalGeneric {
            description: e.to_string(),
        })?;

        let relationship_argument = RelationshipArgument::Column {
            name: source_column.column,
        };

        let connector_argument_name = target_source
            .details
            .argument_mappings
            .get(target_argument)
            .ok_or_else(|| {
                error::Error::Internal(
                    error::InternalError::MissingArgumentMappingInCommandRelationship {
                        source_type: annotation.source_type.clone(),
                        relationship_name: annotation.relationship_name.clone(),
                        command_name: annotation.command_name.clone(),
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
            Err(error::InternalError::MappingExistsInRelationship {
                source_column: source_field_path.field_name.clone(),
                relationship_name: annotation.relationship_name.clone(),
            })?;
        }
    }

    let relationship = Relationship {
        column_mapping: BTreeMap::new(),
        relationship_type: RelationshipType::Object,
        target_collection: CollectionName::from(target_source.function_name.as_str()),
        arguments,
    };
    Ok(relationship)
}
