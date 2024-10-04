//! NDC query generation from 'ModelSelection' IR for relationships.

use open_dds::data_connector::{CollectionName, DataConnectorColumnName};
use open_dds::relationships::RelationshipType;
use open_dds::types::DataConnectorArgumentName;
use std::collections::BTreeMap;

use crate::plan::error;
use graphql_ir::LocalCommandRelationshipInfo;
use graphql_ir::ModelSelection;
use plan_types::{LocalModelRelationshipInfo, NdcRelationshipName};

#[derive(Debug, Clone, PartialEq)]
pub struct Relationship {
    /// A mapping between columns on the source collection to columns on the target collection
    pub column_mapping: BTreeMap<DataConnectorColumnName, DataConnectorColumnName>,
    pub relationship_type: RelationshipType,
    /// The name of a collection
    pub target_collection: CollectionName,
    /// Values to be provided to any collection arguments
    pub arguments: BTreeMap<DataConnectorArgumentName, RelationshipArgument>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RelationshipArgument {
    Column { name: DataConnectorColumnName },
}

/// collect relationships from OrderBy IR component containing relationships.
pub(crate) fn collect_relationships_from_order_by(
    ir: &ModelSelection<'_>,
    relationships: &mut BTreeMap<NdcRelationshipName, Relationship>,
) -> Result<(), error::Error> {
    // from order by clause
    if let Some(order_by) = &ir.order_by {
        for (name, relationship) in &order_by.relationships {
            let result = process_model_relationship_definition(relationship)?;
            relationships.insert(name.clone(), result);
        }
    };
    Ok(())
}

pub fn process_model_relationship_definition(
    relationship_info: &LocalModelRelationshipInfo,
) -> Result<Relationship, error::Error> {
    let &LocalModelRelationshipInfo {
        relationship_name,
        relationship_type,
        source_type,
        source_data_connector,
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
        if matches!(
            metadata_resolve::relationship_execution_category(
                source_data_connector,
                &target_source.model.data_connector,
                &target_source.capabilities
            ),
            metadata_resolve::RelationshipExecutionCategory::Local
        ) {
            let target_column = target_ndc_column.as_ref().ok_or_else(|| {
                error::InternalError::InternalGeneric {
                    description: format!(
                        "No column mapping for relationship {relationship_name} on {source_type}"
                    ),
                }
            })?;

            let source_column = graphql_ir::get_field_mapping_of_field_name(
                source_type_mappings,
                source_type,
                relationship_name,
                &source_field_path.field_name,
            )
            .map_err(|e| error::InternalError::InternalGeneric {
                description: e.to_string(),
            })?;

            if column_mapping
                .insert(source_column.column, target_column.column.clone())
                .is_some()
            {
                Err(error::InternalError::MappingExistsInRelationship {
                    source_column: source_field_path.field_name.clone(),
                    relationship_name: relationship_name.clone(),
                })?;
            }
        } else {
            Err(error::InternalError::RemoteRelationshipsAreNotSupported)?;
        }
    }
    let relationship = Relationship {
        column_mapping,
        relationship_type: relationship_type.clone(),
        target_collection: target_source.model.collection.clone(),
        arguments: BTreeMap::new(),
    };
    Ok(relationship)
}

pub(crate) fn process_command_relationship_definition(
    relationship_info: &LocalCommandRelationshipInfo,
) -> Result<Relationship, error::Error> {
    let &LocalCommandRelationshipInfo {
        annotation,
        source_data_connector,
        source_type_mappings,
        target_source,
    } = relationship_info;

    let mut arguments = BTreeMap::new();
    for metadata_resolve::RelationshipCommandMapping {
        source_field: source_field_path,
        argument_name: target_argument,
    } in &annotation.mappings
    {
        if matches!(
            metadata_resolve::relationship_execution_category(
                source_data_connector,
                &target_source.details.data_connector,
                &target_source.capabilities
            ),
            metadata_resolve::RelationshipExecutionCategory::Local
        ) {
            let source_column = graphql_ir::get_field_mapping_of_field_name(
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
        } else {
            Err(error::InternalError::RemoteRelationshipsAreNotSupported)?;
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
