use super::commands;
use super::common;
use super::model_selection;
use super::relationships;
use crate::ir::selection_set::{FieldSelection, NestedSelection, ResultSelectionSet};
use crate::plan::error;
use crate::plan::ndc_request;
use crate::plan::ProcessResponseAs;
use crate::remote_joins::types::SourceFieldAlias;
use crate::remote_joins::types::{
    JoinLocations, JoinNode, LocationKind, MonotonicCounter, RemoteJoinType, TargetField,
};
use crate::remote_joins::types::{Location, RemoteJoin};
use indexmap::IndexMap;
use metadata_resolve::FieldMapping;
use open_dds::data_connector::DataConnectorColumnName;
use std::collections::{BTreeMap, HashMap};

pub(crate) fn process_nested_selection<'s, 'ir>(
    nested_selection: &'ir NestedSelection<'s>,
    join_id_counter: &mut MonotonicCounter,
) -> Result<(ndc_models::NestedField, JoinLocations<RemoteJoin<'s, 'ir>>), error::Error> {
    match nested_selection {
        NestedSelection::Object(model_selection) => {
            let (fields, join_locations) =
                process_selection_set_ir(model_selection, join_id_counter)?;
            Ok((
                ndc_models::NestedField::Object(ndc_models::NestedObject { fields }),
                join_locations,
            ))
        }
        NestedSelection::Array(nested_selection) => {
            let (field, join_locations) =
                process_nested_selection(nested_selection, join_id_counter)?;
            Ok((
                ndc_models::NestedField::Array(ndc_models::NestedArray {
                    fields: Box::new(field),
                }),
                join_locations,
            ))
        }
    }
}

/// Convert selection set IR ([ResultSelectionSet]) into NDC fields.
///
/// Also produce a [JoinLocations] tree along with NDC fields for all remote
/// relationship fields in this IR.
#[allow(irrefutable_let_patterns)]
pub(crate) fn process_selection_set_ir<'s, 'ir>(
    model_selection: &'ir ResultSelectionSet<'s>,
    join_id_counter: &mut MonotonicCounter,
) -> Result<
    (
        IndexMap<ndc_models::FieldName, ndc_models::Field>,
        JoinLocations<RemoteJoin<'s, 'ir>>,
    ),
    error::Error,
> {
    let mut ndc_fields = IndexMap::<ndc_models::FieldName, ndc_models::Field>::new();
    let mut join_locations = JoinLocations::new();
    for (alias, field) in &model_selection.fields {
        match field {
            FieldSelection::Column {
                column,
                nested_selection,
                arguments,
            } => {
                let (nested_field, nested_join_locations) = nested_selection
                    .as_ref()
                    .map(|nested_selection| {
                        process_nested_selection(nested_selection, join_id_counter)
                    })
                    .transpose()?
                    .unzip();
                ndc_fields.insert(
                    ndc_models::FieldName::from(alias.as_str()),
                    ndc_models::Field::Column {
                        column: ndc_models::FieldName::from(column.as_str()),
                        fields: nested_field,
                        arguments: common::ndc_arguments(arguments)?,
                    },
                );
                if let Some(jl) = nested_join_locations {
                    if !jl.locations.is_empty() {
                        join_locations.locations.insert(
                            alias.clone(),
                            Location {
                                join_node: JoinNode::Local(LocationKind::NestedData),
                                rest: jl,
                            },
                        );
                    }
                }
            }
            FieldSelection::ModelRelationshipLocal {
                query,
                name,
                relationship_info: _,
            } => {
                let (relationship_query, jl) = model_selection::ndc_query(query, join_id_counter)?;
                let ndc_field = ndc_models::Field::Relationship {
                    query: Box::new(relationship_query),
                    relationship: ndc_models::RelationshipName::from(name.0.as_str()),
                    arguments: BTreeMap::new(),
                };
                if !jl.locations.is_empty() {
                    join_locations.locations.insert(
                        alias.clone(),
                        Location {
                            join_node: JoinNode::Local(LocationKind::LocalRelationship),
                            rest: jl,
                        },
                    );
                }
                ndc_fields.insert(ndc_models::FieldName::from(alias.as_str()), ndc_field);
            }
            FieldSelection::CommandRelationshipLocal {
                ir,
                name,
                relationship_info: _,
            } => {
                let (relationship_query, jl) =
                    commands::ndc_query(&ir.command_info, join_id_counter)?;

                let relationship_arguments: BTreeMap<_, _> =
                    common::ndc_relationship_arguments(&ir.command_info.arguments)?;

                let ndc_field = ndc_models::Field::Relationship {
                    query: Box::new(relationship_query),
                    relationship: ndc_models::RelationshipName::from(name.as_str()),
                    arguments: relationship_arguments,
                };

                if !jl.locations.is_empty() {
                    join_locations.locations.insert(
                        alias.clone(),
                        Location {
                            join_node: JoinNode::Local(LocationKind::LocalRelationship),
                            rest: jl,
                        },
                    );
                }
                ndc_fields.insert(ndc_models::FieldName::from(alias.as_str()), ndc_field);
            }
            FieldSelection::ModelRelationshipRemote {
                ir,
                relationship_info,
            } => {
                let mut join_mapping = HashMap::new();
                for ((src_field_alias, src_field), target_field) in &relationship_info.join_mapping
                {
                    let ndc_field_alias = process_remote_relationship_field_mapping(
                        model_selection,
                        src_field,
                        &mut ndc_fields,
                    );
                    join_mapping.insert(
                        src_field_alias.clone(),
                        (
                            ndc_field_alias,
                            TargetField::ModelField(target_field.clone()),
                        ),
                    );
                }
                // Construct the `JoinLocations` tree
                let (ndc_ir, sub_join_locations) =
                    ndc_request::make_ndc_model_query_request(ir, join_id_counter)?;
                let rj_info = RemoteJoin {
                    target_ndc_ir: ndc_ir,
                    target_data_connector: ir.data_connector,
                    join_mapping,
                    process_response_as: ProcessResponseAs::Array { is_nullable: true },
                    remote_join_type: RemoteJoinType::ToModel,
                };
                join_locations.locations.insert(
                    alias.clone(),
                    Location {
                        join_node: JoinNode::Remote(rj_info),
                        rest: sub_join_locations,
                    },
                );
            }
            FieldSelection::CommandRelationshipRemote {
                ir,
                relationship_info,
            } => {
                let mut join_mapping = HashMap::new();

                for ((src_field_alias, src_field), target_field) in &relationship_info.join_mapping
                {
                    let ndc_field_alias = process_remote_relationship_field_mapping(
                        model_selection,
                        src_field,
                        &mut ndc_fields,
                    );
                    join_mapping.insert(
                        src_field_alias.clone(),
                        (
                            ndc_field_alias,
                            TargetField::CommandField(target_field.clone()),
                        ),
                    );
                }
                // Construct the `JoinLocations` tree
                let (ndc_ir, sub_join_locations) =
                    ndc_request::make_ndc_function_query_request(ir, join_id_counter)?;
                let rj_info = RemoteJoin {
                    target_ndc_ir: ndc_ir,
                    target_data_connector: ir.command_info.data_connector,
                    join_mapping,
                    process_response_as: ProcessResponseAs::CommandResponse {
                        command_name: &ir.command_info.command_name,
                        type_container: &ir.command_info.type_container,
                        response_config: &ir.command_info.data_connector.response_config,
                    },
                    remote_join_type: RemoteJoinType::ToCommand,
                };
                join_locations.locations.insert(
                    alias.clone(),
                    Location {
                        join_node: JoinNode::Remote(rj_info),
                        rest: sub_join_locations,
                    },
                );
            }
        };
    }
    Ok((ndc_fields, join_locations))
}

/// Processes a remote relationship field mapping, and returns the alias used in
/// the NDC IR for that field
///
/// - if the selection set DOES NOT contain the field, insert it into the NDC IR
/// (with an internal alias), and return the alias
/// - if the selection set already contains the field, do not insert the field
/// in NDC IR, and return the existing alias
fn process_remote_relationship_field_mapping(
    selection: &ResultSelectionSet<'_>,
    field: &FieldMapping,
    ndc_fields: &mut IndexMap<ndc_models::FieldName, ndc_models::Field>,
) -> SourceFieldAlias {
    match selection.contains(field) {
        None => {
            let internal_alias = make_hasura_phantom_field(&field.column);
            ndc_fields.insert(
                ndc_models::FieldName::from(internal_alias.as_str()),
                ndc_models::Field::Column {
                    column: ndc_models::FieldName::from(field.column.as_str()),
                    fields: None,
                    arguments: BTreeMap::new(),
                },
            );
            SourceFieldAlias(internal_alias)
        }
        Some(field_alias) => SourceFieldAlias(field_alias),
    }
}

fn make_hasura_phantom_field(field_name: &DataConnectorColumnName) -> String {
    format!("__hasura_phantom_field__{}", field_name.as_str())
}

pub(crate) fn collect_relationships_from_nested_selection(
    selection: &NestedSelection,
    relationships: &mut BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
) -> Result<(), error::Error> {
    match selection {
        NestedSelection::Object(selection_set) => {
            collect_relationships_from_selection(selection_set, relationships)
        }
        NestedSelection::Array(nested_selection) => {
            collect_relationships_from_nested_selection(nested_selection, relationships)
        }
    }
}

/// From the fields in `ResultSelectionSet`, collect relationships recursively
/// and create NDC relationship definitions
pub(crate) fn collect_relationships_from_selection(
    selection: &ResultSelectionSet,
    relationships: &mut BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
) -> Result<(), error::Error> {
    for field in selection.fields.values() {
        match field {
            FieldSelection::Column {
                nested_selection, ..
            } => {
                if let Some(nested_selection) = nested_selection {
                    collect_relationships_from_nested_selection(nested_selection, relationships)?;
                }
            }
            FieldSelection::ModelRelationshipLocal {
                query,
                name,
                relationship_info,
            } => {
                relationships.insert(
                    ndc_models::RelationshipName::from(name.0.as_str()),
                    relationships::process_model_relationship_definition(relationship_info)?,
                );
                relationships::collect_relationships(query, relationships)?;
            }
            FieldSelection::CommandRelationshipLocal {
                ir,
                name,
                relationship_info,
            } => {
                relationships.insert(
                    ndc_models::RelationshipName::from(name.0.as_str()),
                    relationships::process_command_relationship_definition(relationship_info)?,
                );
                if let Some(nested_selection) = &ir.command_info.selection {
                    collect_relationships_from_nested_selection(nested_selection, relationships)?;
                }
            }
            // we ignore remote relationships as we are generating relationship
            // definition for one data connector
            FieldSelection::ModelRelationshipRemote { .. }
            | FieldSelection::CommandRelationshipRemote { .. } => (),
        };
    }
    Ok(())
}
