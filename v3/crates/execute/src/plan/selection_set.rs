use super::arguments;
use super::commands;
use super::field;
use super::model_selection;
use super::relationships;
use super::ProcessResponseAs;
use crate::plan::error;
use crate::remote_joins::types::SourceFieldAlias;
use crate::remote_joins::types::{
    JoinLocations, JoinNode, LocationKind, RemoteJoinType, TargetField,
};
use crate::remote_joins::types::{Location, RemoteJoin};
use graphql_ir::NdcRelationshipName;
use graphql_ir::{FieldSelection, NestedSelection, ResultSelectionSet};
use indexmap::IndexMap;
use metadata_resolve::data_connectors::NdcVersion;
use metadata_resolve::FieldMapping;
use open_dds::data_connector::DataConnectorColumnName;
use plan_types::NdcFieldAlias;
use std::collections::{BTreeMap, HashMap};

pub(crate) fn plan_nested_selection<'s>(
    nested_selection: &NestedSelection<'s>,
    ndc_version: NdcVersion,
    relationships: &mut BTreeMap<NdcRelationshipName, relationships::Relationship>,
) -> Result<(field::UnresolvedNestedField<'s>, JoinLocations<'s>), error::Error> {
    match nested_selection {
        NestedSelection::Object(model_selection) => {
            let (fields, join_locations) =
                plan_selection_set(model_selection, ndc_version, relationships)?;
            Ok((
                field::NestedField::Object(field::NestedObject { fields }),
                join_locations,
            ))
        }
        NestedSelection::Array(nested_selection) => {
            let (field, join_locations) =
                plan_nested_selection(nested_selection, ndc_version, relationships)?;
            Ok((
                field::NestedField::Array(field::NestedArray {
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
pub(crate) fn plan_selection_set<'s, 'ir>(
    model_selection: &'ir ResultSelectionSet<'s>,
    ndc_version: NdcVersion,
    relationships: &mut BTreeMap<NdcRelationshipName, relationships::Relationship>,
) -> Result<
    (
        IndexMap<NdcFieldAlias, field::UnresolvedField<'s>>,
        JoinLocations<'s>,
    ),
    error::Error,
> {
    let mut fields = IndexMap::<NdcFieldAlias, field::UnresolvedField<'s>>::new();
    let mut join_locations = JoinLocations::new();
    for (field_name, field) in &model_selection.fields {
        match field {
            FieldSelection::Column {
                column,
                nested_selection,
                arguments,
            } => {
                let mut nested_field = None;
                let mut nested_join_locations = JoinLocations::new();
                if let Some(nested_selection) = nested_selection {
                    let (nested_fields, jl) =
                        plan_nested_selection(nested_selection, ndc_version, relationships)?;
                    nested_field = Some(nested_fields);
                    nested_join_locations = jl;
                }
                fields.insert(
                    field_name.clone(),
                    field::Field::Column {
                        column: column.clone(),
                        fields: nested_field,
                        arguments: arguments::plan_arguments(arguments, relationships)?,
                    },
                );
                if !nested_join_locations.locations.is_empty() {
                    join_locations.locations.insert(
                        field_name.as_str().to_owned(),
                        Location {
                            join_node: JoinNode::Local(LocationKind::NestedData),
                            rest: nested_join_locations,
                        },
                    );
                }
            }
            FieldSelection::ModelRelationshipLocal {
                query,
                name,
                relationship_info,
            } => {
                // collect local model relationship
                relationships.insert(
                    name.clone(),
                    relationships::process_model_relationship_definition(relationship_info)?,
                );
                let (relationship_query, jl) =
                    model_selection::plan_query_node(query, relationships)?;
                let ndc_field = field::Field::Relationship {
                    query_node: Box::new(relationship_query),
                    relationship: name.clone(),
                    arguments: BTreeMap::new(),
                };
                if !jl.locations.is_empty() {
                    join_locations.locations.insert(
                        field_name.as_str().to_owned(),
                        Location {
                            join_node: JoinNode::Local(LocationKind::LocalRelationship),
                            rest: jl,
                        },
                    );
                }
                fields.insert(field_name.clone(), ndc_field);
            }
            FieldSelection::CommandRelationshipLocal {
                ir,
                name,
                relationship_info,
            } => {
                // collect local command relationship
                relationships.insert(
                    name.clone(),
                    relationships::process_command_relationship_definition(relationship_info)?,
                );
                let (relationship_query, jl) =
                    commands::plan_query_node(&ir.command_info, relationships)?;

                let relationship_arguments: BTreeMap<_, _> =
                    arguments::plan_arguments(&ir.command_info.arguments, relationships)?;

                let ndc_field = field::Field::Relationship {
                    query_node: Box::new(relationship_query),
                    relationship: name.clone(),
                    arguments: relationship_arguments,
                };

                if !jl.locations.is_empty() {
                    join_locations.locations.insert(
                        field_name.as_str().to_owned(),
                        Location {
                            join_node: JoinNode::Local(LocationKind::LocalRelationship),
                            rest: jl,
                        },
                    );
                }
                fields.insert(field_name.clone(), ndc_field);
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
                        &mut fields,
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
                let (query_execution, sub_join_locations) =
                    model_selection::plan_query_execution(ir)?;
                let rj_info = RemoteJoin {
                    target_ndc_execution: query_execution,
                    target_data_connector: ir.data_connector,
                    join_mapping,
                    process_response_as: ProcessResponseAs::Array { is_nullable: true },
                    remote_join_type: RemoteJoinType::ToModel,
                };
                join_locations.locations.insert(
                    field_name.as_str().to_owned(),
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
                        &mut fields,
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
                let (ndc_ir, sub_join_locations) = commands::plan_query_execution(ir)?;
                let rj_info = RemoteJoin {
                    target_ndc_execution: ndc_ir,
                    target_data_connector: ir.command_info.data_connector,
                    join_mapping,
                    process_response_as: ProcessResponseAs::CommandResponse {
                        command_name: ir.command_info.command_name.clone(),
                        type_container: ir.command_info.type_container.clone(),
                        response_config: ir.command_info.data_connector.response_config.clone(),
                    },
                    remote_join_type: RemoteJoinType::ToCommand,
                };
                join_locations.locations.insert(
                    field_name.as_str().to_owned(),
                    Location {
                        join_node: JoinNode::Remote(rj_info),
                        rest: sub_join_locations,
                    },
                );
            }
        };
    }
    Ok((fields, join_locations))
}

/// Processes a remote relationship field mapping, and returns the alias used in
/// the NDC IR for that field
///
/// - if the selection set DOES NOT contain the field, insert it into the NDC IR
///   (with an internal alias), and return the alias
/// - if the selection set already contains the field, do not insert the field
///   in NDC IR, and return the existing alias
fn process_remote_relationship_field_mapping(
    selection: &ResultSelectionSet<'_>,
    field_mapping: &FieldMapping,
    fields: &mut IndexMap<NdcFieldAlias, field::UnresolvedField>,
) -> SourceFieldAlias {
    match selection.contains(field_mapping) {
        None => {
            let internal_alias = make_hasura_phantom_field(&field_mapping.column);
            fields.insert(
                NdcFieldAlias::from(internal_alias.as_str()),
                field::Field::Column {
                    column: field_mapping.column.clone(),
                    fields: None,
                    arguments: BTreeMap::new(),
                },
            );
            SourceFieldAlias(internal_alias)
        }
        Some(field_alias) => SourceFieldAlias(field_alias.as_str().to_owned()),
    }
}

fn make_hasura_phantom_field(field_name: &DataConnectorColumnName) -> String {
    format!("__hasura_phantom_field__{}", field_name.as_str())
}
