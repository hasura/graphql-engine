use indexmap::IndexMap;
use ndc_client as ndc;
use std::collections::{BTreeMap, HashMap};

use super::commands;
use super::model_selection;
use super::relationships;
use super::ProcessResponseAs;
use crate::execute::error;
use crate::execute::ir::relationship;
use crate::execute::ir::selection_set::{FieldSelection, NestedSelection, ResultSelectionSet};
use crate::execute::remote_joins::types::{
    JoinLocations, JoinNode, LocationKind, MonotonicCounter, RemoteJoinType, TargetField,
};
use crate::execute::remote_joins::types::{Location, RemoteJoin};

fn process_nested_selection<'s, 'ir>(
    nested_selection: &'ir NestedSelection<'s>,
    join_id_counter: &mut MonotonicCounter,
) -> Result<
    (
        ndc::models::NestedField,
        Option<JoinLocations<RemoteJoin<'s, 'ir>>>,
    ),
    error::Error,
> {
    match nested_selection {
        NestedSelection::Object(model_selection) => {
            let (fields, join_locations) =
                process_selection_set_ir(model_selection, join_id_counter)?;
            Ok((
                ndc::models::NestedField::Object(ndc::models::NestedObject { fields }),
                Some(join_locations),
            ))
        }
        NestedSelection::Array(nested_selection) => {
            let (field, join_locations) =
                process_nested_selection(nested_selection, join_id_counter)?;
            Ok((
                ndc::models::NestedField::Array(ndc::models::NestedArray {
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
        IndexMap<String, ndc::models::Field>,
        JoinLocations<RemoteJoin<'s, 'ir>>,
    ),
    error::Error,
> {
    let mut ndc_fields = IndexMap::new();
    let mut join_locations = JoinLocations::new();
    for (alias, field) in &model_selection.fields {
        match field {
            FieldSelection::Column {
                column,
                nested_selection,
            } => {
                let (nested_field, nested_join_locations) = nested_selection
                    .as_ref()
                    .map(|nested_selection| {
                        process_nested_selection(nested_selection, join_id_counter)
                    })
                    .transpose()?
                    .unzip();
                ndc_fields.insert(
                    alias.to_string(),
                    ndc::models::Field::Column {
                        column: column.clone(),
                        fields: nested_field,
                    },
                );
                if let Some(Some(jl)) = nested_join_locations {
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
                let ndc_field = ndc::models::Field::Relationship {
                    query: Box::new(relationship_query),
                    relationship: name.to_string(),
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
                ndc_fields.insert(alias.to_string(), ndc_field);
            }
            FieldSelection::CommandRelationshipLocal {
                ir,
                name,
                relationship_info: _,
            } => {
                let (relationship_query, jl) =
                    commands::ndc_query(&ir.command_info, join_id_counter)?;

                let relationship_arguments: BTreeMap<_, _> = ir
                    .command_info
                    .arguments
                    .iter()
                    .map(|(argument_name, argument_value)| {
                        (
                            argument_name.clone(),
                            ndc::models::RelationshipArgument::Literal {
                                value: argument_value.clone(),
                            },
                        )
                    })
                    .collect();

                let ndc_field = ndc::models::Field::Relationship {
                    query: Box::new(relationship_query),
                    relationship: name.to_string(),
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
                ndc_fields.insert(alias.to_string(), ndc_field);
            }
            FieldSelection::ModelRelationshipRemote {
                ir,
                relationship_info,
            } => {
                // For all the left join fields, create an alias and inject
                // them into the NDC IR
                let mut join_mapping = HashMap::new();
                for ((src_field_alias, src_field), target_field) in &relationship_info.join_mapping
                {
                    let lhs_alias = make_hasura_phantom_field(&src_field.column);

                    ndc_fields.insert(
                        lhs_alias.clone(),
                        ndc::models::Field::Column {
                            column: src_field.column.clone(),
                            fields: None,
                        },
                    );
                    join_mapping.insert(
                        src_field_alias.clone(),
                        (
                            lhs_alias.clone(),
                            TargetField::ModelField(target_field.clone()),
                        ),
                    );
                }
                // Construct the `JoinLocations` tree
                let (ndc_ir, sub_join_locations) = model_selection::ndc_ir(ir, join_id_counter)?;
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
                // For all the left join fields, create an alias and inject
                // them into the NDC IR
                let mut join_mapping = HashMap::new();
                for ((src_field_alias, src_field), target_field) in &relationship_info.join_mapping
                {
                    let lhs_alias = make_hasura_phantom_field(&src_field.column);

                    ndc_fields.insert(
                        lhs_alias.clone(),
                        ndc::models::Field::Column {
                            column: src_field.column.clone(),
                            fields: None,
                        },
                    );
                    join_mapping.insert(
                        src_field_alias.clone(),
                        (
                            lhs_alias.clone(),
                            TargetField::CommandField(target_field.clone()),
                        ),
                    );
                }
                // Construct the `JoinLocations` tree
                let (ndc_ir, sub_join_locations) = commands::ndc_query_ir(ir, join_id_counter)?;
                let rj_info = RemoteJoin {
                    target_ndc_ir: ndc_ir,
                    target_data_connector: ir.command_info.data_connector,
                    join_mapping,
                    process_response_as: ProcessResponseAs::CommandResponse {
                        command_name: &ir.command_info.command_name,
                        type_container: &ir.command_info.type_container,
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

fn make_hasura_phantom_field(field_name: &str) -> String {
    format!("__hasura_phantom_field__{}", field_name)
}

/// From the fields in `ResultSelectionSet`, collect relationships recursively
/// and create NDC relationship definitions
pub(crate) fn collect_relationships_from_selection(
    selection: &ResultSelectionSet,
    relationships: &mut BTreeMap<String, ndc::models::Relationship>,
) -> Result<(), error::Error> {
    for field in selection.fields.values() {
        match field {
            FieldSelection::Column { .. } => (),
            FieldSelection::ModelRelationshipLocal {
                query,
                name,
                relationship_info,
            } => {
                relationships.insert(
                    name.to_string(),
                    relationship::process_model_relationship_definition(relationship_info)?,
                );
                relationships::collect_relationships(query, relationships)?;
            }
            FieldSelection::CommandRelationshipLocal {
                ir,
                name,
                relationship_info,
            } => {
                relationships.insert(
                    name.to_string(),
                    relationship::process_command_relationship_definition(relationship_info)?,
                );
                collect_relationships_from_selection(&ir.command_info.selection, relationships)?;
            }
            // we ignore remote relationships as we are generating relationship
            // definition for one data connector
            FieldSelection::ModelRelationshipRemote { .. } => (),
            FieldSelection::CommandRelationshipRemote { .. } => (),
        };
    }
    Ok(())
}
