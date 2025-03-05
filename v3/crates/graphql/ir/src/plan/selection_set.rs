use super::arguments;
use super::commands;
use super::model_selection;
use super::types::Plan;
use super::ProcessResponseAs;
use crate::plan::error;
use crate::{FieldSelection, NestedSelection, ResultSelectionSet};
use hasura_authn_core::Session;
use indexmap::IndexMap;
use metadata_resolve::data_connectors::NdcVersion;
use metadata_resolve::{FieldMapping, Metadata};
use open_dds::data_connector::DataConnectorColumnName;
use plan::{process_command_relationship_definition, process_model_relationship_definition};
use plan_types::{
    CommandReturnKind, Field, JoinLocations, JoinNode, Location, LocationKind, NestedArray,
    NestedField, NestedObject, PredicateQueryTrees, QueryExecutionTree, RemoteJoin, RemoteJoinType,
    SourceFieldAlias, TargetField,
};
use plan_types::{NdcFieldAlias, NdcRelationshipName, Relationship, UniqueNumber};
use std::collections::BTreeMap;

pub(crate) fn plan_nested_selection(
    nested_selection: &NestedSelection<'_>,
    ndc_version: NdcVersion,
    relationships: &mut BTreeMap<NdcRelationshipName, Relationship>,
    metadata: &'_ Metadata,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    unique_number: &mut UniqueNumber,
) -> Result<Plan<NestedField>, error::Error> {
    match nested_selection {
        NestedSelection::Object(model_selection) => {
            let Plan {
                inner: fields,
                join_locations,
                remote_predicates,
            } = plan_selection_set(
                model_selection,
                ndc_version,
                relationships,
                metadata,
                session,
                request_headers,
                unique_number,
            )?;
            Ok(Plan {
                inner: NestedField::Object(NestedObject { fields }),
                join_locations,
                remote_predicates,
            })
        }
        NestedSelection::Array(nested_selection) => {
            let Plan {
                inner: field,
                join_locations,
                remote_predicates,
            } = plan_nested_selection(
                nested_selection,
                ndc_version,
                relationships,
                metadata,
                session,
                request_headers,
                unique_number,
            )?;
            Ok(Plan {
                inner: NestedField::Array(NestedArray {
                    fields: Box::new(field),
                }),
                join_locations,
                remote_predicates,
            })
        }
    }
}

/// Convert selection set IR ([ResultSelectionSet]) into NDC fields.
///
/// Also produce a [JoinLocations] tree along with NDC fields for all remote
/// relationship fields in this IR.
#[allow(irrefutable_let_patterns)]
pub(crate) fn plan_selection_set(
    model_selection: &ResultSelectionSet<'_>,
    ndc_version: NdcVersion,
    relationships: &mut BTreeMap<NdcRelationshipName, Relationship>,
    metadata: &'_ Metadata,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    unique_number: &mut UniqueNumber,
) -> Result<Plan<IndexMap<NdcFieldAlias, Field>>, error::Error> {
    let mut fields = IndexMap::<NdcFieldAlias, Field>::new();
    let mut join_locations = JoinLocations::new();
    let mut remote_predicates = PredicateQueryTrees::new();
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
                    let Plan {
                        inner: nested_fields,
                        join_locations: jl,
                        remote_predicates: nested_remote_predicates,
                    } = plan_nested_selection(
                        nested_selection,
                        ndc_version,
                        relationships,
                        metadata,
                        session,
                        request_headers,
                        unique_number,
                    )?;

                    remote_predicates.0.extend(nested_remote_predicates.0);

                    nested_field = Some(nested_fields);
                    nested_join_locations = jl;
                }
                let (arguments, argument_remote_predicates) =
                    arguments::plan_arguments(arguments, relationships, unique_number)?;

                remote_predicates.0.extend(argument_remote_predicates.0);

                fields.insert(
                    field_name.clone(),
                    Field::Column {
                        column: column.clone(),
                        fields: nested_field,
                        arguments,
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
                    process_model_relationship_definition(relationship_info).map_err(
                        |plan_error| {
                            error::Error::Internal(error::InternalError::InternalGeneric {
                                description: plan_error.to_string(),
                            })
                        },
                    )?,
                );
                let Plan {
                    inner: relationship_query,
                    join_locations: jl,
                    remote_predicates: relationship_remote_predicates,
                } = model_selection::plan_query_node(
                    query,
                    relationships,
                    metadata,
                    session,
                    request_headers,
                    unique_number,
                )?;

                remote_predicates.0.extend(relationship_remote_predicates.0);

                let ndc_field = Field::Relationship {
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
                    process_command_relationship_definition(relationship_info).map_err(
                        |plan_error| {
                            error::Error::Internal(error::InternalError::InternalGeneric {
                                description: plan_error.to_string(),
                            })
                        },
                    )?,
                );
                let Plan {
                    inner: relationship_query,
                    join_locations: jl,
                    remote_predicates: command_remote_predicates,
                } = commands::plan_query_node(
                    &ir.command_info,
                    relationships,
                    metadata,
                    session,
                    request_headers,
                    unique_number,
                )?;

                let arguments = match &ir.command_info.selection {
                    crate::CommandSelection::Ir { arguments, .. } => Ok(arguments),
                    crate::CommandSelection::OpenDd { .. } => Err(error::Error::Internal(
                        error::InternalError::InternalGeneric {
                            description: "OpenDD IR found when planning regular IR query".into(),
                        },
                    )),
                }?;

                remote_predicates.0.extend(command_remote_predicates.0);

                let (relationship_arguments, argument_remote_predicates) =
                    arguments::plan_arguments(arguments, relationships, unique_number)?;

                remote_predicates.0.extend(argument_remote_predicates.0);

                let ndc_field = Field::Relationship {
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
                let mut join_mapping = BTreeMap::new();
                for ((src_field_alias, src_field), target_field) in &relationship_info.join_mapping
                {
                    let ndc_field_alias = process_remote_relationship_field_mapping(
                        model_selection,
                        src_field,
                        &mut fields,
                    );
                    join_mapping.insert(
                        src_field_alias.clone(),
                        (ndc_field_alias, target_field.clone()),
                    );
                }
                // Construct the `JoinLocations` tree
                let QueryExecutionTree {
                    query_execution_plan: query_execution,
                    remote_join_executions: sub_join_locations,
                    remote_predicates: model_remote_predicates,
                } = model_selection::plan_query_execution(
                    ir,
                    metadata,
                    session,
                    request_headers,
                    unique_number,
                )?;

                // push any remote predicates to the outer list
                remote_predicates.0.extend(model_remote_predicates.0);

                let rj_info = RemoteJoin {
                    target_ndc_execution: query_execution,
                    target_data_connector: ir.data_connector.clone(),
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
                let mut join_mapping = BTreeMap::new();

                for ((src_field_alias, src_field), target_field) in &relationship_info.join_mapping
                {
                    let ndc_field_alias = process_remote_relationship_field_mapping(
                        model_selection,
                        src_field,
                        &mut fields,
                    );
                    join_mapping.insert(
                        src_field_alias.clone(),
                        (ndc_field_alias, TargetField::Argument(target_field.clone())),
                    );
                }
                // Construct the `JoinLocations` tree
                let QueryExecutionTree {
                    query_execution_plan: ndc_ir,
                    remote_join_executions: sub_join_locations,
                    remote_predicates: command_remote_predicates,
                } = commands::plan_query_execution(
                    ir,
                    metadata,
                    session,
                    request_headers,
                    unique_number,
                )?;

                // we push remote predicates to the outer list
                remote_predicates.0.extend(command_remote_predicates.0);

                let rj_info = RemoteJoin {
                    target_ndc_execution: ndc_ir,
                    target_data_connector: ir.command_info.data_connector.clone(),
                    join_mapping,
                    process_response_as: ProcessResponseAs::CommandResponse {
                        command_name: ir.command_info.command_name.clone(),
                        is_nullable: ir.command_info.type_container.nullable,
                        return_kind: if ir.command_info.type_container.is_list() {
                            CommandReturnKind::Array
                        } else {
                            CommandReturnKind::Object
                        },
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
    Ok(Plan {
        inner: fields,
        join_locations,
        remote_predicates,
    })
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
    fields: &mut IndexMap<NdcFieldAlias, Field>,
) -> SourceFieldAlias {
    match selection.contains(field_mapping) {
        None => {
            let internal_alias = make_hasura_phantom_field(&field_mapping.column);
            fields.insert(
                NdcFieldAlias::from(internal_alias.as_str()),
                Field::Column {
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
