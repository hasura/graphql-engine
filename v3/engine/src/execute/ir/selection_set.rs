use hasura_authn_core::SessionVariables;
use indexmap::IndexMap;
use lang_graphql::ast::common::Alias;
use lang_graphql::normalized_ast;
use ndc_client as ndc;
use open_dds::types::{CustomTypeName, FieldName};
use serde::Serialize;
use std::collections::{BTreeMap, HashMap};

use super::commands::{self, ir_to_ndc_query_ir, FunctionBasedCommand};
use super::model_selection::{self, ModelSelection};
use super::relationship::{
    self, LocalCommandRelationshipInfo, LocalModelRelationshipInfo, RemoteCommandRelationshipInfo,
    RemoteModelRelationshipInfo,
};
use crate::execute::error;
use crate::execute::global_id;
use crate::execute::model_tracking::UsagesCounts;
use crate::execute::remote_joins::types::{
    JoinLocations, MonotonicCounter, RemoteJoinType, ResponseType, TargetField,
};
use crate::execute::remote_joins::types::{Location, RemoteJoin};
use crate::metadata::resolved;
use crate::metadata::resolved::subgraph::Qualified;
use crate::schema::{
    types::{Annotation, OutputAnnotation, RootFieldAnnotation},
    GDS,
};

#[derive(Debug, Serialize)]
pub(crate) enum FieldSelection<'s> {
    Column {
        column: String,
    },
    ModelRelationshipLocal {
        query: ModelSelection<'s>,
        /// Relationship names needs to be unique across the IR. This field contains
        /// the uniquely generated relationship name. `ModelRelationshipAnnotation`
        /// contains a relationship name but that is the name from the metadata.
        name: String,
        relationship_info: LocalModelRelationshipInfo<'s>,
    },
    CommandRelationshipLocal {
        ir: FunctionBasedCommand<'s>,
        name: String,
        relationship_info: LocalCommandRelationshipInfo<'s>,
    },
    ModelRelationshipRemote {
        ir: ModelSelection<'s>,
        relationship_info: RemoteModelRelationshipInfo<'s>,
    },
    CommandRelationshipRemote {
        ir: FunctionBasedCommand<'s>,
        relationship_info: RemoteCommandRelationshipInfo<'s>,
    },
}

/// IR that represents the selected fields of an output type.
#[derive(Debug, Serialize)]
pub(crate) struct ResultSelectionSet<'s> {
    // The fields in the selection set. They are stored in the form that would
    // be converted and sent over the wire. Serialized the map as ordered to
    // produce deterministic golden files.
    pub(crate) fields: IndexMap<String, FieldSelection<'s>>,
}

fn build_global_id_fields(
    global_id_fields: &Vec<FieldName>,
    field_mappings: &BTreeMap<FieldName, resolved::types::FieldMapping>,
    field_alias: &Alias,
    fields: &mut IndexMap<String, FieldSelection>,
) -> Result<(), error::Error> {
    for field_name in global_id_fields {
        let field_mapping = field_mappings.get(field_name).ok_or_else(|| {
            error::InternalEngineError::InternalGeneric {
                description: format!("invalid global id field in annotation: {field_name:}"),
            }
        })?;
        // Prefix the global column id with something that will be unlikely to be chosen
        // by the user,
        //  to not have any conflicts with any of the fields
        // in the selection set.
        let global_col_id_alias = global_id::global_id_col_format(field_alias, field_name);

        fields.insert(
            global_col_id_alias,
            FieldSelection::Column {
                column: field_mapping.column.clone(),
            },
        );
    }
    Ok(())
}

/// Builds the IR from a normalized selection set
/// `field_mappings` is needed separately during IR generation and cannot be embedded
/// into the annotation itself because the same GraphQL type may have different field
/// sources depending on the model being queried.
pub(crate) fn generate_selection_set_ir<'s>(
    selection_set: &normalized_ast::SelectionSet<'s, GDS>,
    data_connector: &'s resolved::data_connector::DataConnector,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, resolved::types::TypeMapping>,
    field_mappings: &BTreeMap<FieldName, resolved::types::FieldMapping>,
    session_variables: &SessionVariables,
    usage_counts: &mut UsagesCounts,
) -> Result<ResultSelectionSet<'s>, error::Error> {
    let mut fields = IndexMap::new();
    for field in selection_set.fields.values() {
        let field_call = field.field_call()?;
        match field_call.info.generic {
            annotation @ Annotation::Output(annotated_field) => match annotated_field {
                OutputAnnotation::Field { name, .. } => {
                    let field_mapping = &field_mappings.get(name).ok_or_else(|| {
                        error::InternalEngineError::InternalGeneric {
                            description: format!("invalid field in annotation: {name:}"),
                        }
                    })?;
                    fields.insert(
                        field.alias.to_string(),
                        FieldSelection::Column {
                            column: field_mapping.column.clone(),
                        },
                    );
                }
                OutputAnnotation::RootField(RootFieldAnnotation::Introspection) => {}
                OutputAnnotation::GlobalIDField { global_id_fields } => {
                    build_global_id_fields(
                        global_id_fields,
                        field_mappings,
                        &field.alias,
                        &mut fields,
                    )?;
                }
                OutputAnnotation::RelayNodeInterfaceID { typename_mappings } => {
                    // Even though we already have the value of the global ID field
                    // here, we try to re-compute the value of the same ID by decoding the ID.
                    // We do this because it simplifies the code structure.
                    // If the NDC were to accept key-value pairs from the v3-engine that will
                    // then be outputted as it is, then we could avoid this computation.
                    let type_name = field.selection_set.type_name.clone().ok_or(
                        error::InternalEngineError::InternalGeneric {
                            description: "typename not found while resolving NodeInterfaceId"
                                .to_string(),
                        },
                    )?;
                    let global_id_fields = typename_mappings.get(&type_name).ok_or(
                        error::InternalEngineError::InternalGeneric {
                            description: format!(
                                "Global ID fields not found of the type {}",
                                type_name
                            ),
                        },
                    )?;

                    build_global_id_fields(
                        global_id_fields,
                        field_mappings,
                        &field.alias,
                        &mut fields,
                    )?;
                }
                OutputAnnotation::RelationshipToModel(relationship_annotation) => {
                    fields.insert(
                        field.alias.to_string(),
                        relationship::generate_model_relationship_ir(
                            field,
                            relationship_annotation,
                            data_connector,
                            type_mappings,
                            session_variables,
                            usage_counts,
                        )?,
                    );
                }
                OutputAnnotation::RelationshipToCommand(relationship_annotation) => {
                    fields.insert(
                        field.alias.to_string(),
                        relationship::generate_command_relationship_ir(
                            field,
                            relationship_annotation,
                            data_connector,
                            type_mappings,
                            session_variables,
                            usage_counts,
                        )?,
                    );
                }
                _ => Err(error::InternalEngineError::UnexpectedAnnotation {
                    annotation: annotation.clone(),
                })?,
            },

            annotation => Err(error::InternalEngineError::UnexpectedAnnotation {
                annotation: annotation.clone(),
            })?,
        }
    }
    Ok(ResultSelectionSet { fields })
}

/// Convert selection set IR (`ResultSelectionSet`) into NDC fields
#[allow(irrefutable_let_patterns)]
pub(crate) fn process_selection_set_ir<'s>(
    model_selection: &ResultSelectionSet<'s>,
    join_id_counter: &mut MonotonicCounter,
) -> Result<
    (
        IndexMap<String, ndc::models::Field>,
        JoinLocations<RemoteJoin<'s>>,
    ),
    error::Error,
> {
    let mut ndc_fields = IndexMap::new();
    let mut join_locations = JoinLocations::new();
    for (alias, field) in &model_selection.fields {
        match field {
            FieldSelection::Column { column } => {
                ndc_fields.insert(
                    alias.to_string(),
                    ndc::models::Field::Column {
                        column: column.clone(),
                    },
                );
            }
            FieldSelection::ModelRelationshipLocal {
                query,
                name,
                relationship_info: _,
            } => {
                let (relationship_query, jl) =
                    model_selection::ir_to_ndc_query(query, join_id_counter)?;
                let ndc_field = ndc::models::Field::Relationship {
                    query: Box::new(relationship_query),
                    relationship: name.to_string(),
                    arguments: BTreeMap::new(),
                };
                if !jl.locations.is_empty() {
                    join_locations.locations.insert(
                        alias.clone(),
                        Location {
                            join_node: None,
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
                    commands::ir_to_ndc_query(&ir.command_info, join_id_counter)?;

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
                            join_node: None,
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
                let (ndc_ir, sub_join_locations) =
                    model_selection::ir_to_ndc_ir(ir, join_id_counter)?;
                let rj_info = RemoteJoin {
                    target_ndc_ir: ndc_ir,
                    target_data_connector: ir.data_connector,
                    join_mapping,
                    process_response_as: ResponseType::Array { is_nullable: true },
                    remote_join_type: RemoteJoinType::ToModel,
                };
                join_locations.locations.insert(
                    alias.clone(),
                    Location {
                        join_node: Some(rj_info),
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
                let (ndc_ir, sub_join_locations) = ir_to_ndc_query_ir(ir, join_id_counter)?;
                let rj_info = RemoteJoin {
                    target_ndc_ir: ndc_ir,
                    target_data_connector: ir.command_info.data_connector,
                    join_mapping,
                    process_response_as: ResponseType::Command {
                        // TODO: fix and remove clone()
                        command_name: ir.command_info.command_name.clone(),
                        type_container: ir.command_info.type_container.clone(),
                    },
                    remote_join_type: RemoteJoinType::ToCommand,
                };
                join_locations.locations.insert(
                    alias.clone(),
                    Location {
                        join_node: Some(rj_info),
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
pub(crate) fn collect_relationships(
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
                collect_relationships(&query.selection, relationships)?;
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
                collect_relationships(&ir.command_info.selection, relationships)?;
            }
            // we ignore remote relationships as we are generating relationship
            // definition for one data connector
            FieldSelection::ModelRelationshipRemote { .. } => (),
            FieldSelection::CommandRelationshipRemote { .. } => (),
        };
    }
    Ok(())
}
