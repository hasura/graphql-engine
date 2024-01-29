use hasura_authn_core::SessionVariables;
use indexmap::IndexMap;
use lang_graphql::ast::common::Alias;
use lang_graphql::normalized_ast;
use open_dds::relationships::RelationshipName;
use open_dds::types::{CustomTypeName, FieldName};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

use super::commands::FunctionBasedCommand;
use super::model_selection::ModelSelection;
use super::relationship::{
    self, LocalCommandRelationshipInfo, LocalModelRelationshipInfo, RemoteCommandRelationshipInfo,
    RemoteModelRelationshipInfo,
};
use crate::execute::error;
use crate::execute::global_id;
use crate::execute::model_tracking::UsagesCounts;
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
        // Relationship names needs to be unique across the IR. This field contains
        // the uniquely generated relationship name. `ModelRelationshipAnnotation`
        // contains a relationship name but that is the name from the metadata.
        name: NDCRelationshipName,
        relationship_info: LocalModelRelationshipInfo<'s>,
    },
    CommandRelationshipLocal {
        ir: FunctionBasedCommand<'s>,
        name: NDCRelationshipName,
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

/// The unique relationship name that is passed to NDC
// Relationship names needs to be unique across the IR. This is so that, the
// NDC can use these names to figure out what joins to use.
// A single "source type" can have only one relationship with a given name,
// hence the relationship name in the IR is a tuple between the source type
// and the relationship name.
// Relationship name = (source_type, relationship_name)
#[derive(
    Serialize,
    Deserialize,
    Clone,
    Debug,
    PartialEq,
    Eq,
    Hash,
    derive_more::Display,
    JsonSchema,
    PartialOrd,
    Ord,
)]
pub struct NDCRelationshipName(pub(crate) String);

impl NDCRelationshipName {
    pub fn new(
        source_type: &Qualified<CustomTypeName>,
        relationship_name: &RelationshipName,
    ) -> Result<Self, error::Error> {
        let name = serde_json::to_string(&(source_type, relationship_name))?;
        Ok(NDCRelationshipName(name))
    }
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
