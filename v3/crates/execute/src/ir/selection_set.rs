use hasura_authn_core::SessionVariables;
use indexmap::IndexMap;
use lang_graphql::ast::common::Alias;
use lang_graphql::normalized_ast;
use open_dds::data_connector::DataConnectorColumnName;
use open_dds::relationships::RelationshipName;
use open_dds::types::{CustomTypeName, DataConnectorArgumentName, FieldName};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

use super::arguments;
use super::commands::FunctionBasedCommand;
use super::model_selection::ModelSelection;
use super::relationship::{
    self, LocalCommandRelationshipInfo, LocalModelRelationshipInfo, RemoteCommandRelationshipInfo,
    RemoteModelRelationshipInfo,
};
use crate::global_id;
use crate::ir::error;
use crate::model_tracking::UsagesCounts;
use metadata_resolve;
use schema::TypeKind;
use schema::{Annotation, OutputAnnotation, RootFieldAnnotation, GDS};

#[derive(Debug, Serialize)]
pub(crate) enum NestedSelection<'s> {
    Object(ResultSelectionSet<'s>),
    Array(Box<NestedSelection<'s>>),
}

#[derive(Debug, Serialize)]
pub(crate) enum FieldSelection<'s> {
    Column {
        column: DataConnectorColumnName,
        nested_selection: Option<NestedSelection<'s>>,
        arguments: BTreeMap<DataConnectorArgumentName, arguments::Argument>,
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
        relationship_info: RemoteModelRelationshipInfo,
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
        source_type: &metadata_resolve::Qualified<CustomTypeName>,
        relationship_name: &RelationshipName,
    ) -> Result<Self, error::Error> {
        let name = serde_json::to_string(&(source_type, relationship_name))?;
        Ok(NDCRelationshipName(name))
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

/// IR that represents the selected fields of an output type.
#[derive(Debug, Serialize, Default)]
pub(crate) struct ResultSelectionSet<'s> {
    // The fields in the selection set. They are stored in the form that would
    // be converted and sent over the wire. Serialized the map as ordered to
    // produce deterministic golden files.
    pub(crate) fields: IndexMap<String, FieldSelection<'s>>,
}

impl<'s> ResultSelectionSet<'s> {
    /// Takes a 'FieldMapping' and returns the alias, if the field is found in
    /// existing fields
    pub(crate) fn contains(&self, other_field: &metadata_resolve::FieldMapping) -> Option<String> {
        self.fields.iter().find_map(|(alias, field)| match field {
            FieldSelection::Column { column, .. } => {
                if column.as_str() == other_field.column.as_str() {
                    Some(alias.clone())
                } else {
                    None
                }
            }
            _ => None,
        })
    }
}

fn build_global_id_fields(
    global_id_fields: &Vec<FieldName>,
    field_mappings: &BTreeMap<FieldName, metadata_resolve::FieldMapping>,
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
                nested_selection: None,
                arguments: BTreeMap::new(),
            },
        );
    }
    Ok(())
}

pub(crate) fn generate_nested_selection<'s>(
    qualified_type_reference: &metadata_resolve::QualifiedTypeReference,
    field_base_type_kind: TypeKind,
    field: &normalized_ast::Field<'s, GDS>,
    data_connector: &'s metadata_resolve::DataConnectorLink,
    type_mappings: &'s BTreeMap<
        metadata_resolve::Qualified<CustomTypeName>,
        metadata_resolve::TypeMapping,
    >,
    session_variables: &SessionVariables,
    request_headers: &reqwest::header::HeaderMap,
    usage_counts: &mut UsagesCounts,
) -> Result<Option<NestedSelection<'s>>, error::Error> {
    match &qualified_type_reference.underlying_type {
        metadata_resolve::QualifiedBaseType::List(element_type) => {
            let array_selection = generate_nested_selection(
                element_type,
                field_base_type_kind,
                field,
                data_connector,
                type_mappings,
                session_variables,
                request_headers,
                usage_counts,
            )?;
            Ok(array_selection.map(|a| NestedSelection::Array(Box::new(a))))
        }
        metadata_resolve::QualifiedBaseType::Named(qualified_type_name) => {
            match qualified_type_name {
                metadata_resolve::QualifiedTypeName::Inbuilt(_) => Ok(None), // Inbuilt types are all scalars so there should be no subselections.
                metadata_resolve::QualifiedTypeName::Custom(data_type) => {
                    match field_base_type_kind {
                        TypeKind::Scalar => Ok(None),
                        TypeKind::Object => {
                            let metadata_resolve::TypeMapping::Object { field_mappings, .. } =
                                type_mappings.get(data_type).ok_or(
                                    error::InternalEngineError::InternalGeneric {
                                        description: format!(
                                            "no type mapping found for type {data_type}"
                                        ),
                                    },
                                )?;
                            let nested_selection = generate_selection_set_ir(
                                &field.selection_set,
                                data_connector,
                                type_mappings,
                                field_mappings,
                                session_variables,
                                request_headers,
                                usage_counts,
                            )?;
                            Ok(Some(NestedSelection::Object(nested_selection)))
                        }
                    }
                }
            }
        }
    }
}

/// Builds the IR from a normalized selection set
/// `field_mappings` is needed separately during IR generation and cannot be embedded
/// into the annotation itself because the same GraphQL type may have different field
/// sources depending on the model being queried.
pub(crate) fn generate_selection_set_ir<'s>(
    selection_set: &normalized_ast::SelectionSet<'s, GDS>,
    data_connector: &'s metadata_resolve::DataConnectorLink,
    type_mappings: &'s BTreeMap<
        metadata_resolve::Qualified<CustomTypeName>,
        metadata_resolve::TypeMapping,
    >,
    field_mappings: &BTreeMap<FieldName, metadata_resolve::FieldMapping>,
    session_variables: &SessionVariables,
    request_headers: &reqwest::header::HeaderMap,
    usage_counts: &mut UsagesCounts,
) -> Result<ResultSelectionSet<'s>, error::Error> {
    let mut fields = IndexMap::new();
    for field in selection_set.fields.values() {
        let field_call = field.field_call()?;
        match field_call.info.generic {
            annotation @ Annotation::Output(annotated_field) => match annotated_field {
                OutputAnnotation::Field {
                    name,
                    field_type,
                    field_base_type_kind,
                    parent_type: _,
                    argument_types,
                } => {
                    let field_mapping = &field_mappings.get(name).ok_or_else(|| {
                        error::InternalEngineError::InternalGeneric {
                            description: format!("invalid field in annotation: {name:}"),
                        }
                    })?;
                    let nested_selection = generate_nested_selection(
                        field_type,
                        *field_base_type_kind,
                        field,
                        data_connector,
                        type_mappings,
                        session_variables,
                        request_headers,
                        usage_counts,
                    )?;
                    let mut field_arguments = BTreeMap::new();
                    for (argument_name, argument_type) in argument_types {
                        let argument_value = match field_call.arguments.get(argument_name) {
                            None => {
                                if argument_type.nullable {
                                    Ok(None)
                                } else {
                                    Err(error::Error::MissingNonNullableArgument {
                                        argument_name: argument_name.to_string(),
                                        field_name: name.to_string(),
                                    })
                                }
                            }
                            Some(val) => arguments::map_argument_value_to_ndc_type(
                                argument_type,
                                &val.value,
                                type_mappings,
                            )
                            .map(Some),
                        }?;
                        if let Some(argument_value) = argument_value {
                            let argument = arguments::Argument::Literal {
                                value: argument_value,
                            };
                            // If argument name is not found in the mapping, use the open_dd argument name as the ndc argument name
                            let ndc_argument_name = field_mapping
                                .argument_mappings
                                .get(argument_name.as_str())
                                .map_or_else(
                                    || DataConnectorArgumentName::from(argument_name.as_str()),
                                    Clone::clone,
                                );
                            field_arguments.insert(ndc_argument_name, argument);
                        }
                    }

                    fields.insert(
                        field.alias.to_string(),
                        FieldSelection::Column {
                            column: field_mapping.column.clone(),
                            nested_selection,
                            arguments: field_arguments,
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
                                "Global ID fields not found of the type {type_name}"
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
                            request_headers,
                            usage_counts,
                        )?,
                    );
                }
                OutputAnnotation::RelationshipToModelAggregate(relationship_annotation) => {
                    fields.insert(
                        field.alias.to_string(),
                        relationship::generate_model_aggregate_relationship_ir(
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
                            request_headers,
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
