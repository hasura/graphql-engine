use hasura_authn_core::SessionVariables;
use indexmap::IndexMap;
use lang_graphql::ast::common::Alias;
use lang_graphql::normalized_ast;
use open_dds::data_connector::DataConnectorColumnName;
use open_dds::query::{Name, ObjectFieldOperand};
use open_dds::types::{CustomTypeName, DataConnectorArgumentName, FieldName};
use plan_types::NdcFieldAlias;
use serde::Serialize;
use std::collections::BTreeMap;

use super::arguments;
use super::commands::FunctionBasedCommand;
use super::model_selection::ModelSelection;
use super::relationship::{self, RemoteCommandRelationshipInfo, RemoteModelRelationshipInfo};
use crate::aggregates::mk_alias_from_graphql_field_path;
use crate::error;
use crate::flags::GraphqlIrFlags;
use crate::global_id;
use graphql_schema::{
    AggregateOutputAnnotation, AggregationFunctionAnnotation, InputAnnotation, TypeKind,
};
use graphql_schema::{Annotation, GDS, OutputAnnotation, RootFieldAnnotation};
use metadata_resolve::{ObjectTypeWithRelationships, Qualified};
use plan::UnresolvedArgument;
use plan_types::{
    LocalCommandRelationshipInfo, LocalModelRelationshipInfo, NdcRelationshipName, UsagesCounts,
};

#[derive(Debug, Serialize)]
pub enum NestedSelection<'s> {
    Object(ResultSelectionSet<'s>),
    Array(Box<NestedSelection<'s>>),
}

#[derive(Debug, Serialize)]
pub enum FieldSelection<'s> {
    Column {
        column: DataConnectorColumnName,
        nested_selection: Option<NestedSelection<'s>>,
        arguments: BTreeMap<DataConnectorArgumentName, UnresolvedArgument<'s>>,
    },
    ModelRelationshipLocal {
        query: ModelSelection<'s>,
        // Relationship names needs to be unique across the IR. This field contains
        // the uniquely generated relationship name. `ModelRelationshipAnnotation`
        // contains a relationship name but that is the name from the metadata.
        name: NdcRelationshipName,
        relationship_info: LocalModelRelationshipInfo<'s>,
    },
    CommandRelationshipLocal {
        ir: FunctionBasedCommand<'s>,
        name: NdcRelationshipName,
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

/// IR that represents the selected fields of an output type.
#[derive(Debug, Serialize, Default)]
pub struct ResultSelectionSet<'s> {
    // The fields in the selection set. They are stored in the form that would
    // be converted and sent over the wire. Serialized the map as ordered to
    // produce deterministic golden files.
    pub fields: IndexMap<NdcFieldAlias, FieldSelection<'s>>,
}

impl ResultSelectionSet<'_> {
    /// Check if the field is found in existing fields. Returns the alias of the field.
    pub fn contains(
        &self,
        other_field: &metadata_resolve::FieldMapping,
    ) -> Option<(&NdcFieldAlias, &Option<NestedSelection<'_>>)> {
        self.fields.iter().find_map(|(alias, field)| match field {
            FieldSelection::Column {
                column,
                nested_selection,
                ..
            } => {
                if column.as_str() == other_field.column.as_str() {
                    Some((alias, nested_selection))
                } else {
                    None
                }
            }
            _ => None,
        })
    }
}

fn build_global_id_fields_for_open_dd_ir(
    global_id_fields: &[FieldName],
    field_alias: &Alias,
    fields: &mut IndexMap<open_dds::query::Alias, open_dds::query::ObjectSubSelection>,
) {
    for field_name in global_id_fields {
        // Prefix the global column id with something that will be unlikely to be chosen
        // by the user,
        //  to not have any conflicts with any of the fields
        // in the selection set.
        let global_col_id_alias = global_id::global_id_col_format(field_alias, field_name);

        let field_selection =
            open_dds::query::ObjectSubSelection::Field(open_dds::query::ObjectFieldSelection {
                selection: None,
                target: open_dds::query::ObjectFieldTarget {
                    arguments: IndexMap::new(),
                    field_name: field_name.clone(),
                },
            });

        fields.insert(
            open_dds::query::Alias::from(
                open_dds::identifier::Identifier::new(global_col_id_alias.as_str()).unwrap(),
            ),
            field_selection,
        );
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum NestedSelectionType {
    /// The nested selection is selecting the root of a command.
    CommandRootSelection,

    /// Any other nested selection
    NestedSelection,
}

pub fn generate_nested_selection_open_dd_ir(
    qualified_type_reference: &metadata_resolve::QualifiedTypeReference,
    field_base_type_kind: TypeKind,
    selection_set_field_nestedness: metadata_resolve::FieldNestedness,
    models: &IndexMap<
        metadata_resolve::Qualified<open_dds::models::ModelName>,
        metadata_resolve::ModelWithPermissions,
    >,
    type_mappings: &BTreeMap<
        metadata_resolve::Qualified<CustomTypeName>,
        metadata_resolve::TypeMapping,
    >,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
    nested_selection_type: NestedSelectionType,
    field: &normalized_ast::Field<'_, GDS>,
    session_variables: &SessionVariables,
    request_headers: &reqwest::header::HeaderMap,
    flags: &GraphqlIrFlags,
    usage_counts: &mut UsagesCounts,
) -> Result<
    Option<IndexMap<open_dds::query::Alias, open_dds::query::ObjectSubSelection>>,
    error::Error,
> {
    match &qualified_type_reference.underlying_type {
        metadata_resolve::QualifiedBaseType::List(element_type) => {
            // If we're selecting the root of a command, then we don't regard this as a "nested field" as such
            // until we nest past the return type of the command.
            // Commands use nested selections for their root because they are either embedded in a '__value'
            // field in a single row for queries or use nested selection types at their root for mutations.
            // However, we don't consider these to be truly nested until they nest past their return type.
            let new_nestedness = match nested_selection_type {
                NestedSelectionType::CommandRootSelection => selection_set_field_nestedness,
                NestedSelectionType::NestedSelection => selection_set_field_nestedness
                    .max(metadata_resolve::FieldNestedness::ArrayNested),
            };

            let array_selection = generate_nested_selection_open_dd_ir(
                element_type,
                field_base_type_kind,
                new_nestedness,
                models,
                type_mappings,
                object_types,
                NestedSelectionType::NestedSelection,
                field,
                session_variables,
                request_headers,
                flags,
                usage_counts,
            )?;
            Ok(array_selection)
        }
        metadata_resolve::QualifiedBaseType::Named(qualified_type_name) => {
            match qualified_type_name {
                metadata_resolve::QualifiedTypeName::Inbuilt(_) => Ok(None), // Inbuilt types are all scalars so there should be no subselections.
                metadata_resolve::QualifiedTypeName::Custom(_data_type) => {
                    match field_base_type_kind {
                        TypeKind::Scalar => Ok(None),
                        TypeKind::Object => {
                            let nested_selection = generate_selection_set_open_dd_ir(
                                &field.selection_set,
                                selection_set_field_nestedness,
                                models,
                                type_mappings,
                                object_types,
                                session_variables,
                                request_headers,
                                flags,
                                usage_counts,
                            )?;
                            Ok(Some(nested_selection))
                        }
                    }
                }
            }
        }
    }
}

/// Builds the OpenDD IR from a normalized selection set
pub fn generate_selection_set_open_dd_ir(
    selection_set: &normalized_ast::SelectionSet<'_, GDS>,
    selection_set_field_nestedness: metadata_resolve::FieldNestedness,
    models: &IndexMap<
        metadata_resolve::Qualified<open_dds::models::ModelName>,
        metadata_resolve::ModelWithPermissions,
    >,
    type_mappings: &BTreeMap<
        metadata_resolve::Qualified<CustomTypeName>,
        metadata_resolve::TypeMapping,
    >,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
    session_variables: &SessionVariables,
    request_headers: &reqwest::header::HeaderMap,
    flags: &GraphqlIrFlags,
    usage_counts: &mut UsagesCounts,
) -> Result<IndexMap<open_dds::query::Alias, open_dds::query::ObjectSubSelection>, error::Error> {
    let mut fields = IndexMap::new();
    for field in selection_set.fields.values() {
        let field_call = field.field_call()?;
        match field_call.info.generic {
            annotation @ Annotation::Output(annotated_field) => {
                match annotated_field {
                    OutputAnnotation::Field {
                        name,
                        field_type,
                        field_base_type_kind,
                        argument_types,
                        ..
                    } => {
                        let nested_selection = generate_nested_selection_open_dd_ir(
                            field_type,
                            *field_base_type_kind,
                            selection_set_field_nestedness
                                .max(metadata_resolve::FieldNestedness::ObjectNested),
                            models,
                            type_mappings,
                            object_types,
                            NestedSelectionType::NestedSelection,
                            field,
                            session_variables,
                            request_headers,
                            flags,
                            usage_counts,
                        )?;
                        let mut field_arguments = IndexMap::new();
                        for (argument_name, argument_type) in argument_types {
                            match field_call.arguments.get(argument_name) {
                                None => {
                                    if !argument_type.nullable {
                                        Err(error::Error::MissingNonNullableArgument {
                                            argument_name: argument_name.to_string(),
                                            field_name: name.to_string(),
                                        })?;
                                    }
                                }
                                Some(val) => {
                                    let Annotation::Input(InputAnnotation::FieldArgument {
                                        argument_name,
                                    }) = val.info.generic
                                    else {
                                        Err(error::InternalEngineError::UnexpectedAnnotation {
                                            annotation: val.info.generic.clone(),
                                        })?
                                    };
                                    let argument_value = arguments::map_argument_value_to_ndc_type(
                                        argument_type,
                                        &val.value,
                                        type_mappings,
                                        flags,
                                    )?;
                                    field_arguments.insert(
                                        argument_name.clone(),
                                        open_dds::query::Value::Literal(argument_value),
                                    );
                                }
                            }
                        }
                        let field_selection = open_dds::query::ObjectSubSelection::Field(
                            open_dds::query::ObjectFieldSelection {
                                selection: nested_selection,
                                target: open_dds::query::ObjectFieldTarget {
                                    arguments: field_arguments,
                                    field_name: name.clone(),
                                },
                            },
                        );

                        fields.insert(make_field_alias(field.alias.0.as_str())?, field_selection);
                    }
                    OutputAnnotation::RootField(RootFieldAnnotation::Introspection) => {}
                    OutputAnnotation::GlobalIDField { global_id_fields } => {
                        build_global_id_fields_for_open_dd_ir(
                            global_id_fields,
                            &field.alias,
                            &mut fields,
                        );
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

                        build_global_id_fields_for_open_dd_ir(
                            global_id_fields,
                            &field.alias,
                            &mut fields,
                        );
                    }
                    OutputAnnotation::RelationshipToModel(relationship_annotation) => {
                        fields.insert(
                            make_field_alias(field.alias.0.as_str())?,
                            open_dds::query::ObjectSubSelection::Relationship(
                                relationship::generate_model_relationship_open_dd_ir(
                                    field,
                                    models,
                                    type_mappings,
                                    object_types,
                                    relationship_annotation,
                                    session_variables,
                                    request_headers,
                                    flags,
                                    usage_counts,
                                )?,
                            ),
                        );
                    }
                    OutputAnnotation::RelationshipToModelAggregate(relationship_annotation) => {
                        fields.insert(
                            make_field_alias(field.alias.0.as_str())?,
                            open_dds::query::ObjectSubSelection::RelationshipAggregate(
                                relationship::generate_model_aggregate_relationship_open_dd_ir(
                                    field,
                                    relationship_annotation,
                                    models,
                                    flags,
                                    usage_counts,
                                )?,
                            ),
                        );
                    }
                    OutputAnnotation::RelationshipToCommand(relationship_annotation) => {
                        fields.insert(
                            make_field_alias(field.alias.0.as_str())?,
                            open_dds::query::ObjectSubSelection::Relationship(
                                relationship::generate_command_relationship_open_dd_ir(
                                    field,
                                    relationship_annotation,
                                    models,
                                    type_mappings,
                                    object_types,
                                    session_variables,
                                    request_headers,
                                    flags,
                                    usage_counts,
                                )?,
                            ),
                        );
                    }
                    _ => Err(error::InternalEngineError::UnexpectedAnnotation {
                        annotation: annotation.clone(),
                    })?,
                }
            }

            annotation => Err(error::InternalEngineError::UnexpectedAnnotation {
                annotation: annotation.clone(),
            })?,
        }
    }
    Ok(fields)
}

/// Builds the OpenDD IR from a normalized selection set
pub fn generate_aggregate_selection_set_open_dd_ir(
    selection_set: &normalized_ast::SelectionSet<'_, GDS>,
) -> Result<IndexMap<Name, open_dds::query::Aggregate>, error::Error> {
    let mut fields = IndexMap::new();
    collect_aggregate_fields(&mut fields, &[], &[], selection_set)?;
    Ok(fields)
}

fn collect_aggregate_fields(
    aggregate_fields: &mut IndexMap<Name, open_dds::query::Aggregate>,
    field_path: &[&open_dds::query::ObjectFieldTarget],
    alias_path: &[&Alias], // For generating the alias for the aggregate field
    selection_set: &normalized_ast::SelectionSet<'_, GDS>,
) -> Result<(), error::Error> {
    for field in selection_set.fields.values() {
        let field_call = field.field_call()?;
        let alias_path = alias_path
            .iter()
            .chain(std::iter::once(&&field.alias))
            .copied()
            .collect::<Vec<&Alias>>();
        let field_alias = make_field_alias(mk_alias_from_graphql_field_path(&alias_path).as_str())?;

        match field_call.info.generic {
            annotation @ Annotation::Output(annotated_field) => match annotated_field {
                OutputAnnotation::Aggregate(aggregate_annotation) => match aggregate_annotation {
                    AggregateOutputAnnotation::AggregatableField {
                        field_name,
                        aggregate_operand_type: _,
                    } => {
                        let arguments = IndexMap::new();
                        let path_item = open_dds::query::ObjectFieldTarget {
                            field_name: field_name.clone(),
                            arguments,
                        };

                        let mut new_field_path = field_path.to_vec();
                        new_field_path.push(&path_item);

                        collect_aggregate_fields(
                            aggregate_fields,
                            &new_field_path,
                            &alias_path,
                            &field.selection_set,
                        )?;
                    }
                    AggregateOutputAnnotation::AggregationFunctionField(aggregation_function) => {
                        let function = match aggregation_function {
                            AggregationFunctionAnnotation::Count => {
                                open_dds::query::AggregationFunction::Count {}
                            }
                            AggregationFunctionAnnotation::CountDistinct => {
                                open_dds::query::AggregationFunction::CountDistinct {}
                            }
                            AggregationFunctionAnnotation::Function {
                                function_name,
                                data_connector_functions: _,
                            } => open_dds::query::AggregationFunction::Custom {
                                name: function_name.clone(),
                            },
                        };

                        // start at end of path list and keep wrapping them in object field
                        // operands
                        let operand: Option<open_dds::query::Operand> = field_path
                            .iter()
                            .rev()
                            .fold(None, |operand, object_field_target| {
                                Some(open_dds::query::Operand::Field(ObjectFieldOperand {
                                    target: Box::new((*object_field_target).clone()),
                                    nested: operand.map(Box::new),
                                }))
                            });

                        aggregate_fields.insert(
                            Name::from(field_alias.as_str().to_owned()),
                            open_dds::query::Aggregate { function, operand },
                        );
                    }
                },
                OutputAnnotation::RootField(graphql_schema::RootFieldAnnotation::Introspection) => {
                } // Skip introspection fields such as __typename, as they will be processed during response handling.
                _ => Err(error::InternalEngineError::UnexpectedAnnotation {
                    annotation: annotation.clone(),
                })?,
            },
            annotation => Err(error::InternalEngineError::UnexpectedAnnotation {
                annotation: annotation.clone(),
            })?,
        }
    }
    Ok(())
}

fn make_field_alias(alias: &str) -> Result<open_dds::query::Alias, error::Error> {
    Ok(open_dds::query::Alias::from(
        open_dds::identifier::Identifier::new(alias).map_err(|_| error::Error::InvalidAlias {
            alias: alias.to_string(),
        })?,
    ))
}
