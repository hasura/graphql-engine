use hasura_authn_core::SessionVariables;
use indexmap::IndexMap;
use lang_graphql::normalized_ast::{self, Field};
use open_dds::{
    arguments::ArgumentName,
    types::{CustomTypeName, FieldName},
};
use std::collections::BTreeMap;

use serde::Serialize;

use super::{
    filter,
    selection_set::{self, generate_selection_set_open_dd_ir},
};
use crate::{
    arguments, error,
    query_root::select_aggregate::{AggregateQuery, aggregate_query},
};
use crate::{flags::GraphqlIrFlags, order_by};
use graphql_schema::{
    Annotation, BooleanExpressionAnnotation, CommandRelationshipAnnotation, GDS, InputAnnotation,
    ModelAggregateRelationshipAnnotation, ModelInputAnnotation, ModelRelationshipAnnotation,
};
use metadata_resolve::{self, ObjectTypeWithRelationships, Qualified, QualifiedTypeReference};
use plan::{count_command, count_model};
use plan_types::{RemoteJoinObjectFieldMapping, TargetField, UsagesCounts};

#[derive(Debug, Clone, Serialize)]
pub struct RemoteModelRelationshipInfo {
    /// This contains processed information about the mappings.
    /// `RelationshipMapping` only contains mapping of field names. This
    /// contains mapping of field names and `metadata_resolve::FieldMapping`.
    /// Also see `build_remote_relationship`.
    pub join_mapping: Vec<(SourceField, TargetField)>,
    /// For any object types used in the join_mapping fields, this contains how to map the object fields
    /// from the names used in the source to the names used in the target
    pub object_type_field_mappings:
        BTreeMap<Qualified<CustomTypeName>, RemoteJoinObjectFieldMapping>,
}

#[derive(Debug, Serialize)]
pub struct RemoteCommandRelationshipInfo<'s> {
    pub annotation: &'s CommandRelationshipAnnotation,
    pub join_mapping: Vec<(SourceField, ArgumentName)>,
    /// For any object types used in the join_mapping fields, this contains how to map the object fields
    /// from the names used in the source to the names used in the target
    pub object_type_field_mappings:
        BTreeMap<Qualified<CustomTypeName>, RemoteJoinObjectFieldMapping>,
}

pub type SourceField = (
    FieldName,
    QualifiedTypeReference,
    metadata_resolve::FieldMapping,
);

pub fn generate_model_relationship_open_dd_ir<'s>(
    field: &Field<'s, GDS>,
    models: &'s IndexMap<
        Qualified<open_dds::models::ModelName>,
        metadata_resolve::ModelWithPermissions,
    >,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
    relationship_annotation: &'s ModelRelationshipAnnotation,
    session_variables: &SessionVariables,
    request_headers: &reqwest::header::HeaderMap,
    flags: &GraphqlIrFlags,
    usage_counts: &mut UsagesCounts,
) -> Result<open_dds::query::RelationshipSelection, error::Error> {
    // Add the target model being used in the usage counts
    count_model(&relationship_annotation.target_model_name, usage_counts);
    let field_call = field.field_call()?;

    let mut limit = None;
    let mut offset = None;
    let mut where_input = None;
    let mut order_by = Vec::new();

    for argument in field_call.arguments.values() {
        match argument.info.generic {
            annotation @ Annotation::Input(argument_annotation) => match argument_annotation {
                InputAnnotation::Model(model_argument_annotation) => {
                    match model_argument_annotation {
                        ModelInputAnnotation::ModelLimitArgument => {
                            // Limit is optional
                            limit = argument
                                .value
                                .as_nullable(
                                    &flags.validate_non_null_graphql_variables,
                                    normalized_ast::Value::as_int_u32,
                                )
                                .map_err(error::Error::map_unexpected_value_to_external_error)?;
                        }
                        ModelInputAnnotation::ModelOffsetArgument => {
                            // Offset is optional
                            offset = argument
                                .value
                                .as_nullable(
                                    &flags.validate_non_null_graphql_variables,
                                    normalized_ast::Value::as_int_u32,
                                )
                                .map_err(error::Error::map_unexpected_value_to_external_error)?;
                        }
                        ModelInputAnnotation::ModelOrderByExpression => {
                            let target_model = models
                                    .get(&relationship_annotation.target_model_name)
                                    .ok_or_else(|| {
                                        error::InternalError::Developer(
                                            error::InternalDeveloperError::TargetModelNotFoundForRelationship {
                                                model_name: relationship_annotation.target_model_name.clone(),
                                                relationship_name: relationship_annotation.relationship_name.clone(),
                                            },
                                        )
                                    })?;
                            let target_model_source =
                                target_model.model.source.as_ref().ok_or_else(|| {
                                    error::Error::InternalMissingTargetModelSourceForRelationship {
                                        relationship_name: relationship_annotation
                                            .relationship_name
                                            .clone(),
                                        type_name: relationship_annotation.source_type.clone(),
                                    }
                                })?;
                            // order by is optional
                            if let Some(open_dd_order_by) = argument.value.as_nullable(
                                &flags.validate_non_null_graphql_variables,
                                |v| {
                                    order_by::build_order_by_open_dd_ir(
                                        v,
                                        usage_counts,
                                        &target_model_source.data_connector,
                                    )
                                },
                            )? {
                                order_by.extend(open_dd_order_by);
                            }
                        }
                        _ => {
                            Err(error::InternalEngineError::UnexpectedAnnotation {
                                annotation: annotation.clone(),
                            })?;
                        }
                    }
                }
                InputAnnotation::BooleanExpression(
                    BooleanExpressionAnnotation::BooleanExpressionRootField,
                ) => {
                    // where argument is optional
                    where_input = argument.value.as_nullable(
                        &flags.validate_non_null_graphql_variables,
                        normalized_ast::Value::as_object,
                    )?;
                }

                _ => {
                    Err(error::InternalEngineError::UnexpectedAnnotation {
                        annotation: annotation.clone(),
                    })?;
                }
            },

            annotation => {
                Err(error::InternalEngineError::UnexpectedAnnotation {
                    annotation: annotation.clone(),
                })?;
            }
        }
    }

    let where_clause = match where_input {
        Some(where_input) => Some(filter::resolve_filter_expression_open_dd(
            where_input,
            flags,
            usage_counts,
        )?),
        None => None,
    };

    let selection = generate_selection_set_open_dd_ir(
        &field.selection_set,
        metadata_resolve::FieldNestedness::NotNested,
        models,
        type_mappings,
        object_types,
        session_variables,
        request_headers,
        flags,
        usage_counts,
    )?;

    let limit: Option<usize> = limit
        .map(|limit| {
            usize::try_from(limit).map_err(|_| error::Error::InvalidLimitValue { value: limit })
        })
        .transpose()?;

    let offset: Option<usize> = offset
        .map(|offset| {
            usize::try_from(offset).map_err(|_| error::Error::InvalidOffsetValue { value: offset })
        })
        .transpose()?;

    let filter = where_clause;

    let target = open_dds::query::RelationshipTarget {
        relationship_name: relationship_annotation.relationship_name.clone(),
        arguments: IndexMap::new(),
        filter,
        limit,
        offset,
        order_by,
    };

    let relationship_selection = open_dds::query::RelationshipSelection {
        selection: Some(selection),
        target,
    };

    Ok(relationship_selection)
}

pub fn generate_model_aggregate_relationship_open_dd_ir<'s>(
    field: &Field<'s, GDS>,
    relationship_annotation: &'s ModelAggregateRelationshipAnnotation,
    models: &'s IndexMap<
        Qualified<open_dds::models::ModelName>,
        metadata_resolve::ModelWithPermissions,
    >,
    flags: &GraphqlIrFlags,
    usage_counts: &mut UsagesCounts,
) -> Result<open_dds::query::RelationshipAggregateSelection, error::Error> {
    // Add the target model being used in the usage counts
    count_model(&relationship_annotation.target_model_name, usage_counts);

    let selection =
        selection_set::generate_aggregate_selection_set_open_dd_ir(&field.selection_set)?;

    let field_call = field.field_call()?;

    let model_source = models
        .get(&relationship_annotation.target_model_name)
        .ok_or_else(
            || error::InternalDeveloperError::TargetModelNotFoundForRelationship {
                model_name: relationship_annotation.target_model_name.clone(),
                relationship_name: relationship_annotation.relationship_name.clone(),
            },
        )?
        .model
        .source
        .as_ref()
        .ok_or_else(|| error::InternalEngineError::InternalGeneric {
            description: format!(
                "Model {} has no source",
                relationship_annotation.target_model_name
            ),
        })?;

    let AggregateQuery {
        limit,
        offset,
        where_clause,
        model_arguments,
        order_by,
    } = aggregate_query(
        field_call,
        &relationship_annotation.target_model_name,
        model_source,
        flags,
        usage_counts,
    )?;

    let target = open_dds::query::RelationshipTarget {
        relationship_name: relationship_annotation.relationship_name.clone(),
        arguments: model_arguments.unwrap_or_default(),
        filter: where_clause,
        limit,
        offset,
        order_by,
    };

    Ok(open_dds::query::RelationshipAggregateSelection { selection, target })
}

pub fn generate_command_relationship_open_dd_ir<'s>(
    field: &Field<'s, GDS>,
    relationship_annotation: &'s CommandRelationshipAnnotation,
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
) -> Result<open_dds::query::RelationshipSelection, error::Error> {
    count_command(&relationship_annotation.command_name, usage_counts);

    let arguments = &field.field_call()?.arguments;

    let target_arguments = arguments::resolve_model_arguments_input_opendd(
        arguments,
        type_mappings,
        flags,
        usage_counts,
    )?;

    let selection = generate_selection_set_open_dd_ir(
        &field.selection_set,
        metadata_resolve::FieldNestedness::NotNested,
        models,
        type_mappings,
        object_types,
        session_variables,
        request_headers,
        flags,
        usage_counts,
    )?;

    let target = open_dds::query::RelationshipTarget {
        relationship_name: relationship_annotation.relationship_name.clone(),
        arguments: target_arguments,
        filter: None,
        limit: None,
        offset: None,
        order_by: vec![],
    };

    let relationship_selection = open_dds::query::RelationshipSelection {
        selection: Some(selection),
        target,
    };

    Ok(relationship_selection)
}
