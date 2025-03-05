use hasura_authn_core::{Session, SessionVariables};
use indexmap::IndexMap;
use lang_graphql::normalized_ast::{self, Field};
use open_dds::{
    arguments::ArgumentName,
    commands::FunctionName,
    models::ModelName,
    relationships::{RelationshipName, RelationshipType},
    types::{CustomTypeName, DataConnectorArgumentName, FieldName},
};
use std::collections::BTreeMap;

use serde::Serialize;

use super::{
    commands::generate_function_based_command,
    filter,
    model_selection::{self, model_selection_ir},
    order_by::build_ndc_order_by,
    permissions,
    selection_set::{self, generate_selection_set_open_dd_ir, FieldSelection},
};
use crate::order_by;
use crate::{
    error,
    query_root::select_aggregate::{aggregate_query, AggregateQuery},
};
use graphql_schema::{
    Annotation, BooleanExpressionAnnotation, CommandRelationshipAnnotation, InputAnnotation,
    ModelAggregateRelationshipAnnotation, ModelInputAnnotation, ModelRelationshipAnnotation, GDS,
};
use metadata_resolve::{
    self, CommandSource, ObjectTypeWithRelationships, Qualified, RelationshipModelMapping,
};
use plan::{count_command, count_model};
use plan_types::{
    mk_argument_target_variable_name, ComparisonTarget, ComparisonValue, Expression,
    LocalCommandRelationshipInfo, LocalFieldComparison, LocalModelRelationshipInfo,
    NdcRelationshipName, TargetField, UsagesCounts,
};

#[derive(Debug, Clone, Serialize)]
pub struct RemoteModelRelationshipInfo {
    /// This contains processed information about the mappings.
    /// `RelationshipMapping` only contains mapping of field names. This
    /// contains mapping of field names and `metadata_resolve::FieldMapping`.
    /// Also see `build_remote_relationship`.
    pub join_mapping: Vec<(SourceField, TargetField)>,
}

#[derive(Debug, Serialize)]
pub struct RemoteCommandRelationshipInfo<'s> {
    pub annotation: &'s CommandRelationshipAnnotation,
    pub join_mapping: Vec<(SourceField, ArgumentName)>,
}

pub type SourceField = (FieldName, metadata_resolve::FieldMapping);

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
                                .as_nullable(normalized_ast::Value::as_int_u32)
                                .map_err(error::Error::map_unexpected_value_to_external_error)?;
                        }
                        ModelInputAnnotation::ModelOffsetArgument => {
                            // Offset is optional
                            offset = argument
                                .value
                                .as_nullable(normalized_ast::Value::as_int_u32)
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
                            if let Some(open_dd_order_by) = argument.value.as_nullable(|v| {
                                order_by::build_order_by_open_dd_ir(
                                    v,
                                    usage_counts,
                                    &target_model_source.data_connector,
                                )
                            })? {
                                order_by.extend(open_dd_order_by);
                            }
                        }
                        _ => {
                            return Err(error::InternalEngineError::UnexpectedAnnotation {
                                annotation: annotation.clone(),
                            })?
                        }
                    }
                }
                InputAnnotation::BooleanExpression(
                    BooleanExpressionAnnotation::BooleanExpressionRootField,
                ) => {
                    if let Some(_target_capabilities) = &relationship_annotation.target_capabilities
                    {
                        // where argument is optional
                        where_input = argument
                            .value
                            .as_nullable(normalized_ast::Value::as_object)?;
                    }
                }

                _ => {
                    return Err(error::InternalEngineError::UnexpectedAnnotation {
                        annotation: annotation.clone(),
                    })?
                }
            },

            annotation => {
                return Err(error::InternalEngineError::UnexpectedAnnotation {
                    annotation: annotation.clone(),
                })?
            }
        }
    }

    let where_clause = match where_input {
        Some(where_input) => Some(filter::resolve_filter_expression_open_dd(
            where_input,
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

pub fn generate_model_relationship_ir<'s>(
    field: &Field<'s, GDS>,
    relationship_annotation: &'s ModelRelationshipAnnotation,
    relationship_field_nestedness: metadata_resolve::FieldNestedness,
    source_data_connector: &'s metadata_resolve::DataConnectorLink,
    source_type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    models: &'s IndexMap<
        Qualified<open_dds::models::ModelName>,
        metadata_resolve::ModelWithPermissions,
    >,
    commands: &'s IndexMap<
        metadata_resolve::Qualified<open_dds::commands::CommandName>,
        metadata_resolve::CommandWithPermissions,
    >,
    object_types: &'s BTreeMap<
        Qualified<open_dds::types::CustomTypeName>,
        metadata_resolve::ObjectTypeWithRelationships,
    >,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    usage_counts: &mut UsagesCounts,
) -> Result<FieldSelection<'s>, error::Error> {
    // Add the target model being used in the usage counts
    count_model(&relationship_annotation.target_model_name, usage_counts);
    let field_call = field.field_call()?;

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

    let (target_source, target_capabilities) = target_model
        .model
        .source
        .as_deref()
        .zip(relationship_annotation.target_capabilities.as_ref())
        .ok_or_else(
            || error::Error::InternalMissingTargetModelSourceForRelationship {
                relationship_name: relationship_annotation.relationship_name.clone(),
                type_name: relationship_annotation.source_type.clone(),
            },
        )?;

    let mut limit = None;
    let mut offset = None;
    let mut where_clause = None;
    let mut order_by = None;

    for argument in field_call.arguments.values() {
        match argument.info.generic {
            annotation @ Annotation::Input(argument_annotation) => match argument_annotation {
                InputAnnotation::Model(model_argument_annotation) => {
                    match model_argument_annotation {
                        ModelInputAnnotation::ModelLimitArgument => {
                            // Limit is optional
                            limit = argument
                                .value
                                .as_nullable(normalized_ast::Value::as_int_u32)
                                .map_err(error::Error::map_unexpected_value_to_external_error)?;
                        }
                        ModelInputAnnotation::ModelOffsetArgument => {
                            // Offset is optional
                            offset = argument
                                .value
                                .as_nullable(normalized_ast::Value::as_int_u32)
                                .map_err(error::Error::map_unexpected_value_to_external_error)?;
                        }
                        ModelInputAnnotation::ModelOrderByExpression => {
                            // order by is optional
                            order_by = argument.value.as_nullable(|v| {
                                build_ndc_order_by(
                                    v,
                                    &session.variables,
                                    usage_counts,
                                    &target_source.type_mappings,
                                    object_types,
                                    source_data_connector,
                                )
                            })?;
                        }
                        _ => {
                            return Err(error::InternalEngineError::UnexpectedAnnotation {
                                annotation: annotation.clone(),
                            })?
                        }
                    }
                }
                InputAnnotation::BooleanExpression(
                    BooleanExpressionAnnotation::BooleanExpressionRootField,
                ) => {
                    // where argument is optional
                    where_clause = argument.value.as_nullable(|v| {
                        filter::resolve_filter_expression(
                            v.as_object()?,
                            &target_source.data_connector,
                            &target_source.type_mappings,
                            object_types,
                            &session.variables,
                            usage_counts,
                        )
                    })?;
                }

                _ => {
                    return Err(error::InternalEngineError::UnexpectedAnnotation {
                        annotation: annotation.clone(),
                    })?
                }
            },

            annotation => {
                return Err(error::InternalEngineError::UnexpectedAnnotation {
                    annotation: annotation.clone(),
                })?
            }
        }
    }

    let query_filter = filter::QueryFilter {
        where_clause,
        additional_filter: None,
    };

    let selection_ir = model_selection_ir(
        &field.selection_set,
        &relationship_annotation.target_type,
        target_source,
        BTreeMap::new(),
        query_filter,
        permissions::get_select_filter_predicate(&field_call.info)?,
        limit,
        offset,
        order_by,
        models,
        commands,
        object_types,
        session,
        request_headers,
        usage_counts,
    )?;

    match metadata_resolve::field_selection_relationship_execution_category(
        relationship_field_nestedness,
        source_data_connector,
        &target_source.data_connector,
        target_capabilities,
    ) {
        metadata_resolve::RelationshipExecutionCategory::Local => {
            Ok(build_local_model_relationship(
                selection_ir,
                &relationship_annotation.relationship_name,
                &relationship_annotation.relationship_type,
                &relationship_annotation.source_type,
                source_data_connector,
                source_type_mappings,
                &relationship_annotation.target_model_name,
                &relationship_annotation.target_type,
                target_source,
                &relationship_annotation.mappings,
            ))
        }
        metadata_resolve::RelationshipExecutionCategory::RemoteForEach => {
            build_remote_relationship(
                selection_ir,
                &relationship_annotation.relationship_name,
                &relationship_annotation.source_type,
                source_type_mappings,
                &relationship_annotation.mappings,
                &target_source.argument_mappings,
            )
        }
    }
}

pub fn generate_model_aggregate_relationship_open_dd_ir<'s>(
    field: &Field<'s, GDS>,
    relationship_annotation: &'s ModelAggregateRelationshipAnnotation,
    models: &'s IndexMap<
        Qualified<open_dds::models::ModelName>,
        metadata_resolve::ModelWithPermissions,
    >,
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

pub fn generate_model_aggregate_relationship_ir<'s>(
    field: &Field<'s, GDS>,
    relationship_annotation: &'s ModelAggregateRelationshipAnnotation,
    relationship_field_nestedness: metadata_resolve::FieldNestedness,
    source_data_connector: &'s metadata_resolve::DataConnectorLink,
    source_type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    models: &'s IndexMap<
        Qualified<open_dds::models::ModelName>,
        metadata_resolve::ModelWithPermissions,
    >,
    object_types: &'s BTreeMap<
        Qualified<open_dds::types::CustomTypeName>,
        metadata_resolve::ObjectTypeWithRelationships,
    >,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    usage_counts: &mut UsagesCounts,
) -> Result<FieldSelection<'s>, error::Error> {
    // Add the target model being used in the usage counts
    count_model(&relationship_annotation.target_model_name, usage_counts);

    let field_call = field.field_call()?;

    let model = models
        .get(&relationship_annotation.target_model_name)
        .ok_or_else(|| {
            error::InternalError::Developer(
                error::InternalDeveloperError::TargetModelNotFoundForRelationship {
                    model_name: relationship_annotation.target_model_name.clone(),
                    relationship_name: relationship_annotation.relationship_name.clone(),
                },
            )
        })?;

    let (target_source, target_capabilities) = model
        .model
        .source
        .as_deref()
        .zip(relationship_annotation.target_capabilities.as_ref())
        .ok_or_else(|| match &field.selection_set.type_name {
            Some(type_name) => {
                error::Error::from(error::InternalDeveloperError::NoSourceDataConnector {
                    type_name: type_name.clone(),
                    field_name: field_call.name.clone(),
                })
            }
            None => error::Error::from(normalized_ast::Error::NoTypenameFound),
        })?;

    let selection_ir = model_selection::generate_aggregate_model_selection_ir(
        field,
        field_call,
        &relationship_annotation.target_type,
        model,
        target_source,
        &relationship_annotation.target_model_name,
        object_types,
        session,
        request_headers,
        usage_counts,
    )?;

    match metadata_resolve::field_selection_relationship_execution_category(
        relationship_field_nestedness,
        source_data_connector,
        &target_source.data_connector,
        target_capabilities,
    ) {
        metadata_resolve::RelationshipExecutionCategory::Local => {
            Ok(build_local_model_relationship(
                selection_ir,
                &relationship_annotation.relationship_name,
                &RelationshipType::Array,
                &relationship_annotation.source_type,
                source_data_connector,
                source_type_mappings,
                &relationship_annotation.target_model_name,
                &relationship_annotation.target_type,
                target_source,
                &relationship_annotation.mappings,
            ))
        }
        metadata_resolve::RelationshipExecutionCategory::RemoteForEach => {
            build_remote_relationship(
                selection_ir,
                &relationship_annotation.relationship_name,
                &relationship_annotation.source_type,
                source_type_mappings,
                &relationship_annotation.mappings,
                &target_source.argument_mappings,
            )
        }
    }
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
    usage_counts: &mut UsagesCounts,
) -> Result<open_dds::query::RelationshipSelection, error::Error> {
    count_command(&relationship_annotation.command_name, usage_counts);

    let selection = generate_selection_set_open_dd_ir(
        &field.selection_set,
        metadata_resolve::FieldNestedness::NotNested,
        models,
        type_mappings,
        object_types,
        session_variables,
        request_headers,
        usage_counts,
    )?;

    let target = open_dds::query::RelationshipTarget {
        relationship_name: relationship_annotation.relationship_name.clone(),
        arguments: IndexMap::new(),
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

pub fn generate_command_relationship_ir<'s>(
    field: &Field<'s, GDS>,
    annotation: &'s CommandRelationshipAnnotation,
    relationship_field_nestedness: metadata_resolve::FieldNestedness,
    source_data_connector: &'s metadata_resolve::DataConnectorLink,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    models: &'s IndexMap<
        metadata_resolve::Qualified<open_dds::models::ModelName>,
        metadata_resolve::ModelWithPermissions,
    >,
    commands: &'s IndexMap<
        metadata_resolve::Qualified<open_dds::commands::CommandName>,
        metadata_resolve::CommandWithPermissions,
    >,
    object_types: &'s BTreeMap<
        metadata_resolve::Qualified<open_dds::types::CustomTypeName>,
        metadata_resolve::ObjectTypeWithRelationships,
    >,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    usage_counts: &mut UsagesCounts,
) -> Result<FieldSelection<'s>, error::Error> {
    count_command(&annotation.command_name, usage_counts);
    let field_call = field.field_call()?;

    let target_command = commands.get(&annotation.command_name).ok_or_else(|| {
        error::InternalError::Developer(
            error::InternalDeveloperError::TargetCommandNotFoundForRelationship {
                command_name: annotation.command_name.clone(),
                relationship_name: annotation.relationship_name.clone(),
            },
        )
    })?;

    let target =
        annotation
            .target_source
            .as_ref()
            .ok_or_else(|| match &field.selection_set.type_name {
                Some(type_name) => {
                    error::Error::from(error::InternalDeveloperError::NoSourceDataConnector {
                        type_name: type_name.clone(),
                        field_name: field_call.name.clone(),
                    })
                }
                None => error::Error::from(normalized_ast::Error::NoTypenameFound),
            })?;

    let target_source = target_command.command.source.as_deref().ok_or_else(|| {
        match &field.selection_set.type_name {
            Some(type_name) => {
                error::Error::from(error::InternalDeveloperError::NoSourceDataConnector {
                    type_name: type_name.clone(),
                    field_name: field_call.name.clone(),
                })
            }
            None => error::Error::from(normalized_ast::Error::NoTypenameFound),
        }
    })?;

    match metadata_resolve::field_selection_relationship_execution_category(
        relationship_field_nestedness,
        source_data_connector,
        &target_source.data_connector,
        &target.capabilities,
    ) {
        metadata_resolve::RelationshipExecutionCategory::Local => build_local_command_relationship(
            field,
            field_call,
            annotation,
            type_mappings,
            target_command,
            &target.function_name,
            target_source,
            models,
            commands,
            object_types,
            session,
            request_headers,
            usage_counts,
        ),
        metadata_resolve::RelationshipExecutionCategory::RemoteForEach => {
            build_remote_command_relationship(
                field,
                field_call,
                annotation,
                type_mappings,
                target_command,
                &target.function_name,
                target_source,
                models,
                commands,
                object_types,
                session,
                request_headers,
                usage_counts,
            )
        }
    }
}

pub fn build_local_model_relationship<'s>(
    relationships_ir: model_selection::ModelSelection<'s>,
    relationship_name: &'s RelationshipName,
    relationship_type: &'s RelationshipType,
    source_type: &'s Qualified<CustomTypeName>,
    source_data_connector: &'s metadata_resolve::DataConnectorLink,
    source_type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    target_model_name: &'s Qualified<ModelName>,
    target_type: &'s Qualified<CustomTypeName>,
    target_source: &'s metadata_resolve::ModelSource,
    target_mappings: &'s Vec<RelationshipModelMapping>,
) -> FieldSelection<'s> {
    let rel_info = LocalModelRelationshipInfo {
        relationship_name,
        relationship_type,
        source_type,
        source_data_connector,
        source_type_mappings,
        target_model_name,
        target_source,
        target_type,
        mappings: target_mappings,
    };

    FieldSelection::ModelRelationshipLocal {
        query: relationships_ir,
        name: NdcRelationshipName::new(source_type, relationship_name),
        relationship_info: rel_info,
    }
}

pub fn build_local_command_relationship<'s>(
    field: &normalized_ast::Field<'s, GDS>,
    field_call: &normalized_ast::FieldCall<'s, GDS>,
    annotation: &'s CommandRelationshipAnnotation,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    target_command: &'s metadata_resolve::CommandWithPermissions,
    target_function_name: &'s FunctionName,
    target_source: &'s CommandSource,
    models: &'s IndexMap<
        metadata_resolve::Qualified<open_dds::models::ModelName>,
        metadata_resolve::ModelWithPermissions,
    >,
    commands: &'s IndexMap<
        metadata_resolve::Qualified<open_dds::commands::CommandName>,
        metadata_resolve::CommandWithPermissions,
    >,
    object_types: &'s BTreeMap<
        metadata_resolve::Qualified<open_dds::types::CustomTypeName>,
        metadata_resolve::ObjectTypeWithRelationships,
    >,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    usage_counts: &mut UsagesCounts,
) -> Result<FieldSelection<'s>, error::Error> {
    let relationships_ir = generate_function_based_command(
        &annotation.command_name,
        target_function_name,
        field,
        field_call,
        &annotation.target_type,
        annotation.target_base_type_kind,
        target_command,
        target_source,
        models,
        commands,
        object_types,
        session,
        request_headers,
        usage_counts,
    )?;

    let rel_info = LocalCommandRelationshipInfo {
        relationship_name: &annotation.relationship_name,
        source_type: &annotation.source_type,
        source_type_mappings: type_mappings,
        command_name: &annotation.command_name,
        argument_mappings: &target_source.argument_mappings,
        function_name: target_function_name,
        mappings: &annotation.mappings,
    };

    // Relationship names needs to be unique across the IR. This is so that, the
    // NDC can use these names to figure out what joins to use.
    // A single "source type" can have only one relationship with a given name,
    // hence the relationship name in the IR is a tuple between the source type
    // and the relationship name.
    // Relationship name = (source_type, relationship_name)

    Ok(FieldSelection::CommandRelationshipLocal {
        ir: relationships_ir,
        name: NdcRelationshipName::new(&annotation.source_type, &annotation.relationship_name),
        relationship_info: rel_info,
    })
}

pub fn build_remote_relationship<'s>(
    mut remote_relationships_ir: model_selection::ModelSelection<'s>,
    relationship_name: &'s RelationshipName,
    source_type: &'s Qualified<CustomTypeName>,
    source_type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    target_mappings: &'s Vec<RelationshipModelMapping>,
    target_argument_mappings: &'s BTreeMap<ArgumentName, DataConnectorArgumentName>,
) -> Result<FieldSelection<'s>, error::Error> {
    let mut join_mapping: Vec<(SourceField, TargetField)> = vec![];
    let mut relationship_join_filter_expressions = Vec::new();
    let mut variable_arguments = BTreeMap::new();

    for metadata_resolve::RelationshipModelMapping {
        source_field,
        target,
    } in target_mappings
    {
        let source_column = plan::get_relationship_field_mapping_of_field_name(
            source_type_mappings,
            source_type,
            relationship_name,
            &source_field.field_name,
        )
        .map_err(|err| {
            error::Error::from(error::InternalDeveloperError::RelationshipFieldMappingError(err))
        })?;

        let source_field = (source_field.field_name.clone(), source_column);
        match target {
            metadata_resolve::RelationshipModelMappingTarget::ModelField(
                metadata_resolve::RelationshipModelMappingFieldTarget {
                    target_field,
                    target_ndc_column,
                },
            ) => {
                let target_column = target_ndc_column.as_ref().ok_or_else(|| {
                    error::InternalEngineError::InternalGeneric {
                        description: format!(
                            "No column mapping for relationship {relationship_name} on {source_type}"
                        ),
                    }
                })?;

                let target_model_field =
                    TargetField::ModelField(target_field.field_name.clone(), target_column.clone());
                let target_value_variable = target_model_field.make_variable_name();
                join_mapping.push((source_field, target_model_field));

                // Generate the join condition expressions for the remote relationship
                let comparison_exp = LocalFieldComparison::BinaryComparison {
                    column: ComparisonTarget::Column {
                        name: target_column.column.clone(),
                        field_path: vec![],
                    },
                    operator: target_column.equal_operator.clone(),
                    value: ComparisonValue::Variable {
                        name: target_value_variable,
                    },
                };
                relationship_join_filter_expressions.push(Expression::LocalField(comparison_exp));
            }
            metadata_resolve::RelationshipModelMappingTarget::Argument(argument_name) => {
                let target_argument = TargetField::Argument(argument_name.clone());
                let target_value_variable = target_argument.make_variable_name();
                join_mapping.push((source_field, target_argument));

                let ndc_argument_name =
                    target_argument_mappings.get(argument_name).ok_or_else(|| {
                        error::InternalDeveloperError::ArgumentMappingNotFoundForRelationship {
                            relationship_name: relationship_name.clone(),
                            argument_name: argument_name.clone(),
                        }
                    })?;

                variable_arguments.insert(ndc_argument_name.clone(), target_value_variable);
            }
        }
    }

    if !relationship_join_filter_expressions.is_empty() {
        remote_relationships_ir
            .filter_clause
            .relationship_join_filter =
            Some(Expression::mk_and(relationship_join_filter_expressions));
    }
    remote_relationships_ir.variable_arguments = variable_arguments;

    let rel_info = RemoteModelRelationshipInfo { join_mapping };
    Ok(FieldSelection::ModelRelationshipRemote {
        ir: remote_relationships_ir,
        relationship_info: rel_info,
    })
}

pub fn build_remote_command_relationship<'n, 's>(
    field: &'n normalized_ast::Field<'s, GDS>,
    field_call: &'n normalized_ast::FieldCall<'s, GDS>,
    annotation: &'s CommandRelationshipAnnotation,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    target_command: &'s metadata_resolve::CommandWithPermissions,
    target_function_name: &'s FunctionName,
    target_source: &'s CommandSource,
    models: &'s IndexMap<
        metadata_resolve::Qualified<open_dds::models::ModelName>,
        metadata_resolve::ModelWithPermissions,
    >,
    commands: &'s IndexMap<
        metadata_resolve::Qualified<open_dds::commands::CommandName>,
        metadata_resolve::CommandWithPermissions,
    >,
    object_types: &'s BTreeMap<
        metadata_resolve::Qualified<open_dds::types::CustomTypeName>,
        metadata_resolve::ObjectTypeWithRelationships,
    >,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    usage_counts: &mut UsagesCounts,
) -> Result<FieldSelection<'s>, error::Error> {
    let mut join_mapping: Vec<(SourceField, ArgumentName)> = vec![];
    for metadata_resolve::RelationshipCommandMapping {
        source_field: source_field_path,
        argument_name: target_argument_name,
    } in &annotation.mappings
    {
        let source_column = plan::get_relationship_field_mapping_of_field_name(
            type_mappings,
            &annotation.source_type,
            &annotation.relationship_name,
            &source_field_path.field_name,
        )
        .map_err(|err| {
            error::Error::from(error::InternalDeveloperError::RelationshipFieldMappingError(err))
        })?;

        let source_field = (source_field_path.field_name.clone(), source_column);
        join_mapping.push((source_field, target_argument_name.clone()));
    }
    let mut remote_relationships_ir = generate_function_based_command(
        &annotation.command_name,
        target_function_name,
        field,
        field_call,
        &annotation.target_type,
        annotation.target_base_type_kind,
        target_command,
        target_source,
        models,
        commands,
        object_types,
        session,
        request_headers,
        usage_counts,
    )?;

    // Add the arguments on which the join is done to the command arguments
    let mut variable_arguments = BTreeMap::new();
    for (_source, target_argument_name) in &join_mapping {
        let target_value_variable = mk_argument_target_variable_name(target_argument_name);
        let ndc_argument_name = target_source
            .argument_mappings
            .get(target_argument_name)
            .ok_or_else(|| {
                error::InternalDeveloperError::ArgumentMappingNotFoundForRelationship {
                    relationship_name: annotation.relationship_name.clone(),
                    argument_name: target_argument_name.clone(),
                }
            })?;

        variable_arguments.insert(ndc_argument_name.clone(), target_value_variable);
    }
    remote_relationships_ir.variable_arguments = variable_arguments;

    let rel_info = RemoteCommandRelationshipInfo {
        annotation,
        join_mapping,
    };
    Ok(FieldSelection::CommandRelationshipRemote {
        ir: remote_relationships_ir,
        relationship_info: rel_info,
    })
}
