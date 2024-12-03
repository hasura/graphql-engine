use std::collections::BTreeMap;

use hasura_authn_core::SessionVariables;
use lang_graphql::normalized_ast::{self, Field};
use open_dds::{
    arguments::ArgumentName,
    relationships::{RelationshipName, RelationshipType},
    types::{CustomTypeName, FieldName},
};

use serde::Serialize;

use super::{
    commands::generate_function_based_command,
    filter,
    model_selection::{self, model_selection_ir},
    order_by::build_ndc_order_by,
    permissions,
    selection_set::FieldSelection,
};

use crate::model_tracking::count_model;
use crate::{error, model_tracking::count_command};
use graphql_schema::{
    Annotation, BooleanExpressionAnnotation, CommandRelationshipAnnotation, CommandTargetSource,
    InputAnnotation, ModelAggregateRelationshipAnnotation, ModelInputAnnotation,
    ModelRelationshipAnnotation, GDS,
};
use metadata_resolve::{self, serialize_qualified_btreemap, Qualified, RelationshipModelMapping};
use plan_types::{
    ComparisonTarget, ComparisonValue, Expression, LocalFieldComparison,
    LocalModelRelationshipInfo, NdcRelationshipName, UsagesCounts, VariableName,
};

#[derive(Debug, Serialize)]
pub struct LocalCommandRelationshipInfo<'s> {
    pub annotation: &'s CommandRelationshipAnnotation,
    pub source_data_connector: &'s metadata_resolve::DataConnectorLink,
    #[serde(serialize_with = "serialize_qualified_btreemap")]
    pub source_type_mappings:
        &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,

    pub target_source: &'s CommandTargetSource,
}

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
pub type TargetField = (FieldName, metadata_resolve::NdcColumnForComparison);

pub fn generate_model_relationship_ir<'s>(
    field: &Field<'s, GDS>,
    relationship_annotation: &'s ModelRelationshipAnnotation,
    relationship_field_nestedness: metadata_resolve::FieldNestedness,
    source_data_connector: &'s metadata_resolve::DataConnectorLink,
    source_type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    session_variables: &SessionVariables,
    request_headers: &reqwest::header::HeaderMap,
    usage_counts: &mut UsagesCounts,
) -> Result<FieldSelection<'s>, error::Error> {
    // Add the target model being used in the usage counts
    count_model(&relationship_annotation.model_name, usage_counts);
    let field_call = field.field_call()?;

    let mut limit = None;
    let mut offset = None;
    let mut where_clause = None;
    let mut order_by = None;

    for argument in field_call.arguments.values() {
        match argument.info.generic {
            annotation @ Annotation::Input(argument_annotation) => {
                match argument_annotation {
                    InputAnnotation::Model(model_argument_annotation) => {
                        match model_argument_annotation {
                            ModelInputAnnotation::ModelLimitArgument => {
                                limit = Some(argument.value.as_int_u32().map_err(
                                    error::Error::map_unexpected_value_to_external_error,
                                )?);
                            }
                            ModelInputAnnotation::ModelOffsetArgument => {
                                offset = Some(argument.value.as_int_u32().map_err(
                                    error::Error::map_unexpected_value_to_external_error,
                                )?);
                            }
                            ModelInputAnnotation::ModelOrderByExpression => {
                                order_by = Some(build_ndc_order_by(
                                    argument,
                                    session_variables,
                                    usage_counts,
                                )?);
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
                        if let Some(model_source) = &relationship_annotation.target_source {
                            where_clause = Some(filter::resolve_filter_expression(
                                argument.value.as_object()?,
                                &model_source.model.data_connector,
                                &model_source.model.type_mappings,
                                session_variables,
                                usage_counts,
                            )?);
                        }
                    }

                    _ => {
                        return Err(error::InternalEngineError::UnexpectedAnnotation {
                            annotation: annotation.clone(),
                        })?
                    }
                }
            }

            annotation => {
                return Err(error::InternalEngineError::UnexpectedAnnotation {
                    annotation: annotation.clone(),
                })?
            }
        }
    }
    let target_source =
        relationship_annotation
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

    let query_filter = filter::QueryFilter {
        where_clause,
        additional_filter: None,
    };

    let selection_ir = model_selection_ir(
        &field.selection_set,
        &relationship_annotation.target_type,
        &target_source.model,
        BTreeMap::new(),
        query_filter,
        permissions::get_select_filter_predicate(&field_call.info)?,
        limit,
        offset,
        order_by,
        session_variables,
        request_headers,
        usage_counts,
    )?;

    match metadata_resolve::relationship_execution_category(
        relationship_field_nestedness,
        source_data_connector,
        &target_source.model.data_connector,
        &target_source.capabilities,
    ) {
        metadata_resolve::RelationshipExecutionCategory::Local => {
            Ok(build_local_model_relationship(
                selection_ir,
                &relationship_annotation.relationship_name,
                &relationship_annotation.relationship_type,
                &relationship_annotation.source_type,
                source_data_connector,
                source_type_mappings,
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
            )
        }
    }
}

pub fn generate_model_aggregate_relationship_ir<'s>(
    field: &Field<'s, GDS>,
    relationship_annotation: &'s ModelAggregateRelationshipAnnotation,
    relationship_field_nestedness: metadata_resolve::FieldNestedness,
    source_data_connector: &'s metadata_resolve::DataConnectorLink,
    source_type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    session_variables: &SessionVariables,
    request_headers: &reqwest::header::HeaderMap,
    usage_counts: &mut UsagesCounts,
) -> Result<FieldSelection<'s>, error::Error> {
    let field_call = field.field_call()?;

    let target_source =
        relationship_annotation
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

    let selection_ir = model_selection::generate_aggregate_model_selection_ir(
        field,
        field_call,
        &relationship_annotation.target_type,
        &target_source.model,
        &relationship_annotation.model_name,
        session_variables,
        request_headers,
        usage_counts,
    )?;

    match metadata_resolve::relationship_execution_category(
        relationship_field_nestedness,
        source_data_connector,
        &target_source.model.data_connector,
        &target_source.capabilities,
    ) {
        metadata_resolve::RelationshipExecutionCategory::Local => {
            Ok(build_local_model_relationship(
                selection_ir,
                &relationship_annotation.relationship_name,
                &RelationshipType::Array,
                &relationship_annotation.source_type,
                source_data_connector,
                source_type_mappings,
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
            )
        }
    }
}

pub fn generate_command_relationship_ir<'s>(
    field: &Field<'s, GDS>,
    annotation: &'s CommandRelationshipAnnotation,
    relationship_field_nestedness: metadata_resolve::FieldNestedness,
    source_data_connector: &'s metadata_resolve::DataConnectorLink,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    session_variables: &SessionVariables,
    request_headers: &reqwest::header::HeaderMap,
    usage_counts: &mut UsagesCounts,
) -> Result<FieldSelection<'s>, error::Error> {
    count_command(&annotation.command_name, usage_counts);
    let field_call = field.field_call()?;

    let target_source =
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

    match metadata_resolve::relationship_execution_category(
        relationship_field_nestedness,
        source_data_connector,
        &target_source.details.data_connector,
        &target_source.capabilities,
    ) {
        metadata_resolve::RelationshipExecutionCategory::Local => build_local_command_relationship(
            field,
            field_call,
            annotation,
            source_data_connector,
            type_mappings,
            target_source,
            session_variables,
            request_headers,
            usage_counts,
        ),
        metadata_resolve::RelationshipExecutionCategory::RemoteForEach => {
            build_remote_command_relationship(
                field,
                field_call,
                annotation,
                type_mappings,
                target_source,
                session_variables,
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
    target_type: &'s Qualified<CustomTypeName>,
    target_source: &'s metadata_resolve::ModelTargetSource,
    target_mappings: &'s Vec<RelationshipModelMapping>,
) -> FieldSelection<'s> {
    let rel_info = LocalModelRelationshipInfo {
        relationship_name,
        relationship_type,
        source_type,
        source_data_connector,
        source_type_mappings,
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
    data_connector: &'s metadata_resolve::DataConnectorLink,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    target_source: &'s CommandTargetSource,
    session_variables: &SessionVariables,
    request_headers: &reqwest::header::HeaderMap,
    usage_counts: &mut UsagesCounts,
) -> Result<FieldSelection<'s>, error::Error> {
    let relationships_ir = generate_function_based_command(
        &annotation.command_name,
        &target_source.function_name,
        field,
        field_call,
        &annotation.target_type,
        annotation.target_base_type_kind,
        &target_source.details,
        session_variables,
        request_headers,
        usage_counts,
    )?;

    let rel_info = LocalCommandRelationshipInfo {
        annotation,
        source_data_connector: data_connector,
        source_type_mappings: type_mappings,
        target_source,
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
) -> Result<FieldSelection<'s>, error::Error> {
    let mut join_mapping: Vec<(SourceField, TargetField)> = vec![];
    for metadata_resolve::RelationshipModelMapping {
        source_field: source_field_path,
        target_field: target_field_path,
        target_ndc_column,
    } in target_mappings
    {
        let source_column = metadata_resolve::get_field_mapping_of_field_name(
            source_type_mappings,
            source_type,
            relationship_name,
            &source_field_path.field_name,
        )
        .map_err(|err| {
            error::Error::from(error::InternalDeveloperError::RelationshipFieldMappingError(err))
        })?;

        let target_column = target_ndc_column.as_ref().ok_or_else(|| {
            error::InternalEngineError::InternalGeneric {
                description: format!(
                    "No column mapping for relationship {relationship_name} on {source_type}"
                ),
            }
        })?;

        let source_field = (source_field_path.field_name.clone(), source_column);
        let target_field = (target_field_path.field_name.clone(), target_column.clone());
        join_mapping.push((source_field, target_field));
    }

    let mut relationship_join_filter_expressions = Vec::new();

    // Generate the join condition expressions for the remote relationship
    for (_source, (_field_name, target_column)) in &join_mapping {
        let target_value_variable = format!("${}", &target_column.column);
        let comparison_exp = LocalFieldComparison::BinaryComparison {
            column: ComparisonTarget::Column {
                name: target_column.column.clone(),
                field_path: vec![],
            },
            operator: target_column.equal_operator.clone(),
            value: ComparisonValue::Variable {
                name: VariableName(target_value_variable),
            },
        };
        relationship_join_filter_expressions.push(Expression::LocalField(comparison_exp));
    }

    remote_relationships_ir
        .filter_clause
        .relationship_join_filter = Some(Expression::mk_and(relationship_join_filter_expressions));

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
    target_source: &'s CommandTargetSource,
    session_variables: &SessionVariables,
    request_headers: &reqwest::header::HeaderMap,
    usage_counts: &mut UsagesCounts,
) -> Result<FieldSelection<'s>, error::Error> {
    let mut join_mapping: Vec<(SourceField, ArgumentName)> = vec![];
    for metadata_resolve::RelationshipCommandMapping {
        source_field: source_field_path,
        argument_name: target_argument_name,
    } in &annotation.mappings
    {
        let source_column = metadata_resolve::get_field_mapping_of_field_name(
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
        &target_source.function_name,
        field,
        field_call,
        &annotation.target_type,
        annotation.target_base_type_kind,
        &target_source.details,
        session_variables,
        request_headers,
        usage_counts,
    )?;

    // Add the arguments on which the join is done to the command arguments
    let mut variable_arguments = BTreeMap::new();
    for (_source, target_argument_name) in &join_mapping {
        let target_value_variable = format!("${target_argument_name}");
        let ndc_argument_name = target_source
            .details
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
