use std::collections::BTreeMap;

use hasura_authn_core::SessionVariables;
use lang_graphql::normalized_ast::{self, Field};
use open_dds::{
    arguments::ArgumentName,
    relationships::{RelationshipName, RelationshipType},
    types::{CustomTypeName, FieldName},
};

use ndc_models;
use serde::Serialize;

use super::permissions;
use super::selection_set::FieldSelection;
use super::{
    commands::generate_function_based_command, filter::resolve_filter_expression,
    filter::ResolvedFilterExpression, model_selection::model_selection_ir,
};
use super::{
    order_by::{build_ndc_order_by, ResolvedOrderBy},
    selection_set::NDCRelationshipName,
};

use crate::model_tracking::{count_model, UsagesCounts};
use crate::{ir::error, model_tracking::count_command};
use metadata_resolve;
use metadata_resolve::{serialize_qualified_btreemap, Qualified};
use schema::ModelRelationshipAnnotation;
use schema::{Annotation, BooleanExpressionAnnotation, InputAnnotation, ModelInputAnnotation, GDS};
use schema::{CommandRelationshipAnnotation, CommandTargetSource};

#[derive(Debug, Serialize)]
pub(crate) struct LocalModelRelationshipInfo<'s> {
    pub relationship_name: &'s RelationshipName,
    pub relationship_type: &'s RelationshipType,
    pub source_type: &'s Qualified<CustomTypeName>,
    pub source_data_connector: &'s metadata_resolve::DataConnectorLink,
    #[serde(serialize_with = "serialize_qualified_btreemap")]
    pub source_type_mappings:
        &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    pub target_source: &'s metadata_resolve::ModelTargetSource,
    pub target_type: &'s Qualified<CustomTypeName>,
    pub mappings: &'s Vec<metadata_resolve::RelationshipModelMapping>,
}

#[derive(Debug, Serialize)]
pub(crate) struct LocalCommandRelationshipInfo<'s> {
    pub annotation: &'s CommandRelationshipAnnotation,
    pub source_data_connector: &'s metadata_resolve::DataConnectorLink,
    #[serde(serialize_with = "serialize_qualified_btreemap")]
    pub source_type_mappings:
        &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    pub target_source: &'s CommandTargetSource,
}

#[derive(Debug, Clone, Serialize)]
pub struct RemoteModelRelationshipInfo<'s> {
    pub annotation: &'s ModelRelationshipAnnotation,
    /// This contains processed information about the mappings.
    /// `RelationshipMapping` only contains mapping of field names. This
    /// contains mapping of field names and `metadata_resolve::FieldMapping`.
    /// Also see `build_remote_relationship`.
    pub join_mapping: Vec<(SourceField, TargetField)>,
}

#[derive(Debug, Serialize)]
pub(crate) struct RemoteCommandRelationshipInfo<'s> {
    pub annotation: &'s CommandRelationshipAnnotation,
    pub join_mapping: Vec<(SourceField, ArgumentName)>,
}

pub type SourceField = (FieldName, metadata_resolve::FieldMapping);
pub type TargetField = (FieldName, metadata_resolve::NdcColumnForComparison);

pub(crate) fn generate_model_relationship_ir<'s>(
    field: &Field<'s, GDS>,
    annotation: &'s ModelRelationshipAnnotation,
    source_data_connector: &'s metadata_resolve::DataConnectorLink,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    session_variables: &SessionVariables,
    usage_counts: &mut UsagesCounts,
) -> Result<FieldSelection<'s>, error::Error> {
    // Add the target model being used in the usage counts
    count_model(&annotation.model_name, usage_counts);
    let field_call = field.field_call()?;

    let mut limit = None;
    let mut offset = None;
    let mut filter_clause = ResolvedFilterExpression {
        expression: None,
        relationships: BTreeMap::new(),
    };
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
                                )?)
                            }
                            ModelInputAnnotation::ModelOffsetArgument => {
                                offset = Some(argument.value.as_int_u32().map_err(
                                    error::Error::map_unexpected_value_to_external_error,
                                )?)
                            }
                            ModelInputAnnotation::ModelOrderByExpression => {
                                order_by = Some(build_ndc_order_by(argument, usage_counts)?)
                            }
                            _ => {
                                return Err(error::InternalEngineError::UnexpectedAnnotation {
                                    annotation: annotation.clone(),
                                })?
                            }
                        }
                    }
                    InputAnnotation::BooleanExpression(
                        BooleanExpressionAnnotation::BooleanExpression,
                    ) => {
                        filter_clause =
                            resolve_filter_expression(argument.value.as_object()?, usage_counts)?
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
        source_data_connector,
        &target_source.model.data_connector,
        &target_source.capabilities,
    ) {
        metadata_resolve::RelationshipExecutionCategory::Local => build_local_model_relationship(
            field,
            field_call,
            annotation,
            source_data_connector,
            type_mappings,
            target_source,
            filter_clause,
            limit,
            offset,
            order_by,
            session_variables,
            usage_counts,
        ),
        metadata_resolve::RelationshipExecutionCategory::RemoteForEach => {
            build_remote_relationship(
                field,
                field_call,
                annotation,
                type_mappings,
                target_source,
                filter_clause,
                limit,
                offset,
                order_by,
                session_variables,
                usage_counts,
            )
        }
    }
}

pub(crate) fn generate_command_relationship_ir<'s>(
    field: &Field<'s, GDS>,
    annotation: &'s CommandRelationshipAnnotation,
    source_data_connector: &'s metadata_resolve::DataConnectorLink,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    session_variables: &SessionVariables,
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
                usage_counts,
            )
        }
    }
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn build_local_model_relationship<'s>(
    field: &normalized_ast::Field<'s, GDS>,
    field_call: &normalized_ast::FieldCall<'s, GDS>,
    annotation: &'s ModelRelationshipAnnotation,
    data_connector: &'s metadata_resolve::DataConnectorLink,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    target_source: &'s metadata_resolve::ModelTargetSource,
    filter_clause: ResolvedFilterExpression<'s>,
    limit: Option<u32>,
    offset: Option<u32>,
    order_by: Option<ResolvedOrderBy<'s>>,
    session_variables: &SessionVariables,
    usage_counts: &mut UsagesCounts,
) -> Result<FieldSelection<'s>, error::Error> {
    let relationships_ir = model_selection_ir(
        &field.selection_set,
        &annotation.target_type,
        &target_source.model,
        BTreeMap::new(),
        filter_clause,
        permissions::get_select_filter_predicate(field_call)?,
        limit,
        offset,
        order_by,
        session_variables,
        usage_counts,
    )?;
    let rel_info = LocalModelRelationshipInfo {
        relationship_name: &annotation.relationship_name,
        relationship_type: &annotation.relationship_type,
        source_type: &annotation.source_type,
        source_data_connector: data_connector,
        source_type_mappings: type_mappings,
        target_source,
        target_type: &annotation.target_type,
        mappings: &annotation.mappings,
    };

    Ok(FieldSelection::ModelRelationshipLocal {
        query: relationships_ir,
        name: NDCRelationshipName::new(&annotation.source_type, &annotation.relationship_name)?,
        relationship_info: rel_info,
    })
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn build_local_command_relationship<'s>(
    field: &normalized_ast::Field<'s, GDS>,
    field_call: &normalized_ast::FieldCall<'s, GDS>,
    annotation: &'s CommandRelationshipAnnotation,
    data_connector: &'s metadata_resolve::DataConnectorLink,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    target_source: &'s CommandTargetSource,
    session_variables: &SessionVariables,
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
        name: NDCRelationshipName::new(&annotation.source_type, &annotation.relationship_name)?,
        relationship_info: rel_info,
    })
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn build_remote_relationship<'n, 's>(
    field: &'n normalized_ast::Field<'s, GDS>,
    field_call: &'n normalized_ast::FieldCall<'s, GDS>,
    annotation: &'s ModelRelationshipAnnotation,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    target_source: &'s metadata_resolve::ModelTargetSource,
    filter_clause: ResolvedFilterExpression<'s>,
    limit: Option<u32>,
    offset: Option<u32>,
    order_by: Option<ResolvedOrderBy<'s>>,
    session_variables: &SessionVariables,
    usage_counts: &mut UsagesCounts,
) -> Result<FieldSelection<'s>, error::Error> {
    let mut join_mapping: Vec<(SourceField, TargetField)> = vec![];
    for metadata_resolve::RelationshipModelMapping {
        source_field: source_field_path,
        target_field: target_field_path,
        target_ndc_column,
    } in annotation.mappings.iter()
    {
        let source_column = get_field_mapping_of_field_name(
            type_mappings,
            &annotation.source_type,
            &annotation.relationship_name,
            &source_field_path.field_name,
        )?;
        let target_column = target_ndc_column.as_ref().ok_or_else(|| {
            error::InternalEngineError::InternalGeneric {
                description: format!(
                    "No column mapping for relationship {} on {}",
                    annotation.relationship_name, annotation.source_type
                ),
            }
        })?;

        let source_field = (source_field_path.field_name.clone(), source_column);
        let target_field = (target_field_path.field_name.clone(), target_column.clone());
        join_mapping.push((source_field, target_field));
    }
    let mut remote_relationships_ir = model_selection_ir(
        &field.selection_set,
        &annotation.target_type,
        &target_source.model,
        BTreeMap::new(),
        filter_clause,
        permissions::get_select_filter_predicate(field_call)?,
        limit,
        offset,
        order_by,
        session_variables,
        usage_counts,
    )?;

    // modify `ModelSelection` to include the join condition in `where` with a variable
    for (_source, (_field_name, target_column)) in &join_mapping {
        let target_value_variable = format!("${}", &target_column.column);
        let comparison_exp = ndc_models::Expression::BinaryComparisonOperator {
            column: ndc_models::ComparisonTarget::Column {
                name: target_column.column.clone(),
                path: vec![],
            },
            operator: target_column.equal_operator.clone(),
            value: ndc_models::ComparisonValue::Variable {
                name: target_value_variable,
            },
        };
        remote_relationships_ir.filter_clause.expression =
            match remote_relationships_ir.filter_clause.expression {
                Some(existing) => Some(ndc_models::Expression::And {
                    expressions: vec![existing, comparison_exp],
                }),
                None => Some(comparison_exp),
            };
    }
    let rel_info = RemoteModelRelationshipInfo {
        annotation,
        join_mapping,
    };
    Ok(FieldSelection::ModelRelationshipRemote {
        ir: remote_relationships_ir,
        relationship_info: rel_info,
    })
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn build_remote_command_relationship<'n, 's>(
    field: &'n normalized_ast::Field<'s, GDS>,
    field_call: &'n normalized_ast::FieldCall<'s, GDS>,
    annotation: &'s CommandRelationshipAnnotation,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    target_source: &'s CommandTargetSource,
    session_variables: &SessionVariables,
    usage_counts: &mut UsagesCounts,
) -> Result<FieldSelection<'s>, error::Error> {
    let mut join_mapping: Vec<(SourceField, ArgumentName)> = vec![];
    for metadata_resolve::RelationshipCommandMapping {
        source_field: source_field_path,
        argument_name: target_argument_name,
    } in annotation.mappings.iter()
    {
        let source_column = get_field_mapping_of_field_name(
            type_mappings,
            &annotation.source_type,
            &annotation.relationship_name,
            &source_field_path.field_name,
        )?;

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
        usage_counts,
    )?;

    // Add the arguments on which the join is done to the command arguments
    let mut variable_arguments = BTreeMap::new();
    for (_source, target_argument_name) in &join_mapping {
        let target_value_variable = format!("${}", target_argument_name);
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

pub(crate) fn get_field_mapping_of_field_name(
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    type_name: &Qualified<CustomTypeName>,
    relationship_name: &RelationshipName,
    field_name: &FieldName,
) -> Result<metadata_resolve::FieldMapping, error::Error> {
    let type_mapping = type_mappings.get(type_name).ok_or_else(|| {
        error::InternalDeveloperError::TypeMappingNotFoundForRelationship {
            type_name: type_name.clone(),
            relationship_name: relationship_name.clone(),
        }
    })?;
    match type_mapping {
        metadata_resolve::TypeMapping::Object { field_mappings, .. } => Ok(field_mappings
            .get(field_name)
            .ok_or_else(
                || error::InternalDeveloperError::FieldMappingNotFoundForRelationship {
                    type_name: type_name.clone(),
                    relationship_name: relationship_name.clone(),
                    field_name: field_name.clone(),
                },
            )?
            .clone()),
    }
}
