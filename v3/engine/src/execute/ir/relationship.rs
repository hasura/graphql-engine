use std::collections::BTreeMap;

use hasura_authn_core::SessionVariables;
use lang_graphql::normalized_ast::{self, Field};
use open_dds::{
    arguments::ArgumentName,
    relationships::{RelationshipName, RelationshipType},
    types::{CustomTypeName, FieldName},
};

use ndc_client as ndc;
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

use crate::execute::model_tracking::{count_model, UsagesCounts};
use crate::metadata::resolved::{
    relationship::{relationship_execution_category, RelationshipExecutionCategory},
    subgraph::serialize_qualified_btreemap,
};
use crate::schema::types::output_type::relationship::{
    ModelRelationshipAnnotation, ModelTargetSource,
};
use crate::{
    execute::{error, model_tracking::count_command},
    schema::types::output_type::relationship::{
        CommandRelationshipAnnotation, CommandTargetSource,
    },
};
use crate::{
    metadata::resolved::{self, subgraph::Qualified},
    schema::{
        types::{Annotation, InputAnnotation, ModelInputAnnotation},
        GDS,
    },
};

#[derive(Debug, Serialize)]
pub(crate) struct LocalModelRelationshipInfo<'s> {
    pub relationship_name: &'s RelationshipName,
    pub relationship_type: &'s RelationshipType,
    pub source_type: &'s Qualified<CustomTypeName>,
    pub source_data_connector: &'s resolved::data_connector::DataConnectorLink,
    #[serde(serialize_with = "serialize_qualified_btreemap")]
    pub source_type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, resolved::types::TypeMapping>,
    pub target_source: &'s ModelTargetSource,
    pub target_type: &'s Qualified<CustomTypeName>,
    pub mappings: &'s Vec<resolved::relationship::RelationshipModelMapping>,
}

#[derive(Debug, Serialize)]
pub(crate) struct LocalCommandRelationshipInfo<'s> {
    pub annotation: &'s CommandRelationshipAnnotation,
    pub source_data_connector: &'s resolved::data_connector::DataConnectorLink,
    #[serde(serialize_with = "serialize_qualified_btreemap")]
    pub source_type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, resolved::types::TypeMapping>,
    pub target_source: &'s CommandTargetSource,
}

#[derive(Debug, Clone, Serialize)]
pub struct RemoteModelRelationshipInfo<'s> {
    pub annotation: &'s ModelRelationshipAnnotation,
    /// This contains processed information about the mappings.
    /// `RelationshipMapping` only contains mapping of field names. This
    /// contains mapping of field names and `resolved::types::FieldMapping`.
    /// Also see `build_remote_relationship`.
    pub join_mapping: Vec<(SourceField, TargetField)>,
}

#[derive(Debug, Serialize)]
pub(crate) struct RemoteCommandRelationshipInfo<'s> {
    pub annotation: &'s CommandRelationshipAnnotation,
    pub join_mapping: Vec<(SourceField, ArgumentName)>,
}

pub type SourceField = (FieldName, resolved::types::FieldMapping);
pub type TargetField = (FieldName, resolved::types::FieldMapping);

pub(crate) fn process_model_relationship_definition(
    relationship_info: &LocalModelRelationshipInfo,
) -> Result<ndc::models::Relationship, error::Error> {
    let &LocalModelRelationshipInfo {
        relationship_name,
        relationship_type,
        source_type,
        source_data_connector,
        source_type_mappings,
        target_source,

        target_type,
        mappings,
    } = relationship_info;

    let mut column_mapping = BTreeMap::new();
    for resolved::relationship::RelationshipModelMapping {
        source_field: source_field_path,
        target_field: target_field_path,
    } in mappings.iter()
    {
        if !matches!(
            relationship_execution_category(
                source_data_connector,
                &target_source.model.data_connector,
                &target_source.capabilities
            ),
            RelationshipExecutionCategory::Local
        ) {
            Err(error::InternalEngineError::RemoteRelationshipsAreNotSupported)?
        } else {
            let source_column = get_field_mapping_of_field_name(
                source_type_mappings,
                source_type,
                relationship_name,
                &source_field_path.field_name,
            )?;
            let target_column = get_field_mapping_of_field_name(
                &target_source.model.type_mappings,
                target_type,
                relationship_name,
                &target_field_path.field_name,
            )?;

            if column_mapping
                .insert(source_column.column, target_column.column)
                .is_some()
            {
                Err(error::InternalEngineError::MappingExistsInRelationship {
                    source_column: source_field_path.field_name.clone(),
                    relationship_name: relationship_name.clone(),
                })?
            }
        }
    }
    let ndc_relationship = ndc_client::models::Relationship {
        column_mapping,
        relationship_type: {
            match relationship_type {
                RelationshipType::Object => ndc_client::models::RelationshipType::Object,
                RelationshipType::Array => ndc_client::models::RelationshipType::Array,
            }
        },
        target_collection: target_source.model.collection.to_string(),
        arguments: BTreeMap::new(),
    };
    Ok(ndc_relationship)
}

pub(crate) fn process_command_relationship_definition(
    relationship_info: &LocalCommandRelationshipInfo,
) -> Result<ndc::models::Relationship, error::Error> {
    let &LocalCommandRelationshipInfo {
        annotation,
        source_data_connector,
        source_type_mappings,
        target_source,
    } = relationship_info;

    let mut arguments = BTreeMap::new();
    for resolved::relationship::RelationshipCommandMapping {
        source_field: source_field_path,
        argument_name: target_argument,
    } in annotation.mappings.iter()
    {
        if !matches!(
            relationship_execution_category(
                source_data_connector,
                &target_source.details.data_connector,
                &target_source.capabilities
            ),
            RelationshipExecutionCategory::Local
        ) {
            Err(error::InternalEngineError::RemoteRelationshipsAreNotSupported)?
        } else {
            let source_column = get_field_mapping_of_field_name(
                source_type_mappings,
                &annotation.source_type,
                &annotation.relationship_name,
                &source_field_path.field_name,
            )?;

            let relationship_argument = ndc::models::RelationshipArgument::Column {
                name: source_column.column,
            };

            if arguments
                .insert(target_argument.to_string(), relationship_argument)
                .is_some()
            {
                Err(error::InternalEngineError::MappingExistsInRelationship {
                    source_column: source_field_path.field_name.clone(),
                    relationship_name: annotation.relationship_name.clone(),
                })?
            }
        }
    }

    let ndc_relationship = ndc_client::models::Relationship {
        column_mapping: BTreeMap::new(),
        relationship_type: ndc_client::models::RelationshipType::Object,
        target_collection: target_source.function_name.to_string(),
        arguments,
    };
    Ok(ndc_relationship)
}

pub(crate) fn generate_model_relationship_ir<'s>(
    field: &Field<'s, GDS>,
    annotation: &'s ModelRelationshipAnnotation,
    source_data_connector: &'s resolved::data_connector::DataConnectorLink,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, resolved::types::TypeMapping>,
    session_variables: &SessionVariables,
    usage_counts: &mut UsagesCounts,
) -> Result<FieldSelection<'s>, error::Error> {
    // Add the target model being used in the usage counts
    count_model(annotation.model_name.clone(), usage_counts);
    let field_call = field.field_call()?;

    let mut limit = None;
    let mut offset = None;
    let mut filter_clause = ResolvedFilterExpression {
        expressions: Vec::new(),
        relationships: BTreeMap::new(),
    };
    let mut order_by = None;

    for argument in field_call.arguments.values() {
        match argument.info.generic {
            annotation @ Annotation::Input(argument_annotation) => match argument_annotation {
                InputAnnotation::Model(model_argument_annotation) => {
                    match model_argument_annotation {
                        ModelInputAnnotation::ModelLimitArgument => {
                            limit = Some(argument.value.as_int_u32()?)
                        }
                        ModelInputAnnotation::ModelOffsetArgument => {
                            offset = Some(argument.value.as_int_u32()?)
                        }
                        ModelInputAnnotation::ModelFilterExpression => {
                            filter_clause = resolve_filter_expression(
                                argument.value.as_object()?,
                                usage_counts,
                            )?
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
    match relationship_execution_category(
        source_data_connector,
        &target_source.model.data_connector,
        &target_source.capabilities,
    ) {
        RelationshipExecutionCategory::Local => build_local_model_relationship(
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
        RelationshipExecutionCategory::RemoteForEach => build_remote_relationship(
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
        ),
    }
}

pub(crate) fn generate_command_relationship_ir<'s>(
    field: &Field<'s, GDS>,
    annotation: &'s CommandRelationshipAnnotation,
    source_data_connector: &'s resolved::data_connector::DataConnectorLink,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, resolved::types::TypeMapping>,
    session_variables: &SessionVariables,
    usage_counts: &mut UsagesCounts,
) -> Result<FieldSelection<'s>, error::Error> {
    count_command(annotation.command_name.clone(), usage_counts);
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

    match relationship_execution_category(
        source_data_connector,
        &target_source.details.data_connector,
        &target_source.capabilities,
    ) {
        RelationshipExecutionCategory::Local => build_local_command_relationship(
            field,
            field_call,
            annotation,
            source_data_connector,
            type_mappings,
            target_source,
            session_variables,
        ),
        RelationshipExecutionCategory::RemoteForEach => build_remote_command_relationship(
            field,
            field_call,
            annotation,
            type_mappings,
            target_source,
            session_variables,
        ),
    }
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn build_local_model_relationship<'s>(
    field: &normalized_ast::Field<'s, GDS>,
    field_call: &normalized_ast::FieldCall<'s, GDS>,
    annotation: &'s ModelRelationshipAnnotation,
    data_connector: &'s resolved::data_connector::DataConnectorLink,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, resolved::types::TypeMapping>,
    target_source: &'s ModelTargetSource,
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
    data_connector: &'s resolved::data_connector::DataConnectorLink,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, resolved::types::TypeMapping>,
    target_source: &'s CommandTargetSource,
    session_variables: &SessionVariables,
) -> Result<FieldSelection<'s>, error::Error> {
    let relationships_ir = generate_function_based_command(
        &annotation.command_name,
        &target_source.function_name,
        field,
        field_call,
        &annotation.underlying_object_typename,
        &target_source.details,
        session_variables,
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
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, resolved::types::TypeMapping>,
    target_source: &'s ModelTargetSource,
    filter_clause: ResolvedFilterExpression<'s>,
    limit: Option<u32>,
    offset: Option<u32>,
    order_by: Option<ResolvedOrderBy<'s>>,
    session_variables: &SessionVariables,
    usage_counts: &mut UsagesCounts,
) -> Result<FieldSelection<'s>, error::Error> {
    let mut join_mapping: Vec<(SourceField, TargetField)> = vec![];
    for resolved::relationship::RelationshipModelMapping {
        source_field: source_field_path,
        target_field: target_field_path,
    } in annotation.mappings.iter()
    {
        let source_column = get_field_mapping_of_field_name(
            type_mappings,
            &annotation.source_type,
            &annotation.relationship_name,
            &source_field_path.field_name,
        )?;
        let target_column = get_field_mapping_of_field_name(
            &target_source.model.type_mappings,
            &annotation.target_type,
            &annotation.relationship_name,
            &target_field_path.field_name,
        )?;

        let source_field = (source_field_path.field_name.clone(), source_column);
        let target_field = (target_field_path.field_name.clone(), target_column);
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
    for (_source, (_field_name, field)) in &join_mapping {
        let target_value_variable = format!("${}", &field.column);
        let comparison_exp = ndc::models::Expression::BinaryComparisonOperator {
            column: ndc::models::ComparisonTarget::Column {
                name: field.column.clone(),
                path: vec![],
            },
            operator: ndc::models::BinaryComparisonOperator::Equal,
            value: ndc::models::ComparisonValue::Variable {
                name: target_value_variable,
            },
        };
        remote_relationships_ir
            .filter_clause
            .expressions
            .push(comparison_exp);
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
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, resolved::types::TypeMapping>,
    target_source: &'s CommandTargetSource,
    session_variables: &SessionVariables,
) -> Result<FieldSelection<'s>, error::Error> {
    let mut join_mapping: Vec<(SourceField, ArgumentName)> = vec![];
    for resolved::relationship::RelationshipCommandMapping {
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
        &annotation.underlying_object_typename,
        &target_source.details,
        session_variables,
    )?;

    // Add the arguments on which the join is done to the command arguments
    let mut variable_arguments = BTreeMap::new();
    for (_source, target_argument_name) in &join_mapping {
        let target_value_variable = format!("${}", target_argument_name);
        variable_arguments.insert(target_argument_name.to_string(), target_value_variable);
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

fn get_field_mapping_of_field_name(
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, resolved::types::TypeMapping>,
    type_name: &Qualified<CustomTypeName>,
    relationship_name: &RelationshipName,
    field_name: &FieldName,
) -> Result<resolved::types::FieldMapping, error::Error> {
    let type_mapping = type_mappings.get(type_name).ok_or_else(|| {
        error::InternalDeveloperError::TypeMappingNotFoundForRelationship {
            type_name: type_name.clone(),
            relationship_name: relationship_name.clone(),
        }
    })?;
    match type_mapping {
        resolved::types::TypeMapping::Object { field_mappings } => Ok(field_mappings
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
