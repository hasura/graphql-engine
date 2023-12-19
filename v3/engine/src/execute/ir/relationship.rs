use std::collections::BTreeMap;

use hasura_authn_core::SessionVariables;
use lang_graphql::normalized_ast::{self, Field};
use open_dds::{
    relationships::{RelationshipName, RelationshipType},
    types::{CustomTypeName, FieldName},
};

use ndc_client as ndc;
use serde::Serialize;

use super::filter::resolve_filter_expression;
use super::model_selection::model_selection_ir;
use super::order_by::build_ndc_order_by;
use super::permissions;
use super::selection_set::FieldSelection;
use crate::execute::error;
use crate::execute::model_tracking::{count_model, UsagesCounts};
use crate::metadata::resolved::subgraph::serialize_qualified_btreemap;
use crate::schema::types::output_type::relationship::{
    ModelRelationshipAnnotation, ModelTargetSource,
};
use crate::{
    metadata::resolved::{self, subgraph::Qualified},
    schema::{
        types::{Annotation, InputAnnotation, ModelInputAnnotation},
        GDS,
    },
};

#[derive(Debug, Serialize)]
pub(crate) struct RelationshipInfo<'s> {
    pub annotation: &'s ModelRelationshipAnnotation,
    pub source_data_connector: &'s resolved::data_connector::DataConnector,
    #[serde(serialize_with = "serialize_qualified_btreemap")]
    pub source_type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, resolved::types::TypeMapping>,
    pub target_source: &'s ModelTargetSource,
}

#[derive(Debug, Clone, Serialize)]
pub struct RemoteRelationshipInfo<'s> {
    pub annotation: &'s ModelRelationshipAnnotation,
    /// This contains processed information about the mappings.
    /// `RelationshipMapping` only contains mapping of field names. This
    /// contains mapping of field names and `resolved::types::FieldMapping`.
    /// Also see `build_remote_relationship`.
    pub join_mapping: Vec<(SourceField, TargetField)>,
}

pub type SourceField = (FieldName, resolved::types::FieldMapping);
pub type TargetField = (FieldName, resolved::types::FieldMapping);

pub(crate) fn process_relationship_definition(
    relationship_info: &RelationshipInfo,
) -> Result<ndc::models::Relationship, error::Error> {
    let &RelationshipInfo {
        annotation,
        source_data_connector,
        source_type_mappings,
        target_source,
    } = relationship_info;

    let mut column_mapping = BTreeMap::new();
    for resolved::relationship::RelationshipModelMapping {
        source_field: source_field_path,
        target_field: target_field_path,
    } in annotation.mappings.iter()
    {
        if !matches!(
            relationship_execution_category(target_source, source_data_connector),
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
            let target_column = get_field_mapping_of_field_name(
                &target_source.model.type_mappings,
                &annotation.target_type,
                &annotation.relationship_name,
                &target_field_path.field_name,
            )?;

            if column_mapping
                .insert(source_column.column, target_column.column)
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
        column_mapping,
        relationship_type: {
            match annotation.relationship_type {
                RelationshipType::Object => ndc_client::models::RelationshipType::Object,
                RelationshipType::Array => ndc_client::models::RelationshipType::Array,
            }
        },
        target_collection: target_source.model.collection.to_string(),
        arguments: BTreeMap::new(),
    };
    Ok(ndc_relationship)
}

enum RelationshipExecutionCategory {
    // Push down relationship definition to the data connector
    Local,
    // Use foreach in the data connector to fetch related rows for multiple objects in a single request
    RemoteForEach,
}

#[allow(clippy::match_single_binding)]
fn relationship_execution_category(
    target_source: &ModelTargetSource,
    source_connector: &resolved::data_connector::DataConnector,
) -> RelationshipExecutionCategory {
    // It's a local relationship if the source and target connectors are the same and
    // the connector supports relationships.
    if target_source.model.data_connector.name == source_connector.name
        && target_source.capabilities.relationships
    {
        RelationshipExecutionCategory::Local
    } else {
        match target_source.capabilities.foreach {
            // TODO: When we support naive relationships for connectors not implementing foreach,
            // add another match arm / return enum variant
            () => RelationshipExecutionCategory::RemoteForEach,
        }
    }
}

pub(crate) fn generate_relationship_ir<'s>(
    field: &Field<'s, GDS>,
    annotation: &'s ModelRelationshipAnnotation,
    data_connector: &'s resolved::data_connector::DataConnector,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, resolved::types::TypeMapping>,
    session_variables: &SessionVariables,
    usage_counts: &mut UsagesCounts,
) -> Result<FieldSelection<'s>, error::Error> {
    // Add the target model being used in the usage counts
    count_model(annotation.model_name.clone(), usage_counts);
    let field_call = field.field_call()?;

    let mut limit = None;
    let mut offset = None;
    let mut filter_clause = Vec::new();
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
                            filter_clause = resolve_filter_expression(argument.value.as_object()?)?
                        }
                        ModelInputAnnotation::ModelOrderByExpression => {
                            order_by = Some(build_ndc_order_by(argument)?)
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
    match relationship_execution_category(target_source, data_connector) {
        RelationshipExecutionCategory::Local => build_local_relationship(
            field,
            field_call,
            annotation,
            data_connector,
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

#[allow(clippy::too_many_arguments)]
pub(crate) fn build_local_relationship<'s>(
    field: &normalized_ast::Field<'s, GDS>,
    field_call: &normalized_ast::FieldCall<'s, GDS>,
    annotation: &'s ModelRelationshipAnnotation,
    data_connector: &'s resolved::data_connector::DataConnector,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, resolved::types::TypeMapping>,
    target_source: &'s ModelTargetSource,
    filter_clause: Vec<ndc::models::Expression>,
    limit: Option<u32>,
    offset: Option<u32>,
    order_by: Option<ndc::models::OrderBy>,
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
    let rel_info = RelationshipInfo {
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
    let relationship_name =
        serde_json::to_string(&(&annotation.source_type, &annotation.relationship_name))?;

    Ok(FieldSelection::LocalRelationship {
        query: relationships_ir,
        name: relationship_name,
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
    filter_clause: Vec<ndc::models::Expression>,
    limit: Option<u32>,
    offset: Option<u32>,
    order_by: Option<ndc::models::OrderBy>,
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
        remote_relationships_ir.filter_clause.push(comparison_exp);
    }
    let rel_info = RemoteRelationshipInfo {
        annotation,
        join_mapping,
    };
    Ok(FieldSelection::RemoteRelationship {
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
