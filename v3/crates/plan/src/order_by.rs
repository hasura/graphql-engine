use std::collections::BTreeMap;

use super::types::{OrderByError, PlanError};
use crate::metadata_accessor::OutputObjectTypeView;
use hasura_authn_core::Session;
use metadata_resolve::{Qualified, RelationshipTarget, TypeMapping};
use open_dds::{
    data_connector::DataConnectorColumnName, query::OrderByElement, types::CustomTypeName,
};
use plan_types::{PredicateQueryTrees, ResolvedFilterExpression, UniqueNumber, UsagesCounts};

pub fn to_resolved_order_by_element(
    metadata: &metadata_resolve::Metadata,
    session: &Session,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    type_name: &Qualified<CustomTypeName>,
    model_object_type: &OutputObjectTypeView,
    data_connector: &metadata_resolve::DataConnectorLink,
    element: &OrderByElement,
    collect_relationships: &mut BTreeMap<plan_types::NdcRelationshipName, plan_types::Relationship>,
    remote_predicates: &mut PredicateQueryTrees,
    unique_number: &mut UniqueNumber,
    usage_counts: &mut UsagesCounts,
) -> Result<plan_types::OrderByElement<ResolvedFilterExpression>, PlanError> {
    let order_direction = match element.direction {
        open_dds::models::OrderByDirection::Asc => plan_types::OrderByDirection::Asc,
        open_dds::models::OrderByDirection::Desc => plan_types::OrderByDirection::Desc,
    };
    let target = from_operand(
        metadata,
        session,
        type_mappings,
        type_name,
        model_object_type,
        data_connector,
        &element.operand,
        vec![], // Start with empty relationship path
        vec![], // Start with empty field path
        collect_relationships,
        remote_predicates,
        unique_number,
        usage_counts,
    )?;
    Ok(plan_types::OrderByElement {
        order_direction,
        target,
    })
}

fn from_operand(
    metadata: &metadata_resolve::Metadata,
    session: &Session,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    type_name: &Qualified<CustomTypeName>,
    object_type: &OutputObjectTypeView,
    data_connector: &metadata_resolve::DataConnectorLink,
    operand: &open_dds::query::Operand,
    relationship_path: Vec<plan_types::RelationshipPathElement<ResolvedFilterExpression>>,
    field_path: Vec<DataConnectorColumnName>,
    collect_relationships: &mut BTreeMap<plan_types::NdcRelationshipName, plan_types::Relationship>,
    remote_predicates: &mut PredicateQueryTrees,
    unique_number: &mut UniqueNumber,
    usage_counts: &mut UsagesCounts,
) -> Result<plan_types::OrderByTarget<ResolvedFilterExpression>, PlanError> {
    match operand {
        open_dds::query::Operand::Field(field_operand) => resolve_field_operand(
            metadata,
            session,
            type_mappings,
            type_name,
            object_type,
            data_connector,
            field_operand,
            relationship_path,
            field_path,
            collect_relationships,
            remote_predicates,
            unique_number,
            usage_counts,
        ),
        open_dds::query::Operand::Relationship(relationship_operand) => {
            resolve_relationship_operand(
                metadata,
                session,
                type_mappings,
                type_name,
                object_type,
                data_connector,
                relationship_operand,
                relationship_path,
                field_path,
                collect_relationships,
                remote_predicates,
                unique_number,
                usage_counts,
            )
        }
        open_dds::query::Operand::RelationshipAggregate(relationship_operand) => {
            Err(OrderByError::RelationshipAggregateNotSupported(
                relationship_operand.target.relationship_name.clone(),
            )
            .into_plan_error())?
        }
    }
}

fn resolve_field_operand(
    metadata: &metadata_resolve::Metadata,
    session: &Session,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    type_name: &Qualified<CustomTypeName>,
    object_type: &OutputObjectTypeView,
    data_connector: &metadata_resolve::DataConnectorLink,
    operand: &open_dds::query::ObjectFieldOperand,
    relationship_path: Vec<plan_types::RelationshipPathElement<ResolvedFilterExpression>>,
    mut field_path: Vec<DataConnectorColumnName>,
    collect_relationships: &mut BTreeMap<plan_types::NdcRelationshipName, plan_types::Relationship>,
    remote_predicates: &mut PredicateQueryTrees,
    unique_number: &mut UniqueNumber,
    usage_counts: &mut UsagesCounts,
) -> Result<plan_types::OrderByTarget<ResolvedFilterExpression>, PlanError> {
    let type_mapping = type_mappings.get(type_name).ok_or_else(|| {
        OrderByError::Internal(format!("can't find type mapping for type: {type_name}"))
            .into_plan_error()
    })?;
    let field_mapping = match type_mapping {
        metadata_resolve::TypeMapping::Object {
            ndc_object_type_name: _,
            field_mappings,
        } => field_mappings
            .get(&operand.target.field_name)
            .ok_or_else(|| OrderByError::FieldMappingNotFound {
                field_name: operand.target.field_name.clone(),
                object_type_name: type_name.clone(),
            })?,
    };
    let column_name = field_mapping.column.clone();

    if let Some(nested_operand) = &operand.nested {
        let field = object_type
            .fields
            .get(&operand.target.field_name)
            .ok_or_else(|| {
                OrderByError::Internal(format!(
                    "can't find object field definition for field {} in type: {}",
                    operand.target.field_name, type_name
                ))
                .into_plan_error()
            })?;

        let field_type = field
            .field_type
            .get_underlying_type_name()
            .get_custom_type_name()
            .ok_or_else(|| {
                OrderByError::NestedOrderByNotSupported(format!(
                    "field {} is a built-in scalar",
                    operand.target.field_name,
                ))
                .into_plan_error()
            })?;

        let field_object_type =
            crate::metadata_accessor::get_output_object_type(metadata, field_type, &session.role)?;

        field_path.push(column_name);
        from_operand(
            metadata,
            session,
            type_mappings,
            field_type,
            &field_object_type,
            data_connector,
            nested_operand,
            relationship_path,
            field_path,
            collect_relationships,
            remote_predicates,
            unique_number,
            usage_counts,
        )
    } else {
        // Get the first column in the field_path
        let first_column = field_path
            .first()
            .cloned()
            .unwrap_or_else(|| column_name.clone()); // If field_path is empty, use column_name
        Ok(plan_types::OrderByTarget::Column {
            relationship_path,
            name: first_column,
            field_path: field_path
                .into_iter()
                .chain([column_name]) // Append column_name to field_path
                .skip(1) // Skip the first column as it's used as the name
                .collect(),
        })
    }
}

fn resolve_relationship_operand(
    metadata: &metadata_resolve::Metadata,
    session: &Session,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    type_name: &Qualified<CustomTypeName>,
    object_type: &OutputObjectTypeView,
    data_connector: &metadata_resolve::DataConnectorLink,
    operand: &open_dds::query::RelationshipOperand,
    mut relationship_path: Vec<plan_types::RelationshipPathElement<ResolvedFilterExpression>>,
    mut field_path: Vec<DataConnectorColumnName>,
    collect_relationships: &mut BTreeMap<plan_types::NdcRelationshipName, plan_types::Relationship>,
    remote_predicates: &mut PredicateQueryTrees,
    unique_number: &mut UniqueNumber,
    usage_counts: &mut UsagesCounts,
) -> Result<plan_types::OrderByTarget<ResolvedFilterExpression>, PlanError> {
    let relationship_name = &operand.target.relationship_name;
    // Get the relationship field
    let relationship = object_type
        .relationship_fields
        .get(relationship_name)
        .ok_or_else(|| {
            OrderByError::Internal(format!(
                "can't find relationship {} in type: {}",
                operand.target.relationship_name, type_name
            ))
            .into_plan_error()
        })?;

    match &relationship.target {
        RelationshipTarget::Model(model_relationship_target) => {
            // Get the target model
            let target_model_name = &model_relationship_target.model_name;
            let target_model = metadata.models.get(target_model_name).ok_or_else(|| {
                OrderByError::Internal(format!("model {target_model_name} not found in metadata"))
                    .into_plan_error()
            })?;

            let target_model_source = target_model.model.source.as_deref().ok_or_else(|| {
                OrderByError::Internal(format!("model {target_model_name} has no source"))
                    .into_plan_error()
            })?;

            let local_model_relationship_info = plan_types::LocalModelRelationshipInfo {
                relationship_name,
                relationship_type: &model_relationship_target.relationship_type,
                source_type: type_name,
                source_type_mappings: type_mappings,
                target_model_name,
                target_source: target_model_source,
                mappings: &model_relationship_target.mappings,
            };

            let ndc_relationship_name =
                plan_types::NdcRelationshipName::new(type_name, relationship_name);

            // Reject remote relationship
            super::query::field_selection::reject_remote_relationship(
                relationship_name,
                data_connector,
                &target_model_source.data_connector,
            )
            .map_err(|e| {
                // Convert to OrderByError for more specific error handling
                OrderByError::RemoteRelationshipNotSupported(e.to_string()).into_plan_error()
            })?;

            // Permission filter
            let target_permission_filter = super::filter::resolve_model_permission_filter(
                session,
                target_model,
                target_model_source,
                &metadata.object_types,
                collect_relationships,
                remote_predicates,
                unique_number,
                usage_counts,
            )?;

            // Record this relationship
            collect_relationships.insert(
                ndc_relationship_name.clone(),
                super::query::process_model_relationship_definition(
                    &local_model_relationship_info,
                )?,
            );
            let target_type = &model_relationship_target.target_typename;
            let target_type_mappings = &target_model_source.type_mappings;

            // Create relationship path element
            let path_element = plan_types::RelationshipPathElement {
                field_path: field_path.clone(),
                relationship_name: ndc_relationship_name,
                filter_predicate: target_permission_filter,
            };
            relationship_path.push(path_element);
            field_path = vec![]; // Field path resets as we pass through a relationship

            let target_output_object_type = crate::metadata_accessor::get_output_object_type(
                metadata,
                target_type,
                &session.role,
            )?;

            // Handle nested operand
            match operand.nested.as_ref() {
                Some(nested_operand) => from_operand(
                    metadata,
                    session,
                    target_type_mappings,
                    target_type,
                    &target_output_object_type,
                    data_connector,
                    nested_operand,
                    relationship_path,
                    field_path,
                    collect_relationships,
                    remote_predicates,
                    unique_number,
                    usage_counts,
                ),
                None => Err(OrderByError::Internal(
                    "Relationship operand must have a nested field".to_string(),
                )
                .into_plan_error())?,
            }
        }
        RelationshipTarget::Command(_command_relationship_target) => {
            // Command relationships are not supported in order_by
            Err(OrderByError::Internal(format!(
                "Command relationships are not supported in order_by: {relationship_name}"
            ))
            .into_plan_error())?
        }
    }
}
