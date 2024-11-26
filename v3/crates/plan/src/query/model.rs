use super::types::NDCQuery;
use super::{field_selection, model_target};

use crate::column::to_resolved_column;
use crate::types::PlanError;
use execute::plan::process_model_relationship_definition;
use indexmap::IndexMap;
use std::collections::BTreeMap;
use std::sync::Arc;

use engine_types::HttpContext;
use hasura_authn_core::Session;
use metadata_resolve::{Metadata, Qualified};
use open_dds::query::{
    Aggregate, AggregationFunction, ModelSelection, ModelTarget, ObjectSubSelection, Operand,
    RelationshipSelection,
};
use open_dds::types::CustomTypeName;
use plan_types::{
    AggregateFieldSelection, AggregateSelectionSet, Field, FieldsSelection, NdcFieldAlias,
    NdcRelationshipName, PredicateQueryTrees, QueryExecutionPlan, QueryNodeNew, Relationship,
};

pub async fn from_model_aggregate_selection(
    model_target: &ModelTarget,
    selection: &IndexMap<String, Aggregate>,
    metadata: &Metadata,
    session: &Arc<Session>,
    http_context: &Arc<HttpContext>,
    request_headers: &reqwest::header::HeaderMap,
) -> Result<
    (
        Qualified<CustomTypeName>,
        NDCQuery,
        IndexMap<NdcFieldAlias, AggregateFieldSelection>,
    ),
    PlanError,
> {
    let qualified_model_name = metadata_resolve::Qualified::new(
        model_target.subgraph.clone(),
        model_target.model_name.clone(),
    );

    let model = metadata.models.get(&qualified_model_name).ok_or_else(|| {
        PlanError::Internal(format!(
            "model {qualified_model_name} not found in metadata"
        ))
    })?;

    let model_source = model.model.source.as_ref().ok_or_else(|| {
        PlanError::Internal(format!("model {qualified_model_name} has no source"))
    })?;

    let model_object_type = metadata
        .object_types
        .get(&model.model.data_type)
        .ok_or_else(|| {
            PlanError::Internal(format!(
                "object type {} not found in metadata",
                model.model.data_type
            ))
        })?;

    let mut fields = IndexMap::new();

    for (field_alias, aggregate) in selection {
        let column_path = match aggregate.operand.as_ref() {
            None => Ok(vec![]),
            Some(Operand::Field(operand)) => {
                let column = to_resolved_column(
                    metadata,
                    &model_source.type_mappings,
                    &model.model.data_type,
                    model_object_type,
                    operand,
                )?;
                Ok([vec![column.column_name], column.field_path].concat())
            }
            Some(_) => Err(PlanError::Internal("unsupported aggregate operand".into())),
        }?;

        let ndc_aggregate = match aggregate.function {
            AggregationFunction::Count {} => Ok(AggregateFieldSelection::Count { column_path }),
            AggregationFunction::CountDistinct {} => {
                Ok(AggregateFieldSelection::CountDistinct { column_path })
            }
            AggregationFunction::Custom { .. } => Err(PlanError::Internal(
                "custom aggregate functions are not supported".into(),
            )),
        }?;

        fields.insert(NdcFieldAlias::from(field_alias.as_str()), ndc_aggregate);
    }

    let query = model_target::model_target_to_ndc_query(
        model_target,
        session,
        http_context,
        metadata,
        request_headers,
        model,
        model_source,
        model_object_type,
    )
    .await?;

    Ok((model.model.data_type.clone(), query, fields))
}

#[async_recursion::async_recursion]
pub async fn from_model_selection(
    model_selection: &ModelSelection,
    metadata: &Metadata,
    session: &Arc<Session>,
    http_context: &Arc<HttpContext>,
    request_headers: &reqwest::header::HeaderMap,
) -> Result<(Qualified<CustomTypeName>, NDCQuery, FieldsSelection), PlanError> {
    let model_target = &model_selection.target;
    let qualified_model_name = metadata_resolve::Qualified::new(
        model_target.subgraph.clone(),
        model_target.model_name.clone(),
    );

    let model = metadata.models.get(&qualified_model_name).ok_or_else(|| {
        PlanError::Internal(format!(
            "model {qualified_model_name} not found in metadata"
        ))
    })?;

    let model_source = model.model.source.as_ref().ok_or_else(|| {
        PlanError::Internal(format!("model {qualified_model_name} has no source"))
    })?;

    let model_object_type = metadata
        .object_types
        .get(&model.model.data_type)
        .ok_or_else(|| {
            PlanError::Internal(format!(
                "object type {} not found in metadata",
                model.model.data_type
            ))
        })?;

    let metadata_resolve::TypeMapping::Object { field_mappings, .. } = model_source
        .type_mappings
        .get(&model.model.data_type)
        .ok_or_else(|| {
            PlanError::Internal(format!(
                "couldn't fetch type_mapping of type {} for model {}",
                model.model.data_type, qualified_model_name
            ))
        })?;

    let type_permissions = model_object_type
        .type_output_permissions
        .get(&session.role)
        .ok_or_else(|| {
            PlanError::Permission(format!(
                "role {} does not have permission to select any fields of model {}",
                session.role, qualified_model_name
            ))
        })?;

    let mut relationships = BTreeMap::new();
    let mut ndc_fields = IndexMap::new();
    for (field_alias, object_sub_selection) in &model_selection.selection {
        let ndc_field = match object_sub_selection {
            ObjectSubSelection::Field(field_selection) => field_selection::from_field_selection(
                field_selection,
                session,
                metadata,
                &qualified_model_name,
                model,
                model_source,
                model_object_type,
                field_mappings,
                type_permissions,
            )?,
            ObjectSubSelection::Relationship(relationship_selection) => {
                from_relationship_selection(
                    relationship_selection,
                    metadata,
                    session,
                    http_context,
                    request_headers,
                    model,
                    model_source,
                    model_object_type,
                    &mut relationships,
                )
                .await?
            }
            ObjectSubSelection::RelationshipAggregate(_) => {
                return Err(PlanError::Internal(
                    "only normal field/relationship selections are supported in NDCPushDownPlanner.".into(),
                ));
            }
        };
        ndc_fields.insert(NdcFieldAlias::from(field_alias.as_str()), ndc_field);
    }

    let mut query = model_target::model_target_to_ndc_query(
        model_target,
        session,
        http_context,
        metadata,
        request_headers,
        model,
        model_source,
        model_object_type,
    )
    .await?;

    // collect relationships accummulated in this scope.
    query.collection_relationships.append(&mut relationships);

    Ok((
        model.model.data_type.clone(),
        query,
        FieldsSelection { fields: ndc_fields },
    ))
}

pub async fn from_relationship_selection(
    relationship_selection: &RelationshipSelection,
    metadata: &Metadata,
    session: &Arc<Session>,
    http_context: &Arc<HttpContext>,
    request_headers: &reqwest::header::HeaderMap,
    model: &metadata_resolve::ModelWithArgumentPresets,
    model_source: &Arc<metadata_resolve::ModelSource>,
    model_object_type: &metadata_resolve::ObjectTypeWithRelationships,
    relationships: &mut BTreeMap<plan_types::NdcRelationshipName, Relationship>,
) -> Result<Field, PlanError> {
    let RelationshipSelection { target, selection } = relationship_selection;
    let (_, relationship_field) = model_object_type
        .relationship_fields
        .iter()
        .find(|(_, relationship_field)| {
            relationship_field.relationship_name == target.relationship_name
        })
        .ok_or_else(|| {
            PlanError::Internal(format!(
                "couldn't find the relationship {} in the type {}",
                target.relationship_name, model.model.data_type,
            ))
        })?;

    let metadata_resolve::RelationshipTarget::Model(model_relationship_target) =
        &relationship_field.target
    else {
        return Err(PlanError::Relationship(format!(
            "Expecting Model as a relationship target for {}",
            target.relationship_name,
        )));
    };
    let target_model_name = &model_relationship_target.model_name;
    let target_model = metadata.models.get(target_model_name).ok_or_else(|| {
        PlanError::Internal(format!("model {target_model_name} not found in metadata"))
    })?;

    let target_model_source =
        target_model.model.source.as_ref().ok_or_else(|| {
            PlanError::Internal(format!("model {target_model_name} has no source"))
        })?;

    // Reject remote relationships
    if target_model_source.data_connector.name != model_source.data_connector.name {
        return Err(PlanError::Relationship(format!(
            "Remote relationships are not supported: {}",
            &target.relationship_name
        )));
    }

    let target_source = metadata_resolve::ModelTargetSource {
        model: target_model_source.clone(),
        capabilities: relationship_field
            .target_capabilities
            .as_ref()
            .ok_or_else(|| {
                PlanError::Relationship(format!(
                    "Relationship capabilities not found for relationship {} in data connector {}",
                    &target.relationship_name, &target_model_source.data_connector.name,
                ))
            })?
            .clone(),
    };
    let local_model_relationship_info = plan_types::LocalModelRelationshipInfo {
        relationship_name: &target.relationship_name,
        relationship_type: &model_relationship_target.relationship_type,
        source_type: &model.model.data_type,
        source_data_connector: &model_source.data_connector,
        source_type_mappings: &model_source.type_mappings,
        target_source: &target_source,
        target_type: &model_relationship_target.target_typename,
        mappings: &model_relationship_target.mappings,
    };

    let ndc_relationship_name =
        NdcRelationshipName::new(&model.model.data_type, &target.relationship_name);
    relationships.insert(
        ndc_relationship_name.clone(),
        process_model_relationship_definition(&local_model_relationship_info).map_err(|err| {
            PlanError::Internal(format!(
                "Unable to process relationship {} definition: {}",
                &target.relationship_name, err
            ))
        })?,
    );

    let relationship_model_target = ModelTarget {
        subgraph: target_model_name.subgraph.clone(),
        model_name: target_model_name.name.clone(),
        arguments: target.arguments.clone(),
        filter: target.filter.clone(),
        order_by: target.order_by.clone(),
        limit: target.limit,
        offset: target.offset,
    };

    let relationship_target_model_selection = ModelSelection {
        target: relationship_model_target,
        selection: selection.as_ref().map_or_else(IndexMap::new, Clone::clone),
    };

    let (_, ndc_query, relationship_fields) = from_model_selection(
        &relationship_target_model_selection,
        metadata,
        session,
        http_context,
        request_headers,
    )
    .await?;
    let QueryExecutionPlan {
        remote_predicates: _,
        query_node,
        collection: _,
        arguments,
        mut collection_relationships,
        variables: _,
        data_connector: _,
    } = ndc_query_to_query_execution_plan(&ndc_query, &relationship_fields, &IndexMap::new());

    // Collect relationships from the generated query above
    relationships.append(&mut collection_relationships);

    let ndc_field = Field::Relationship {
        relationship: ndc_relationship_name,
        arguments,
        query_node: Box::new(query_node),
    };
    Ok(ndc_field)
}

// take NDCQuery and fields and make a sweet execution plan
pub fn ndc_query_to_query_execution_plan(
    query: &NDCQuery,
    fields: &FieldsSelection,
    aggregate_fields: &IndexMap<NdcFieldAlias, AggregateFieldSelection>,
) -> QueryExecutionPlan {
    let query_fields: Option<FieldsSelection> = if fields.fields.is_empty() {
        None
    } else {
        Some(fields.clone())
    };

    let query_aggregate_fields = if aggregate_fields.is_empty() {
        None
    } else {
        Some(AggregateSelectionSet {
            fields: aggregate_fields.clone(),
        })
    };

    // only send an ordering if there are actually elements
    let order_by = if query.order_by.order_by_elements.is_empty() {
        None
    } else {
        Some(query.order_by.order_by_elements.clone())
    };

    QueryExecutionPlan {
        remote_predicates: PredicateQueryTrees::new(),
        query_node: QueryNodeNew {
            fields: query_fields,
            aggregates: query_aggregate_fields,
            limit: query.limit,
            offset: query.offset,
            order_by,
            predicate: query.filter.clone(),
        },
        collection: query.collection_name.clone(),
        arguments: query.arguments.clone(),
        collection_relationships: query.collection_relationships.clone(),
        variables: None,
        data_connector: query.data_connector.clone(),
    }
}
