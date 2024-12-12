use super::types::NDCQuery;
use super::{field_selection, model_target};

use crate::column::to_resolved_column;
use crate::types::PlanError;
use indexmap::IndexMap;
use std::collections::BTreeMap;
use std::sync::Arc;

use hasura_authn_core::Session;
use metadata_resolve::{Metadata, Qualified};
use open_dds::query::{Aggregate, AggregationFunction, ModelSelection, ModelTarget, Operand};
use open_dds::types::CustomTypeName;
use plan_types::{
    AggregateFieldSelection, AggregateSelectionSet, FieldsSelection, NdcFieldAlias,
    QueryExecutionPlan, QueryNodeNew, UniqueNumber,
};

pub struct ModelAggregateSelection {
    pub object_type_name: Qualified<CustomTypeName>,
    pub query: NDCQuery,
    pub fields: IndexMap<NdcFieldAlias, AggregateFieldSelection>,
}

pub fn from_model_aggregate_selection(
    model_target: &ModelTarget,
    selection: &IndexMap<String, Aggregate>,
    metadata: &Metadata,
    session: &Arc<Session>,
    request_headers: &reqwest::header::HeaderMap,
    unique_number: &mut UniqueNumber,
) -> Result<ModelAggregateSelection, PlanError> {
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
        metadata,
        request_headers,
        model,
        model_source,
        model_object_type,
        unique_number,
    )?;

    Ok(ModelAggregateSelection {
        object_type_name: model.model.data_type.clone(),
        query,
        fields,
    })
}

pub fn from_model_selection(
    model_selection: &ModelSelection,
    metadata: &Metadata,
    session: &Arc<Session>,
    request_headers: &reqwest::header::HeaderMap,
    unique_number: &mut UniqueNumber,
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

    let mut relationships = BTreeMap::new();

    let ndc_fields = field_selection::resolve_field_selection(
        metadata,
        session,
        request_headers,
        &model.model.data_type,
        model_object_type,
        &model_source.type_mappings,
        &model_source.data_connector,
        &model_selection.selection,
        &mut relationships,
        unique_number,
    )?;

    let mut query = model_target::model_target_to_ndc_query(
        model_target,
        session,
        metadata,
        request_headers,
        model,
        model_source,
        model_object_type,
        unique_number,
    )?;

    // collect relationships accummulated in this scope.
    query.collection_relationships.append(&mut relationships);

    Ok((
        model.model.data_type.clone(),
        query,
        FieldsSelection { fields: ndc_fields },
    ))
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
    let order_by = if query.order_by.is_empty() {
        None
    } else {
        Some(query.order_by.clone())
    };

    QueryExecutionPlan {
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
