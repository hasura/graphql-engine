pub mod field_selection;
pub mod model_target;
mod types;
pub use types::{NDCQuery, QueryContext};

use crate::column::to_resolved_column;
use crate::types::PlanError;
use indexmap::IndexMap;
use std::collections::BTreeMap;
use std::sync::Arc;

use execute::{
    plan::{field::Field, Argument, ResolvedFilterExpression},
    QueryExecutionPlan, QueryNode,
};
use graphql_ir::AggregateFieldSelection;
use hasura_authn_core::Session;
use metadata_resolve::{Metadata, Qualified};
use open_dds::query::{
    Aggregate, AggregationFunction, ModelSelection, ModelTarget, ObjectSubSelection, Operand,
    Query, QueryRequest,
};
use open_dds::types::CustomTypeName;
use plan_types::NdcFieldAlias;

// make a query execution plan, assuming an OpenDD IR with a single model request
pub async fn plan_query_request<'req, 'metadata>(
    query_request: &'req QueryRequest,
    metadata: &'metadata Metadata,
    session: &Arc<Session>,
    http_context: &Arc<execute::HttpContext>,
) -> Result<(execute::ResolvedQueryExecutionPlan, QueryContext), PlanError>
where
    'metadata: 'req,
{
    let QueryRequest::V1(query_request_v1) = query_request;

    // to limit scope, let's assume there's one item and explode otherwise
    let (_alias, query) = query_request_v1.queries.first().unwrap();

    // return plan for a single query (again, wrong, but let's unblock ourselves for now)
    query_to_plan(query, metadata, session, http_context).await
}

// turn a single OpenDD IR Query into a query execution plan
async fn query_to_plan<'req, 'metadata>(
    query: &'req Query,
    metadata: &'metadata Metadata,
    session: &Arc<Session>,
    http_context: &Arc<execute::HttpContext>,
) -> Result<(execute::ResolvedQueryExecutionPlan, QueryContext), PlanError>
where
    'metadata: 'req,
{
    match query {
        open_dds::query::Query::Model(model_selection) => {
            let (type_name, ndc_query, fields) =
                from_model_selection(model_selection, metadata, session, http_context).await?;
            let query_execution_plan =
                ndc_query_to_query_execution_plan(&ndc_query, &fields, &IndexMap::new());
            let query_context = QueryContext { type_name };
            Ok((query_execution_plan, query_context))
        }
        open_dds::query::Query::ModelAggregate(model_aggregate) => {
            // we have to use `String` rather than `Alias` in the planning code so not to restrict ourselves to aliases
            // that are valid GraphQL types. Probably want to change `Alias` in OpenDD to something
            // less frontend specific, then we can avoid this cloning.
            let selection = model_aggregate
                .selection
                .iter()
                .map(|(k, v)| (k.to_string(), v.clone()))
                .collect();

            let (type_name, ndc_query, aggregate_fields) = from_model_aggregate_selection(
                &model_aggregate.target,
                &selection,
                metadata,
                session,
                http_context,
            )
            .await?;
            let query_execution_plan =
                ndc_query_to_query_execution_plan(&ndc_query, &IndexMap::new(), &aggregate_fields);
            let query_context = QueryContext { type_name };
            Ok((query_execution_plan, query_context))
        }

        open_dds::query::Query::Command(_) => Err(PlanError::Internal(
            "Model requests supported only".to_string(),
        )),
    }
}

pub async fn from_model_aggregate_selection(
    model_target: &ModelTarget,
    selection: &IndexMap<String, Aggregate>,
    metadata: &Metadata,
    session: &Arc<Session>,
    http_context: &Arc<execute::HttpContext>,
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
        model,
        model_source,
        model_object_type,
    )
    .await?;

    Ok((model.model.data_type.clone(), query, fields))
}

pub async fn from_model_selection(
    model_selection: &ModelSelection,
    metadata: &Metadata,
    session: &Arc<Session>,
    http_context: &Arc<execute::HttpContext>,
) -> Result<
    (
        Qualified<CustomTypeName>,
        NDCQuery,
        IndexMap<NdcFieldAlias, Field<ResolvedFilterExpression>>,
    ),
    PlanError,
> {
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

    let metadata_resolve::TypeMapping::Object { field_mappings, .. } = model_source
        .type_mappings
        .get(&model.model.data_type)
        .ok_or_else(|| {
            PlanError::Internal(format!(
                "couldn't fetch type_mapping of type {} for model {}",
                model.model.data_type, qualified_model_name
            ))
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

    let type_permissions = model_object_type
        .type_output_permissions
        .get(&session.role)
        .ok_or_else(|| {
            PlanError::Permission(format!(
                "role {} does not have permission to select any fields of model {}",
                session.role, qualified_model_name
            ))
        })?;

    let mut ndc_fields = IndexMap::new();

    for (field_alias, object_sub_selection) in &model_selection.selection {
        let ObjectSubSelection::Field(field_selection) = object_sub_selection else {
            return Err(PlanError::Internal(
                "only normal field selections are supported in NDCPushDownPlanner.".into(),
            ));
        };
        if !type_permissions
            .allowed_fields
            .contains(&field_selection.target.field_name)
        {
            return Err(PlanError::Permission(format!(
                "role {} does not have permission to select the field {} from type {} of model {}",
                session.role,
                field_selection.target.field_name,
                model.model.data_type,
                qualified_model_name
            )));
        }

        let field_mapping = field_mappings
            .get(&field_selection.target.field_name)
            // .map(|field_mapping| field_mapping.column.clone())
            .ok_or_else(|| {
                PlanError::Internal(format!(
                    "couldn't fetch field mapping of field {} in type {} for model {}",
                    field_selection.target.field_name, model.model.data_type, qualified_model_name
                ))
            })?;

        let field_type = &model_object_type
            .object_type
            .fields
            .get(&field_selection.target.field_name)
            .ok_or_else(|| {
                PlanError::Internal(format!(
                    "could not look up type of field {}",
                    field_selection.target.field_name
                ))
            })?
            .field_type;

        let fields = field_selection::ndc_nested_field_selection_for(
            metadata,
            field_type,
            &model_source.type_mappings,
        )?;

        let ndc_field = Field::Column {
            column: field_mapping.column.clone(),
            fields,
            arguments: BTreeMap::new(),
        };

        ndc_fields.insert(NdcFieldAlias::from(field_alias.as_str()), ndc_field);
    }

    let query = model_target::model_target_to_ndc_query(
        model_target,
        session,
        http_context,
        metadata,
        model,
        model_source,
        model_object_type,
    )
    .await?;

    Ok((model.model.data_type.clone(), query, ndc_fields))
}

// take NDCQuery and fields and make a sweet execution plan
pub fn ndc_query_to_query_execution_plan(
    query: &NDCQuery,
    fields: &IndexMap<NdcFieldAlias, Field<ResolvedFilterExpression>>,
    aggregate_fields: &IndexMap<NdcFieldAlias, AggregateFieldSelection>,
) -> QueryExecutionPlan<ResolvedFilterExpression> {
    let query_fields: Option<IndexMap<_, _>> = if fields.is_empty() {
        None
    } else {
        Some(
            fields
                .iter()
                .map(|(field_name, field)| (field_name.clone(), field.clone()))
                .collect(),
        )
    };

    let query_aggregate_fields = if aggregate_fields.is_empty() {
        None
    } else {
        Some(graphql_ir::AggregateSelectionSet {
            fields: aggregate_fields.clone(),
        })
    };

    QueryExecutionPlan {
        query_node: QueryNode {
            fields: query_fields,
            aggregates: query_aggregate_fields,
            limit: query.limit,
            offset: query.offset,
            order_by: Some(query.order_by.order_by_elements.clone()),
            predicate: query.filter.clone(),
        },
        collection: query.collection_name.clone(),
        arguments: query
            .arguments
            .iter()
            .map(|(argument, value)| {
                (
                    argument.clone(),
                    Argument::Literal {
                        value: value.clone(),
                    },
                )
            })
            .collect(),
        collection_relationships: query.collection_relationships.clone(),
        variables: None,
        data_connector: query.data_connector.clone(),
    }
}
