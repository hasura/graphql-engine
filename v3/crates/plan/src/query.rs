mod types;
pub use types::{NDCQuery, QueryContext};

use crate::filter::to_resolved_filter_expr;
use crate::order_by::to_resolved_order_by_element;
use crate::types::PlanError;
use indexmap::IndexMap;
use std::collections::BTreeMap;
use std::sync::Arc;

use execute::{
    plan::{
        field::{Field, NestedArray, NestedField},
        Argument, ResolvedFilterExpression,
    },
    HttpContext, QueryExecutionPlan, QueryNode,
};
use hasura_authn_core::Session;
use metadata_resolve::{
    FilterPermission, Metadata, Qualified, QualifiedTypeReference, TypeMapping,
};
use open_dds::query::{ModelSelection, ModelTarget, ObjectSubSelection, Query, QueryRequest};
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
            let query_execution_plan = ndc_query_to_query_execution_plan(&ndc_query, &fields);
            let query_context = QueryContext { type_name };
            Ok((query_execution_plan, query_context))
        }
        _ => Err(PlanError::Internal(
            "Model requests supported only".to_string(),
        )),
    }
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

        let fields =
            ndc_nested_field_selection_for(metadata, field_type, &model_source.type_mappings)?;

        let ndc_field = Field::Column {
            column: field_mapping.column.clone(),
            fields,
            arguments: BTreeMap::new(),
        };

        ndc_fields.insert(NdcFieldAlias::from(field_alias.as_str()), ndc_field);
    }

    let query = model_target_to_ndc_query(
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

pub fn ndc_nested_field_selection_for(
    metadata: &Metadata,
    column_type: &QualifiedTypeReference,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
) -> Result<Option<NestedField<ResolvedFilterExpression>>, PlanError> {
    match &column_type.underlying_type {
        metadata_resolve::QualifiedBaseType::Named(name) => match name {
            metadata_resolve::QualifiedTypeName::Custom(name) => {
                if let Some(_scalar_type) = metadata.scalar_types.get(name) {
                    return Ok(None);
                }
                if let Some(object_type) = metadata.object_types.get(name) {
                    let TypeMapping::Object {
                        ndc_object_type_name: _,
                        field_mappings,
                    } = type_mappings.get(name).ok_or_else(|| {
                        PlanError::Internal(format!("can't find mapping object for type: {name}"))
                    })?;

                    let mut fields = IndexMap::new();

                    for (field_name, field_mapping) in field_mappings {
                        let field_def = object_type.object_type.fields.get(field_name).ok_or_else(|| PlanError::Internal(format!(
                            "can't find object field definition for field {field_name} in type: {name}"
                        )))?;
                        let nested_fields: Option<NestedField<ResolvedFilterExpression>> =
                            ndc_nested_field_selection_for(
                                metadata,
                                &field_def.field_type,
                                type_mappings,
                            )?;
                        fields.insert(
                            NdcFieldAlias::from(field_name.as_str()),
                            Field::Column {
                                column: field_mapping.column.clone(),
                                fields: nested_fields,
                                arguments: BTreeMap::new(),
                            },
                        );
                    }

                    return Ok(Some(NestedField::Object(
                        execute::plan::field::NestedObject { fields },
                    )));
                }

                Err(PlanError::Internal(format!(
                    "named type was neither a scalar nor an object: {name}",
                )))
            }
            metadata_resolve::QualifiedTypeName::Inbuilt(_) => Ok(None),
        },
        metadata_resolve::QualifiedBaseType::List(list_type) => {
            let fields =
                ndc_nested_field_selection_for(metadata, list_type.as_ref(), type_mappings)?;

            Ok(fields.map(|fields| {
                NestedField::Array(NestedArray {
                    fields: Box::new(fields),
                })
            }))
        }
    }
}

pub async fn model_target_to_ndc_query(
    model_target: &ModelTarget,
    session: &Session,
    http_context: &HttpContext,
    metadata: &metadata_resolve::Metadata,
    // The following are things we could compute, but we have them on hand
    // at all call sites anyway:
    model: &metadata_resolve::ModelWithPermissions,
    model_source: &metadata_resolve::ModelSource,
    model_object_type: &metadata_resolve::ObjectTypeWithRelationships,
) -> Result<NDCQuery, PlanError> {
    let qualified_model_name = metadata_resolve::Qualified::new(
        model_target.subgraph.clone(),
        model_target.model_name.clone(),
    );

    let model_select_permission = model.select_permissions.get(&session.role).ok_or_else(|| {
        PlanError::Permission(format!(
            "role {} does not have select permission for model {}",
            session.role, qualified_model_name
        ))
    })?;

    let mut usage_counts = graphql_ir::UsagesCounts::default();
    let mut relationships: BTreeMap<graphql_ir::NdcRelationshipName, execute::plan::Relationship> =
        BTreeMap::new();

    let permission_filter = match &model_select_permission.filter {
        FilterPermission::AllowAll => Ok::<_, PlanError>(None),
        FilterPermission::Filter(filter) => {
            let filter_ir = graphql_ir::process_model_predicate(
                &model_source.data_connector,
                &model_source.type_mappings,
                filter,
                &session.variables,
                &mut usage_counts,
            )
            .map_err(|e| {
                PlanError::Internal(format!("error when processing model predicate: {e}"))
            })?;

            let filter_plan = execute::plan::plan_expression(&filter_ir, &mut relationships)
                .map_err(|e| {
                    PlanError::Internal(format!("error constructing permission filter plan: {e}"))
                })?;

            // TODO: this thing has to change, need to be pushed into the
            // execution plan. We shouldn't be running this in the planning phase
            let resolve_context =
                execute::plan::ResolveFilterExpressionContext::new_allow_in_engine_resolution(
                    http_context,
                );

            // TODO: this resolve step should go away once we've split `execute` into actual 'plan' and
            // 'run' steps, then none of this planning will need to be async. (currently the async
            // is in case we're running remote predicates, which should not be a planning concern).
            let filter = execute::plan::resolve_expression(filter_plan, &resolve_context)
                .await
                .map_err(|e| {
                    PlanError::Internal(format!("error resolving permission filter plan: {e}"))
                })?;
            Ok(Some(filter))
        }
    }?;

    let mut ndc_arguments = BTreeMap::new();
    for (argument_name, argument_value) in &model_target.arguments {
        let ndc_argument_name = model_source.argument_mappings.get(argument_name).ok_or_else(|| PlanError::Internal(format!("couldn't fetch argument mapping for argument {argument_name} of model {qualified_model_name}")))?;
        let ndc_argument_value = match argument_value {
            open_dds::query::Value::BooleanExpression(_) => {
                return Err(PlanError::Internal(format!("unexpected boolean expression as value for argument {argument_name} of model {qualified_model_name}")));
            }
            open_dds::query::Value::Literal(value) => value,
        };
        ndc_arguments.insert(ndc_argument_name.clone(), ndc_argument_value.clone());
    }

    let model_filter = model_target
        .filter
        .as_ref()
        .map(|expr| {
            to_resolved_filter_expr(
                metadata,
                &model_source.type_mappings,
                &model.model.data_type,
                model_object_type,
                expr,
            )
        })
        .transpose()?;

    let filter = match (model_filter, permission_filter) {
        (None, filter) | (filter, None) => filter,
        (Some(filter), Some(permission_filter)) => Some(ResolvedFilterExpression::mk_and(vec![
            filter,
            permission_filter,
        ])),
    };

    let order_by_elements = model_target
        .order_by
        .iter()
        .map(|element| {
            to_resolved_order_by_element(
                metadata,
                &model_source.type_mappings,
                &model.model.data_type,
                model_object_type,
                element,
            )
        })
        .collect::<Result<Vec<_>, PlanError>>()?;

    let limit = model_target
        .limit
        .map(u32::try_from)
        .transpose()
        .map_err(|_| PlanError::Internal("limit out of range".into()))?;

    let offset: Option<u32> = model_target
        .offset
        .map(u32::try_from)
        .transpose()
        .map_err(|_| PlanError::Internal("offset out of range".into()))?;

    let query = NDCQuery {
        arguments: ndc_arguments,
        collection_name: model_source.collection.clone(),
        collection_relationships: relationships,
        data_connector: model_source.data_connector.clone(),
        filter,
        limit,
        offset,
        order_by: graphql_ir::ResolvedOrderBy {
            order_by_elements,
            relationships: BTreeMap::new(),
        },
    };

    Ok(query)
}

// take NDCQuery and fields and make a sweet execution plan
pub fn ndc_query_to_query_execution_plan(
    query: &NDCQuery,
    fields: &IndexMap<NdcFieldAlias, Field<ResolvedFilterExpression>>,
) -> QueryExecutionPlan<ResolvedFilterExpression> {
    // move this to `plan`
    QueryExecutionPlan {
        query_node: QueryNode {
            fields: Some(
                fields
                    .iter()
                    .map(|(field_name, field)| (field_name.clone(), field.clone()))
                    .collect(),
            ),
            aggregates: None,
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
