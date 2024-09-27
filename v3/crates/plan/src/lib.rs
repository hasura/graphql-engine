use execute::plan::field::Field;
use indexmap::IndexMap;
use metadata_resolve::{Metadata, ModelWithPermissions, Qualified, TypeMapping};
use open_dds::query::{
    ModelSelection, ObjectFieldSelection, ObjectFieldTarget, ObjectSubSelection, Query,
    QueryRequest,
};
use open_dds::types::CustomTypeName;
use plan_types::NdcFieldAlias;
use std::collections::BTreeMap;
use std::sync::Arc;

// this is the thing we do
pub fn plan_query_request<'req, 'metadata>(
    query_request: &'req QueryRequest,
    metadata: &'metadata Metadata,
) -> Result<(execute::UnresolvedQueryExecutionPlan<'req>, QueryContext), PlanError>
where
    'metadata: 'req,
{
    let QueryRequest::V1(query_request_v1) = query_request;

    // to limit scope, let's assume there's one item and explode otherwise
    let (_alias, query) = query_request_v1.queries.first().unwrap();

    // return plan for a single query (again, wrong, but let's unblock ourselves for now)
    query_to_plan(query, metadata)
}

#[derive(Debug, derive_more::Display)]
pub enum PlanError {
    InternalError(String),
}

// additional query context which is helpful when processing the response afterwards
pub struct QueryContext {
    pub type_name: Qualified<CustomTypeName>,
}

fn query_to_plan<'req, 'metadata>(
    query: &'req Query,
    metadata: &'metadata Metadata,
) -> Result<(execute::UnresolvedQueryExecutionPlan<'req>, QueryContext), PlanError>
where
    'metadata: 'req,
{
    match query {
        open_dds::query::Query::Model(model_query) => model_query_to_plan(model_query, metadata),
        _ => Err(PlanError::InternalError(
            "Model requests supported only".to_string(),
        )),
    }
}

fn get_limit(model_selection: &ModelSelection) -> Result<Option<u32>, PlanError> {
    model_selection
        .target
        .limit
        .map(u32::try_from)
        .transpose()
        .map_err(|e| PlanError::InternalError(e.to_string()))
}

fn get_fields<'metadata>(
    model_selection: &ModelSelection,
    type_mappings: &'metadata TypeMapping,
) -> Result<Option<IndexMap<NdcFieldAlias, Field<graphql_ir::Expression<'metadata>>>>, PlanError> {
    let mut fields = IndexMap::new();

    for (alias, field) in &model_selection.selection {
        match field {
            ObjectSubSelection::Field(ObjectFieldSelection { target, selection }) => {
                if selection.is_some() {
                    return Err(PlanError::InternalError(
                        "sub selections not supported yet".to_string(),
                    ));
                }

                let ObjectFieldTarget {
                    field_name,
                    arguments,
                } = target;

                let open_dd_field_name = {
                    if !arguments.is_empty() {
                        return Err(PlanError::InternalError(
                            "Field arguments not yet supported".to_string(),
                        ));
                    }
                    field_name
                };

                let ndc_field_alias = NdcFieldAlias::from(alias.as_str());
                let data_connector_column_name = match type_mappings {
                    TypeMapping::Object { field_mappings, .. } => field_mappings
                        .get(open_dd_field_name)
                        .ok_or_else(|| {
                            PlanError::InternalError(
                                "Open DD field name lookup not found".to_string(),
                            )
                        })?
                        .column
                        .clone(),
                };
                let ndc_field = Field::Column {
                    arguments: BTreeMap::new(),
                    column: data_connector_column_name,
                    fields: None,
                };
                fields.insert(ndc_field_alias, ndc_field);
            }
            ObjectSubSelection::Relationship(_) | ObjectSubSelection::RelationshipAggregate(_) => {
                return Err(PlanError::InternalError(
                    "get_fields is only implemented for plain fields".to_string(),
                ));
            }
        }
    }
    Ok(Some(fields))
}

pub fn model_query_to_plan<'req, 'metadata>(
    model_selection: &'req ModelSelection,
    metadata: &'metadata Metadata,
) -> Result<(execute::UnresolvedQueryExecutionPlan<'req>, QueryContext), PlanError>
where
    'metadata: 'req,
{
    let qualified_model_name = Qualified::new(
        model_selection.target.subgraph.clone(),
        model_selection.target.model_name.clone(),
    );

    let model = metadata
        .models
        .get(&qualified_model_name)
        .ok_or_else(|| PlanError::InternalError("Could not find model".to_string()))?;

    // TODO: proper internal error, but we should not be exposing models without a source
    let model_source = model
        .model
        .source
        .as_ref()
        .ok_or_else(|| PlanError::InternalError("Model source not found".to_string()))?;

    let object_type = metadata
        .object_types
        .get(&model.model.data_type)
        .ok_or_else(|| PlanError::InternalError("Underlying object type not found".to_string()))?;

    let data_connector_mappings = object_type
        .type_mappings
        .get(
            &model_source.data_connector.name,
            &model_source.collection_type,
        )
        .ok_or_else(|| {
            PlanError::InternalError("Data connector type mappings not found".to_string())
        })?;

    let query_node = execute::QueryNode {
        // Optionally limit to N results
        limit: get_limit(model_selection)?,
        // Optionally offset from the Nth result
        offset: None,
        // Optionally sort results
        order_by: None,
        // Optionally filter results
        predicate: None,
        // Aggregate fields of the query
        aggregates: None,
        // Fields of the query
        fields: get_fields(model_selection, data_connector_mappings)?,
    };

    let query_context = QueryContext {
        type_name: model.model.data_type.clone(),
    };

    let query_execution_plan = execute::QueryExecutionPlan {
        query_node,
        // The name of a collection
        collection: model_source.collection.clone(),
        // Values to be provided to any collection arguments
        arguments: BTreeMap::new(),
        // Any relationships between collections involved in the query request
        collection_relationships: BTreeMap::new(),
        // One set of named variables for each rowset to fetch. Each variable set
        // should be subtituted in turn, and a fresh set of rows returned.
        variables: None,
        // The data connector used to fetch the data
        data_connector: lookup_data_connector(model)?,
    };

    Ok((query_execution_plan, query_context))
}

fn lookup_data_connector(
    model: &ModelWithPermissions,
) -> Result<Arc<metadata_resolve::DataConnectorLink>, PlanError> {
    match &model.model.source {
        Some(source) => Ok(source.data_connector.clone()),
        None => Err(PlanError::InternalError(
            "Data connector not found".to_string(),
        )),
    }
}
