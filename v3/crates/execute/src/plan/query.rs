use std::sync::Arc;

use crate::error;
use async_recursion::async_recursion;
use indexmap::IndexMap;
use open_dds::{data_connector::CollectionName, types::DataConnectorArgumentName};
use plan_types::{
    AggregateSelectionSet, NdcFieldAlias, NdcRelationshipName, OrderByElement, PredicateQueryTrees,
    Relationship, VariableName,
};
use std::collections::BTreeMap;

use super::arguments;
use super::field;
use super::filter;
use super::filter::ResolveFilterExpressionContext;

pub type UnresolvedQueryExecutionPlan<'s> = QueryExecutionPlan<'s>;

#[derive(Debug, Clone, PartialEq)]
pub struct QueryExecutionPlan<'s> {
    pub remote_predicates: PredicateQueryTrees,
    pub query_node: QueryNode<'s>,
    /// The name of a collection
    pub collection: CollectionName,
    /// Values to be provided to any collection arguments
    pub arguments: BTreeMap<DataConnectorArgumentName, arguments::Argument<'s>>,
    /// Any relationships between collections involved in the query request
    pub collection_relationships: BTreeMap<NdcRelationshipName, Relationship>,
    /// One set of named variables for each rowset to fetch. Each variable set
    /// should be subtituted in turn, and a fresh set of rows returned.
    pub variables: Option<Vec<BTreeMap<VariableName, serde_json::Value>>>,
    /// The data connector used to fetch the data
    pub data_connector: Arc<metadata_resolve::DataConnectorLink>,
}

impl<'s> QueryExecutionPlan<'s> {
    pub async fn resolve(
        self,
        resolve_context: &ResolveFilterExpressionContext<'_>,
    ) -> Result<plan_types::QueryExecutionPlan, error::FieldError> {
        let QueryExecutionPlan {
            remote_predicates,
            query_node,
            collection,
            arguments,
            collection_relationships,
            variables,
            data_connector,
        } = self;
        let query_request = plan_types::QueryExecutionPlan {
            remote_predicates,
            query_node: query_node.resolve(resolve_context).await?,
            collection,
            arguments: arguments::resolve_arguments(resolve_context, arguments).await?,
            collection_relationships,
            variables,
            data_connector,
        };
        Ok(query_request)
    }
}

pub type UnresolvedQueryNode<'s> = QueryNode<'s>;

/// Query plan for fetching data
#[derive(Debug, Clone, PartialEq)]
pub struct QueryNode<'s> {
    /// Optionally limit to N results
    pub limit: Option<u32>,
    /// Optionally offset from the Nth result
    pub offset: Option<u32>,
    /// Optionally sort results
    pub order_by: Option<Vec<OrderByElement>>,
    /// Optionally filter results
    pub predicate: Option<plan_types::Expression<'s>>,
    /// Aggregate fields of the query
    pub aggregates: Option<AggregateSelectionSet>,
    /// Fields of the query
    pub fields: Option<IndexMap<NdcFieldAlias, field::Field<'s>>>,
}

impl<'s> QueryNode<'s> {
    #[async_recursion]
    pub async fn resolve(
        self,
        resolve_context: &'s ResolveFilterExpressionContext<'_>,
    ) -> Result<plan_types::QueryNodeNew, error::FieldError>
    where
        's: 'async_recursion,
    {
        let QueryNode {
            limit,
            offset,
            order_by,
            predicate,
            aggregates,
            fields,
        } = self;
        let predicate = match predicate {
            Some(predicate) => Some(filter::resolve_expression(predicate, resolve_context).await?),
            None => None,
        };
        let fields = match fields {
            Some(fields) => {
                let mut ndc_fields_ = IndexMap::new();
                for (name, field) in fields {
                    ndc_fields_.insert(name, field.resolve(resolve_context).await?);
                }
                Some(ndc_fields_)
            }
            None => None,
        };
        Ok(plan_types::QueryNodeNew {
            limit,
            offset,
            order_by,
            predicate,
            aggregates,
            fields: fields.map(|fields| plan_types::FieldsSelection { fields }),
        })
    }
}
