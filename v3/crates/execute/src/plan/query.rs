use crate::{error, HttpContext};
use ir::{AggregateSelectionSet, NdcFieldAlias, NdcRelationshipName, OrderByElement, VariableName};

use async_recursion::async_recursion;
use indexmap::IndexMap;
use open_dds::{data_connector::CollectionName, types::DataConnectorArgumentName};
use std::collections::BTreeMap;

use super::arguments;
use super::field;
use super::filter;
use super::relationships;

pub type UnresolvedQueryExecutionPlan<'s> = QueryExecutionPlan<'s, ir::Expression<'s>>;
pub type ResolvedQueryExecutionPlan<'s> = QueryExecutionPlan<'s, filter::ResolvedFilterExpression>;

#[derive(Debug, Clone, PartialEq)]
pub struct QueryExecutionPlan<'s, TFilterExpression> {
    pub query_node: QueryNode<TFilterExpression>,
    /// The name of a collection
    pub collection: CollectionName,
    /// Values to be provided to any collection arguments
    pub arguments: BTreeMap<DataConnectorArgumentName, arguments::Argument<TFilterExpression>>,
    /// Any relationships between collections involved in the query request
    pub collection_relationships: BTreeMap<NdcRelationshipName, relationships::Relationship>,
    /// One set of named variables for each rowset to fetch. Each variable set
    /// should be subtituted in turn, and a fresh set of rows returned.
    pub variables: Option<Vec<BTreeMap<VariableName, serde_json::Value>>>,
    /// The data connector used to fetch the data
    pub data_connector: &'s metadata_resolve::DataConnectorLink,
}

impl<'s> UnresolvedQueryExecutionPlan<'s> {
    pub async fn resolve(
        self,
        http_context: &'s HttpContext,
    ) -> Result<ResolvedQueryExecutionPlan<'s>, error::FieldError> {
        let QueryExecutionPlan {
            query_node,
            collection,
            arguments,
            collection_relationships,
            variables,
            data_connector,
        } = self;
        let query_request = QueryExecutionPlan {
            query_node: query_node.resolve(http_context).await?,
            collection,
            arguments: arguments::resolve_arguments(http_context, arguments).await?,
            collection_relationships,
            variables,
            data_connector,
        };
        Ok(query_request)
    }
}

pub type UnresolvedQueryNode<'s> = QueryNode<ir::Expression<'s>>;
pub type ResolvedQueryNode = QueryNode<filter::ResolvedFilterExpression>;

/// Query plan for fetching data
#[derive(Debug, Clone, PartialEq)]
pub struct QueryNode<TFilterExpression> {
    /// Optionally limit to N results
    pub limit: Option<u32>,
    /// Optionally offset from the Nth result
    pub offset: Option<u32>,
    /// Optionally sort results
    pub order_by: Option<Vec<OrderByElement>>,
    /// Optionally filter results
    pub predicate: Option<TFilterExpression>,
    /// Aggregate fields of the query
    pub aggregates: Option<AggregateSelectionSet>,
    /// Fields of the query
    pub fields: Option<IndexMap<NdcFieldAlias, field::Field<TFilterExpression>>>,
}

impl<'s> UnresolvedQueryNode<'s> {
    #[async_recursion]
    pub async fn resolve(
        self,
        http_context: &'s HttpContext,
    ) -> Result<ResolvedQueryNode, error::FieldError>
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
            Some(predicate) => Some(filter::resolve_expression(predicate, http_context).await?),
            None => None,
        };
        let fields = match fields {
            Some(fields) => {
                let mut ndc_fields_ = IndexMap::new();
                for (name, field) in fields {
                    ndc_fields_.insert(name, field.resolve(http_context).await?);
                }
                Some(ndc_fields_)
            }
            None => None,
        };
        Ok(QueryNode {
            limit,
            offset,
            order_by,
            predicate,
            aggregates,
            fields,
        })
    }
}
