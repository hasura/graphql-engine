use std::sync::Arc;

use crate::error;
use async_recursion::async_recursion;
use graphql_ir::{AggregateSelectionSet, NdcRelationshipName, OrderByElement, VariableName};
use indexmap::IndexMap;
use open_dds::{data_connector::CollectionName, types::DataConnectorArgumentName};
use plan_types::NdcFieldAlias;
use std::collections::BTreeMap;

use super::arguments;
use super::field;
use super::filter;
use super::filter::ResolveFilterExpressionContext;
use super::relationships;

pub type UnresolvedQueryExecutionPlan<'s> = QueryExecutionPlan<graphql_ir::Expression<'s>>;
pub type ResolvedQueryExecutionPlan = QueryExecutionPlan<filter::ResolvedFilterExpression>;

#[derive(Debug, Clone, PartialEq)]
pub struct QueryExecutionPlan<TFilterExpression> {
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
    pub data_connector: Arc<metadata_resolve::DataConnectorLink>,
}

impl<'s> UnresolvedQueryExecutionPlan<'s> {
    pub async fn resolve(
        self,
        resolve_context: &ResolveFilterExpressionContext<'_>,
    ) -> Result<ResolvedQueryExecutionPlan, error::FieldError> {
        let QueryExecutionPlan {
            query_node,
            collection,
            arguments,
            collection_relationships,
            variables,
            data_connector,
        } = self;
        let query_request = QueryExecutionPlan {
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

pub type UnresolvedQueryNode<'s> = QueryNode<graphql_ir::Expression<'s>>;
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
        resolve_context: &'s ResolveFilterExpressionContext<'_>,
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
