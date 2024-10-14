use crate::error;
use async_recursion::async_recursion;
use indexmap::IndexMap;
use open_dds::{data_connector::DataConnectorColumnName, types::DataConnectorArgumentName};
use plan_types::NdcFieldAlias;
use plan_types::NdcRelationshipName;
use std::collections::BTreeMap;

use super::arguments;
use super::filter;
use super::filter::ResolveFilterExpressionContext;
use super::query;

pub type UnresolvedField<'s> = Field<plan_types::Expression<'s>>;
pub type ResolvedField = Field<filter::ResolvedFilterExpression>;

/// Field plan
#[derive(Debug, Clone, PartialEq)]
pub enum Field<TFilterExpression> {
    Column {
        /// Column
        column: DataConnectorColumnName,
        /// Nested fields if column is array or object type
        fields: Option<NestedField<TFilterExpression>>,
        /// Input field arguments
        arguments: BTreeMap<DataConnectorArgumentName, arguments::Argument<TFilterExpression>>,
    },
    Relationship {
        /// The relationship query
        query_node: Box<query::QueryNode<TFilterExpression>>,
        /// The name of the relationship to follow for the subquery
        relationship: NdcRelationshipName,
        /// Values to be provided to any collection arguments
        arguments: BTreeMap<DataConnectorArgumentName, arguments::Argument<TFilterExpression>>,
    },
}

impl<'s> UnresolvedField<'s> {
    /// Resolve field plan into NDC field
    pub async fn resolve(
        self,
        resolve_context: &'s ResolveFilterExpressionContext<'_>,
    ) -> Result<ResolvedField, error::FieldError> {
        match self {
            Field::Column {
                column,
                fields,
                arguments,
            } => {
                let resolved_fields = match fields {
                    Some(nested_field) => Some(nested_field.resolve(resolve_context).await?),
                    None => None,
                };
                Ok(Field::Column {
                    column,
                    fields: resolved_fields,
                    arguments: arguments::resolve_arguments(resolve_context, arguments).await?,
                })
            }
            Field::Relationship {
                query_node,
                relationship,
                arguments,
            } => {
                let query_node = query_node.resolve(resolve_context).await?;
                Ok(Field::Relationship {
                    query_node: Box::new(query_node),
                    relationship,
                    arguments: arguments::resolve_arguments(resolve_context, arguments).await?,
                })
            }
        }
    }
}

pub type UnresolvedNestedField<'s> = NestedField<plan_types::Expression<'s>>;
pub type ResolvedNestedField = NestedField<filter::ResolvedFilterExpression>;

#[derive(Debug, Clone, PartialEq)]
pub enum NestedField<TFilterExpression> {
    Object(NestedObject<TFilterExpression>),
    Array(NestedArray<TFilterExpression>),
}

impl<'s> UnresolvedNestedField<'s> {
    #[async_recursion]
    pub async fn resolve(
        self,
        resolve_context: &'s ResolveFilterExpressionContext,
    ) -> Result<ResolvedNestedField, error::FieldError>
    where
        's: 'async_recursion,
    {
        match self {
            NestedField::Object(nested_object) => Ok(NestedField::Object(
                nested_object.resolve(resolve_context).await?,
            )),
            NestedField::Array(nested_array) => Ok(NestedField::Array(
                nested_array.resolve(resolve_context).await?,
            )),
        }
    }
}

pub type UnresolvedNestedObject<'s> = NestedObject<plan_types::Expression<'s>>;
pub type ResolvedNestedObject = NestedObject<filter::ResolvedFilterExpression>;

#[derive(Debug, Clone, PartialEq)]
pub struct NestedObject<TFilterExpression> {
    pub fields: IndexMap<NdcFieldAlias, Field<TFilterExpression>>,
}

impl<'s> UnresolvedNestedObject<'s> {
    pub async fn resolve(
        self,
        resolve_context: &'s ResolveFilterExpressionContext<'_>,
    ) -> Result<ResolvedNestedObject, error::FieldError> {
        let mut fields = IndexMap::new();
        for (name, field) in self.fields {
            fields.insert(name, field.resolve(resolve_context).await?);
        }
        Ok(NestedObject { fields })
    }
}

pub type UnresolvedNestedArray<'s> = NestedArray<plan_types::Expression<'s>>;
pub type ResolvedNestedArray = NestedArray<filter::ResolvedFilterExpression>;

#[derive(Debug, Clone, PartialEq)]
pub struct NestedArray<TFilterExpression> {
    pub fields: Box<NestedField<TFilterExpression>>,
}

impl<'s> UnresolvedNestedArray<'s> {
    pub async fn resolve(
        self,
        resolve_context: &'s ResolveFilterExpressionContext<'_>,
    ) -> Result<ResolvedNestedArray, error::FieldError> {
        let fields = self.fields.resolve(resolve_context).await?;
        Ok(NestedArray {
            fields: Box::new(fields),
        })
    }
}
