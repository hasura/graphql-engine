use crate::{error, HttpContext};
use async_recursion::async_recursion;
use indexmap::IndexMap;
use ir::{NdcFieldAlias, NdcRelationshipName};
use open_dds::{data_connector::DataConnectorColumnName, types::DataConnectorArgumentName};
use std::collections::BTreeMap;

use super::arguments;
use super::filter;
use super::query;

pub type UnresolvedField<'s> = Field<ir::Expression<'s>>;
pub type ResolvedField<'s> = Field<filter::ResolvedFilterExpression>;

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
        http_context: &'s HttpContext,
    ) -> Result<ResolvedField<'s>, error::FieldError> {
        match self {
            Field::Column {
                column,
                fields,
                arguments,
            } => {
                let resolved_fields = match fields {
                    Some(nested_field) => Some(nested_field.resolve(http_context).await?),
                    None => None,
                };
                Ok(Field::Column {
                    column,
                    fields: resolved_fields,
                    arguments: arguments::resolve_arguments(http_context, arguments).await?,
                })
            }
            Field::Relationship {
                query_node,
                relationship,
                arguments,
            } => {
                let query_node = query_node.resolve(http_context).await?;
                Ok(Field::Relationship {
                    query_node: Box::new(query_node),
                    relationship,
                    arguments: arguments::resolve_arguments(http_context, arguments).await?,
                })
            }
        }
    }
}

pub type UnresolvedNestedField<'s> = NestedField<ir::Expression<'s>>;
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
        http_context: &'s HttpContext,
    ) -> Result<ResolvedNestedField, error::FieldError>
    where
        's: 'async_recursion,
    {
        match self {
            NestedField::Object(nested_object) => Ok(NestedField::Object(
                nested_object.resolve(http_context).await?,
            )),
            NestedField::Array(nested_array) => Ok(NestedField::Array(
                nested_array.resolve(http_context).await?,
            )),
        }
    }
}

pub type UnresolvedNestedObject<'s> = NestedObject<ir::Expression<'s>>;
pub type ResolvedNestedObject = NestedObject<filter::ResolvedFilterExpression>;

#[derive(Debug, Clone, PartialEq)]
pub struct NestedObject<TFilterExpression> {
    pub fields: IndexMap<NdcFieldAlias, Field<TFilterExpression>>,
}

impl<'s> UnresolvedNestedObject<'s> {
    pub async fn resolve(
        self,
        http_context: &'s HttpContext,
    ) -> Result<ResolvedNestedObject, error::FieldError> {
        let mut fields = IndexMap::new();
        for (name, field) in self.fields {
            fields.insert(name, field.resolve(http_context).await?);
        }
        Ok(NestedObject { fields })
    }
}

pub type UnresolvedNestedArray<'s> = NestedArray<ir::Expression<'s>>;
pub type ResolvedNestedArray = NestedArray<filter::ResolvedFilterExpression>;

#[derive(Debug, Clone, PartialEq)]
pub struct NestedArray<TFilterExpression> {
    pub fields: Box<NestedField<TFilterExpression>>,
}

impl<'s> UnresolvedNestedArray<'s> {
    pub async fn resolve(
        self,
        http_context: &'s HttpContext,
    ) -> Result<ResolvedNestedArray, error::FieldError> {
        let fields = self.fields.resolve(http_context).await?;
        Ok(NestedArray {
            fields: Box::new(fields),
        })
    }
}
