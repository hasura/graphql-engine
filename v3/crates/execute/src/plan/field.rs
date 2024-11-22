use crate::error;
use async_recursion::async_recursion;
use indexmap::IndexMap;
use open_dds::{data_connector::DataConnectorColumnName, types::DataConnectorArgumentName};
use plan_types::NdcFieldAlias;
use plan_types::NdcRelationshipName;
use std::collections::BTreeMap;

use super::arguments;
use super::filter::ResolveFilterExpressionContext;
use super::query;

pub type UnresolvedField<'s> = Field<'s>;

/// Field plan
#[derive(Debug, Clone, PartialEq)]
pub enum Field<'s> {
    Column {
        /// Column
        column: DataConnectorColumnName,
        /// Nested fields if column is array or object type
        fields: Option<NestedField<'s>>,
        /// Input field arguments
        arguments: BTreeMap<DataConnectorArgumentName, arguments::Argument<'s>>,
    },
    Relationship {
        /// The relationship query
        query_node: Box<query::QueryNode<'s>>,
        /// The name of the relationship to follow for the subquery
        relationship: NdcRelationshipName,
        /// Values to be provided to any collection arguments
        arguments: BTreeMap<DataConnectorArgumentName, arguments::Argument<'s>>,
    },
}

impl<'s> Field<'s> {
    /// Resolve field plan into NDC field
    pub async fn resolve(
        self,
        resolve_context: &'s ResolveFilterExpressionContext<'_>,
    ) -> Result<plan_types::Field, error::FieldError> {
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
                Ok(plan_types::Field::Column {
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
                Ok(plan_types::Field::Relationship {
                    query_node: Box::new(query_node),
                    relationship,
                    arguments: arguments::resolve_arguments(resolve_context, arguments).await?,
                })
            }
        }
    }
}

pub type UnresolvedNestedField<'s> = NestedField<'s>;

#[derive(Debug, Clone, PartialEq)]
pub enum NestedField<'s> {
    Object(NestedObject<'s>),
    Array(NestedArray<'s>),
}

impl<'s> NestedField<'s> {
    #[async_recursion]
    pub async fn resolve(
        self,
        resolve_context: &'s ResolveFilterExpressionContext,
    ) -> Result<plan_types::NestedField, error::FieldError>
    where
        's: 'async_recursion,
    {
        match self {
            NestedField::Object(nested_object) => Ok(plan_types::NestedField::Object(
                nested_object.resolve(resolve_context).await?,
            )),
            NestedField::Array(nested_array) => Ok(plan_types::NestedField::Array(
                nested_array.resolve(resolve_context).await?,
            )),
        }
    }
}

pub type UnresolvedNestedObject<'s> = NestedObject<'s>;

#[derive(Debug, Clone, PartialEq)]
pub struct NestedObject<'s> {
    pub fields: IndexMap<NdcFieldAlias, Field<'s>>,
}

impl<'s> NestedObject<'s> {
    pub async fn resolve(
        self,
        resolve_context: &'s ResolveFilterExpressionContext<'_>,
    ) -> Result<plan_types::NestedObject, error::FieldError> {
        let mut fields = IndexMap::new();
        for (name, field) in self.fields {
            fields.insert(name, field.resolve(resolve_context).await?);
        }
        Ok(plan_types::NestedObject { fields })
    }
}

pub type UnresolvedNestedArray<'s> = NestedArray<'s>;

#[derive(Debug, Clone, PartialEq)]
pub struct NestedArray<'s> {
    pub fields: Box<NestedField<'s>>,
}

impl<'s> NestedArray<'s> {
    pub async fn resolve(
        self,
        resolve_context: &'s ResolveFilterExpressionContext<'_>,
    ) -> Result<plan_types::NestedArray, error::FieldError> {
        let fields = self.fields.resolve(resolve_context).await?;
        Ok(plan_types::NestedArray {
            fields: Box::new(fields),
        })
    }
}
