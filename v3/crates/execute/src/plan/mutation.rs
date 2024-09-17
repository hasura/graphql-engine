use crate::error;
use graphql_ir::NdcRelationshipName;
use open_dds::{commands::ProcedureName, types::DataConnectorArgumentName};
use std::collections::BTreeMap;

use super::arguments;
use super::field;
use super::filter;
use super::filter::ResolveFilterExpressionContext;
use super::relationships;

pub type UnresolvedMutationExecutionPlan<'s> =
    MutationExecutionPlan<'s, graphql_ir::Expression<'s>>;
pub type ResolvedMutationExecutionPlan<'s> =
    MutationExecutionPlan<'s, filter::ResolvedFilterExpression>;

#[derive(Debug)]
pub struct MutationExecutionPlan<'s, TFilterExpression> {
    /// The name of a procedure
    pub procedure_name: ProcedureName,
    /// Any named procedure arguments
    pub procedure_arguments:
        BTreeMap<DataConnectorArgumentName, arguments::MutationArgument<TFilterExpression>>,
    /// The fields to return from the result, or null to return everything
    pub procedure_fields: Option<field::NestedField<TFilterExpression>>,
    /// Any relationships between collections involved in the query request
    pub collection_relationships: BTreeMap<NdcRelationshipName, relationships::Relationship>,
    /// The data connector used to fetch the data
    pub data_connector: &'s metadata_resolve::DataConnectorLink,
}

impl<'s> UnresolvedMutationExecutionPlan<'s> {
    pub async fn resolve(
        self,
        resolve_context: &'s ResolveFilterExpressionContext,
    ) -> Result<ResolvedMutationExecutionPlan<'s>, error::FieldError> {
        let MutationExecutionPlan {
            procedure_name,
            procedure_arguments,
            procedure_fields,
            collection_relationships,
            data_connector,
        } = self;

        let resolved_fields = match procedure_fields {
            Some(fields) => Some(fields.resolve(resolve_context).await?),
            None => None,
        };

        let resolved_arguments =
            arguments::resolve_mutation_arguments(resolve_context, procedure_arguments).await?;

        Ok(MutationExecutionPlan {
            procedure_name,
            procedure_arguments: resolved_arguments,
            procedure_fields: resolved_fields,
            collection_relationships,
            data_connector,
        })
    }
}
