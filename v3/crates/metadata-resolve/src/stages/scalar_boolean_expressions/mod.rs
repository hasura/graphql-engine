mod error;
mod scalar;
mod types;

pub use error::{ScalarBooleanExpressionTypeError, ScalarBooleanExpressionTypeIssue};
use std::collections::{BTreeMap, BTreeSet};

use lang_graphql::ast::common as ast;
use open_dds::{boolean_expression::BooleanExpressionOperand, types::CustomTypeName};

use crate::stages::{data_connectors, graphql_config, object_types, scalar_types};
use crate::Qualified;

pub use scalar::resolve_logical_operators;
pub use types::{
    IsNullOperator, LogicalOperators, LogicalOperatorsGraphqlConfig,
    ResolvedScalarBooleanExpressionType, ScalarBooleanExpressionsOutput,
};

pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    mut graphql_types: BTreeSet<ast::TypeName>,
    data_connectors: &data_connectors::DataConnectors,
    object_types: &object_types::ObjectTypesWithTypeMappings,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    graphql_config: &graphql_config::GraphqlConfig,
) -> Result<ScalarBooleanExpressionsOutput, ScalarBooleanExpressionTypeError> {
    let mut raw_boolean_expression_types = BTreeMap::new();
    let mut issues = vec![];

    // first we collect all the boolean_expression_types
    // so we have a full set to refer to when resolving them
    for open_dds::accessor::QualifiedObject {
        path: _,
        subgraph,
        object: boolean_expression_type,
    } in &metadata_accessor.boolean_expression_types
    {
        raw_boolean_expression_types.insert(
            Qualified::new(subgraph.clone(), boolean_expression_type.name.clone()),
            (subgraph, boolean_expression_type),
        );
    }

    // let's resolve scalar types first

    let mut boolean_expression_scalar_types = BTreeMap::new();

    for (boolean_expression_type_name, (subgraph, boolean_expression_type)) in
        &raw_boolean_expression_types
    {
        if let BooleanExpressionOperand::Scalar(boolean_expression_scalar_operand) =
            &boolean_expression_type.operand
        {
            let scalar_boolean_expression_type = scalar::resolve_scalar_boolean_expression_type(
                boolean_expression_type_name,
                boolean_expression_scalar_operand,
                boolean_expression_type,
                subgraph,
                data_connectors,
                object_types,
                scalar_types,
                graphql_config,
                &metadata_accessor.flags,
                &mut graphql_types,
                &mut issues,
            )?;

            boolean_expression_scalar_types.insert(
                boolean_expression_type_name.clone(),
                scalar_boolean_expression_type,
            );
        }
    }

    Ok(ScalarBooleanExpressionsOutput {
        boolean_expression_scalar_types,
        // TODO: make sure we are adding new types to graphql_types
        graphql_types,
        issues,
    })
}
