mod error;
mod scalar;
mod types;

pub use error::ScalarBooleanExpressionTypeError;
use std::collections::{BTreeMap, BTreeSet};

use lang_graphql::ast::common as ast;
use open_dds::{boolean_expression::BooleanExpressionOperand, types::CustomTypeName};

use crate::stages::{data_connectors, object_types, scalar_types};
use crate::Qualified;

pub use types::{
    IncludeIsNull, ResolvedScalarBooleanExpressionType, ScalarBooleanExpressionsOutput,
};

pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    mut graphql_types: BTreeSet<ast::TypeName>,
    data_connectors: &data_connectors::DataConnectors,
    object_types: &object_types::ObjectTypesWithTypeMappings,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
) -> Result<ScalarBooleanExpressionsOutput, ScalarBooleanExpressionTypeError> {
    let mut raw_boolean_expression_types = BTreeMap::new();

    // first we collect all the boolean_expression_types
    // so we have a full set to refer to when resolving them
    for open_dds::accessor::QualifiedObject {
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
                &boolean_expression_type.is_null,
                subgraph,
                data_connectors,
                object_types,
                scalar_types,
                &boolean_expression_type.graphql,
                &mut graphql_types,
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
    })
}
