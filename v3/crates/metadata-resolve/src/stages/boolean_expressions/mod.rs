mod error;
mod graphql;
mod helpers;
mod object;
mod types;
pub use error::BooleanExpressionError;

use std::collections::{BTreeMap, BTreeSet};

use lang_graphql::ast::common as ast;
use open_dds::{boolean_expression::BooleanExpressionOperand, types::CustomTypeName};

use crate::stages::{graphql_config, scalar_boolean_expressions, type_permissions};
use crate::Qualified;

pub use types::{
    BooleanExpressionComparableRelationship, BooleanExpressionGraphqlConfig,
    BooleanExpressionGraphqlFieldConfig, BooleanExpressionTypes, BooleanExpressionsOutput,
    ComparisonExpressionInfo, IncludeLogicalOperators, ObjectComparisonExpressionInfo,
    ResolvedObjectBooleanExpressionType,
};

pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    boolean_expression_scalar_types: &BTreeMap<
        Qualified<CustomTypeName>,
        scalar_boolean_expressions::ResolvedScalarBooleanExpressionType,
    >,
    existing_graphql_types: &BTreeSet<ast::TypeName>,
    graphql_config: &graphql_config::GraphqlConfig,
    object_types: &type_permissions::ObjectTypesWithPermissions,
) -> Result<BooleanExpressionsOutput, BooleanExpressionError> {
    let mut raw_boolean_expression_types = BTreeMap::new();

    // TODO: make sure we are adding new types here, we are almost certainly not doing this atm
    let graphql_types = existing_graphql_types.clone();

    // first we collect all the boolean_expression_types
    // so we have a full set to refer to when resolving them
    for open_dds::accessor::QualifiedObject {
        subgraph,
        object: boolean_expression_type,
    } in &metadata_accessor.boolean_expression_types
    {
        raw_boolean_expression_types.insert(
            Qualified::new(subgraph.to_string(), boolean_expression_type.name.clone()),
            (subgraph, boolean_expression_type),
        );
    }

    let mut boolean_expression_object_types = BTreeMap::new();

    for (boolean_expression_type_name, (subgraph, boolean_expression_type)) in
        &raw_boolean_expression_types
    {
        if let BooleanExpressionOperand::Object(boolean_expression_object_operand) =
            &boolean_expression_type.operand
        {
            let object_boolean_expression_type = object::resolve_object_boolean_expression_type(
                boolean_expression_type_name,
                boolean_expression_object_operand,
                &boolean_expression_type.logical_operators,
                subgraph,
                &boolean_expression_type.graphql,
                object_types,
                boolean_expression_scalar_types,
                &raw_boolean_expression_types,
                graphql_config,
            )?;

            boolean_expression_object_types.insert(
                boolean_expression_type_name.clone(),
                object_boolean_expression_type,
            );
        }
    }

    Ok(BooleanExpressionsOutput {
        boolean_expression_types: BooleanExpressionTypes {
            objects: boolean_expression_object_types,
            scalars: boolean_expression_scalar_types.clone(),
        },
        graphql_types,
    })
}
