use crate::stages::{data_connectors, graphql_config, type_permissions};
use crate::types::error::{BooleanExpressionError, Error};
use crate::types::internal_flags::MetadataResolveFlagsInternal;
use crate::Qualified;
use open_dds::{boolean_expression::BooleanExpressionOperand, types::CustomTypeName};
use std::collections::{BTreeMap, BTreeSet};
mod graphql;
mod object;
mod scalar;
mod types;
use lang_graphql::ast::common as ast;

mod helpers;

pub use types::{
    BooleanExpressionComparableRelationship, BooleanExpressionGraphqlConfig,
    BooleanExpressionGraphqlFieldConfig, BooleanExpressionTypes, BooleanExpressionsOutput,
    ComparisonExpressionInfo, IncludeIsNull, IncludeLogicalOperators,
    ObjectComparisonExpressionInfo, ResolvedObjectBooleanExpressionType,
    ResolvedScalarBooleanExpressionType,
};

pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    flags: MetadataResolveFlagsInternal,
    existing_graphql_types: &BTreeSet<ast::TypeName>,
    graphql_config: &graphql_config::GraphqlConfig,
    data_connectors: &data_connectors::DataConnectors,
    object_types: &BTreeMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
) -> Result<BooleanExpressionsOutput, Error> {
    if !flags.enable_boolean_expression_types
        && !metadata_accessor.boolean_expression_types.is_empty()
    {
        return Err(Error::BooleanExpressionError {
            boolean_expression_error: BooleanExpressionError::NewBooleanExpressionTypesAreDisabled,
        });
    };

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
                &boolean_expression_type.graphql,
            )?;

            boolean_expression_scalar_types.insert(
                boolean_expression_type_name.clone(),
                scalar_boolean_expression_type,
            );
        }
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
                &boolean_expression_scalar_types,
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
            scalars: boolean_expression_scalar_types,
        },
        graphql_types,
    })
}
