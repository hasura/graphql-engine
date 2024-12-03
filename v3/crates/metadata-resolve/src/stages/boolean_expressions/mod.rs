mod error;
mod graphql;
mod helpers;
mod object;
mod types;
pub use error::BooleanExpressionError;

use std::collections::{BTreeMap, BTreeSet};

use lang_graphql::ast::common as ast;
use open_dds::{boolean_expression::BooleanExpressionOperand, types::CustomTypeName};

use crate::stages::{graphql_config, relationships, scalar_boolean_expressions, type_permissions};
use crate::Qualified;

pub use types::{
    BooleanExpressionComparableRelationship, BooleanExpressionGraphqlConfig,
    BooleanExpressionGraphqlFieldConfig, BooleanExpressionIssue, BooleanExpressionTypes,
    BooleanExpressionsOutput, ComparableFieldKind, ComparisonExpressionInfo,
    IncludeLogicalOperators, ObjectBooleanExpressionGraphqlConfig, ObjectComparisonExpressionInfo,
    ObjectComparisonKind, OperatorMapping, ResolvedObjectBooleanExpressionType,
    ResolvedObjectBooleanExpressionTypeFields, ScalarBooleanExpressionGraphqlConfig,
};

use super::aggregate_boolean_expressions;

pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    boolean_expression_scalar_types: BTreeMap<
        Qualified<CustomTypeName>,
        scalar_boolean_expressions::ResolvedScalarBooleanExpressionType,
    >,
    boolean_expression_object_aggregate_types: BTreeMap<
        Qualified<CustomTypeName>,
        aggregate_boolean_expressions::ObjectAggregateBooleanExpression,
    >,
    boolean_expression_scalar_aggregate_types: BTreeMap<
        Qualified<CustomTypeName>,
        aggregate_boolean_expressions::ScalarAggregateBooleanExpression,
    >,
    mut graphql_types: BTreeSet<ast::TypeName>,
    graphql_config: &graphql_config::GraphqlConfig,
    object_types: &type_permissions::ObjectTypesWithPermissions,
    relationships: &relationships::Relationships,
    flags: &open_dds::flags::Flags,
) -> Result<BooleanExpressionsOutput, BooleanExpressionError> {
    let mut raw_boolean_expression_types = BTreeMap::new();
    let mut issues = Vec::new();
    let mut raw_models = BTreeMap::new();
    let mut object_boolean_expression_type_names = BTreeSet::new();

    // collect raw models for use in resolving relationships
    for open_dds::accessor::QualifiedObject {
        path: _,
        subgraph,
        object: model,
    } in &metadata_accessor.models
    {
        raw_models.insert(
            Qualified::new(subgraph.clone(), model.name().clone()),
            model,
        );
    }

    // collect raw object_boolean_expression_type names for validation
    for open_dds::accessor::QualifiedObject {
        path: _,
        subgraph,
        object: object_boolean_expression_type,
    } in &metadata_accessor.object_boolean_expression_types
    {
        object_boolean_expression_type_names.insert(Qualified::new(
            subgraph.clone(),
            object_boolean_expression_type.name.clone(),
        ));
    }

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

    let mut boolean_expression_object_types = BTreeMap::new();

    for (boolean_expression_type_name, (subgraph, boolean_expression_type)) in
        &raw_boolean_expression_types
    {
        if let BooleanExpressionOperand::Object(boolean_expression_object_operand) =
            &boolean_expression_type.operand
        {
            let (object_boolean_expression_type, boolean_expression_issues) =
                object::resolve_object_boolean_expression_type(
                    boolean_expression_type_name,
                    boolean_expression_object_operand,
                    &boolean_expression_type.logical_operators,
                    subgraph,
                    boolean_expression_type.graphql.as_ref(),
                    object_types,
                    &boolean_expression_scalar_types,
                    &raw_boolean_expression_types,
                    relationships,
                    &raw_models,
                    &object_boolean_expression_type_names,
                    graphql_config,
                    &mut graphql_types,
                    flags,
                )?;

            issues.extend(boolean_expression_issues);
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
            object_aggregates: boolean_expression_object_aggregate_types,
            scalar_aggregates: boolean_expression_scalar_aggregate_types,
        },
        // TODO: make sure we are adding new types to graphql_types
        graphql_types,
        issues,
    })
}
