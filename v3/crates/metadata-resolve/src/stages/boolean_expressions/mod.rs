mod error;
mod graphql;
mod helpers;
mod legacy;
mod object;
mod types;
pub use error::BooleanExpressionError;
use open_dds::identifier::SubgraphName;
use open_dds::models::ModelName;
use std::collections::{BTreeMap, BTreeSet};

use open_dds::{
    boolean_expression::BooleanExpressionOperand, data_connector::DataConnectorName,
    types::CustomTypeName,
};

use super::aggregate_boolean_expressions;
use crate::Qualified;
use crate::stages::{
    data_connector_scalar_types, data_connectors, graphql_config, relationships,
    scalar_boolean_expressions, type_permissions,
};
pub use types::{
    BooleanExpressionComparableRelationship, BooleanExpressionGraphqlConfig,
    BooleanExpressionGraphqlFieldConfig, BooleanExpressionIssue, BooleanExpressionTypeIdentifier,
    BooleanExpressionTypes, BooleanExpressionsOutput, ComparableFieldKind,
    ComparableRelationshipExecutionStrategy, ComparisonExpressionInfo, DataConnectorType,
    IncludeLogicalOperators, ObjectBooleanExpressionDataConnector,
    ObjectBooleanExpressionGraphqlConfig, ObjectComparisonExpressionInfo, ObjectComparisonKind,
    OperatorMapping, ResolvedObjectBooleanExpressionType,
    ResolvedObjectBooleanExpressionTypeFields, ScalarBooleanExpressionGraphqlConfig,
    ScalarComparisonKind,
};

pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    boolean_expression_scalar_types: BTreeMap<
        BooleanExpressionTypeIdentifier,
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
    data_connectors: &data_connectors::DataConnectors,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars,
    >,
    graphql_config: &graphql_config::GraphqlConfig,
    object_types: &type_permissions::ObjectTypesWithPermissions,
    relationships: &relationships::Relationships,
    graphql_types: &mut graphql_config::GraphqlTypeNames,
) -> Result<BooleanExpressionsOutput, Vec<BooleanExpressionError>> {
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
        let type_name = Qualified::new(subgraph.clone(), boolean_expression_type.name.clone());
        if raw_boolean_expression_types
            .insert(type_name.clone(), (subgraph, boolean_expression_type))
            .is_some()
        {
            issues.push(BooleanExpressionIssue::DuplicateBooleanExpressionType { type_name });
        }
    }
    let mut results = vec![];
    let mut boolean_expression_object_types = BTreeMap::new();

    // collect raw object_boolean_expression_type names for validation
    for open_dds::accessor::QualifiedObject {
        path: _,
        subgraph,
        object: object_boolean_expression_type,
    } in &metadata_accessor.object_boolean_expression_types
    {
        results.push(resolve_legacy_object_boolean_expression_type(
            subgraph,
            object_boolean_expression_type,
            data_connectors,
            data_connector_scalars,
            object_types,
            &boolean_expression_scalar_types,
            &raw_boolean_expression_types,
            relationships,
            &raw_models,
            &object_boolean_expression_type_names,
            graphql_config,
            metadata_accessor,
            &mut boolean_expression_object_types,
            &mut issues,
            graphql_types,
        ));
    }

    for (boolean_expression_type_name, (subgraph, boolean_expression_type)) in
        &raw_boolean_expression_types
    {
        results.push(resolve_object_boolean_expression_type(
            boolean_expression_type_name,
            subgraph,
            boolean_expression_type,
            metadata_accessor,
            object_types,
            &boolean_expression_scalar_types,
            &raw_boolean_expression_types,
            relationships,
            &raw_models,
            &object_boolean_expression_type_names,
            graphql_config,
            &mut boolean_expression_object_types,
            &mut issues,
            graphql_types,
        ));
    }

    partition_eithers::collect_any_errors(results).map(|_| BooleanExpressionsOutput {
        boolean_expression_types: BooleanExpressionTypes {
            objects: boolean_expression_object_types,
            scalars: boolean_expression_scalar_types,
            object_aggregates: boolean_expression_object_aggregate_types,
            scalar_aggregates: boolean_expression_scalar_aggregate_types,
        },
        // TODO: make sure we are adding new types to graphql_types
        issues,
    })
}

fn resolve_object_boolean_expression_type(
    boolean_expression_type_name: &Qualified<CustomTypeName>,
    subgraph: &SubgraphName,
    boolean_expression_type: &open_dds::boolean_expression::BooleanExpressionTypeV1,
    metadata_accessor: &open_dds::accessor::MetadataAccessor,

    object_types: &type_permissions::ObjectTypesWithPermissions,
    boolean_expression_scalar_types: &BTreeMap<
        BooleanExpressionTypeIdentifier,
        scalar_boolean_expressions::ResolvedScalarBooleanExpressionType,
    >,
    raw_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        (
            &SubgraphName,
            &open_dds::boolean_expression::BooleanExpressionTypeV1,
        ),
    >,
    relationships: &relationships::Relationships,
    raw_models: &BTreeMap<Qualified<ModelName>, &open_dds::models::Model>,
    object_boolean_expression_type_names: &BTreeSet<Qualified<CustomTypeName>>,
    graphql_config: &graphql_config::GraphqlConfig,
    boolean_expression_object_types: &mut BTreeMap<
        Qualified<CustomTypeName>,
        ResolvedObjectBooleanExpressionType,
    >,
    issues: &mut Vec<BooleanExpressionIssue>,
    graphql_types: &mut graphql_config::GraphqlTypeNames,
) -> Result<(), BooleanExpressionError> {
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
                boolean_expression_scalar_types,
                raw_boolean_expression_types,
                relationships,
                raw_models,
                object_boolean_expression_type_names,
                graphql_config,
                &metadata_accessor.flags,
                graphql_types,
            )?;

        issues.extend(boolean_expression_issues);
        boolean_expression_object_types.insert(
            boolean_expression_type_name.clone(),
            object_boolean_expression_type,
        );
    }
    Ok(())
}

fn resolve_legacy_object_boolean_expression_type(
    subgraph: &SubgraphName,
    object_boolean_expression_type: &open_dds::types::ObjectBooleanExpressionTypeV1,
    data_connectors: &data_connectors::DataConnectors,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars,
    >,
    object_types: &type_permissions::ObjectTypesWithPermissions,
    boolean_expression_scalar_types: &BTreeMap<
        BooleanExpressionTypeIdentifier,
        scalar_boolean_expressions::ResolvedScalarBooleanExpressionType,
    >,
    raw_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        (
            &SubgraphName,
            &open_dds::boolean_expression::BooleanExpressionTypeV1,
        ),
    >,
    relationships: &relationships::Relationships,
    raw_models: &BTreeMap<Qualified<ModelName>, &open_dds::models::Model>,
    object_boolean_expression_type_names: &BTreeSet<Qualified<CustomTypeName>>,
    graphql_config: &graphql_config::GraphqlConfig,
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    boolean_expression_object_types: &mut BTreeMap<
        Qualified<CustomTypeName>,
        ResolvedObjectBooleanExpressionType,
    >,
    issues: &mut Vec<BooleanExpressionIssue>,
    graphql_types: &mut graphql_config::GraphqlTypeNames,
) -> Result<(), BooleanExpressionError> {
    let boolean_expression_type_name = Qualified::new(
        subgraph.clone(),
        object_boolean_expression_type.name.clone(),
    );

    let (object_boolean_expression_type, boolean_expression_issues) =
        legacy::resolve_object_boolean_expression_type(
            object_boolean_expression_type,
            &boolean_expression_type_name,
            data_connectors,
            data_connector_scalars,
            object_types,
            boolean_expression_scalar_types,
            raw_boolean_expression_types,
            relationships,
            raw_models,
            object_boolean_expression_type_names,
            graphql_config,
            &metadata_accessor.flags,
            graphql_types,
        )?;

    issues.extend(boolean_expression_issues);
    boolean_expression_object_types.insert(
        boolean_expression_type_name.clone(),
        object_boolean_expression_type,
    );

    Ok(())
}

/// Determines the strategy for executing relationship predicates based on the connectors and their capabilities.
pub fn get_comparable_relationship_execution_strategy(
    source_connector_name: &Qualified<DataConnectorName>,
    target_connector_name: &Qualified<DataConnectorName>,
    target_source_relationship_capabilities: Option<
        &data_connectors::DataConnectorRelationshipCapabilities,
    >,
) -> ComparableRelationshipExecutionStrategy {
    // It's a local relationship if the source and target connectors are the same and
    // the connector supports relationships.
    if target_connector_name == source_connector_name
        && target_source_relationship_capabilities.is_some_and(|r| r.supports_relation_comparisons)
    {
        ComparableRelationshipExecutionStrategy::NDCPushdown
    } else {
        ComparableRelationshipExecutionStrategy::InEngine
    }
}
