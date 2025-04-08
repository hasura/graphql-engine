mod error;
mod helpers;
mod legacy;
mod scalar;
mod types;

pub use error::{ScalarBooleanExpressionTypeError, ScalarBooleanExpressionTypeIssue};
pub use helpers::resolve_ndc_type;
pub use scalar::resolve_logical_operators;
pub use types::{
    IsNullOperator, IsNullOperatorGraphqlConfig, LogicalOperators, LogicalOperatorsGraphqlConfig,
    ResolvedScalarBooleanExpressionType, ScalarBooleanExpressionsOutput,
};

use std::collections::BTreeMap;

use open_dds::{
    boolean_expression::BooleanExpressionOperand, data_connector::DataConnectorName,
    types::CustomTypeName,
};

use crate::Qualified;
use crate::stages::{
    boolean_expressions, data_connector_scalar_types, data_connectors, graphql_config,
    object_types, scalar_types,
};

pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    data_connectors: &data_connectors::DataConnectors,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars<'_>,
    >,
    object_types: &object_types::ObjectTypesWithTypeMappings,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    graphql_config: &graphql_config::GraphqlConfig,
    graphql_types: &mut graphql_config::GraphqlTypeNames,
) -> Result<ScalarBooleanExpressionsOutput, Vec<ScalarBooleanExpressionTypeError>> {
    let mut raw_boolean_expression_types = BTreeMap::new();
    let mut issues = vec![];
    let mut results = vec![];

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

    let mut boolean_expression_scalar_types = BTreeMap::new();

    // let's resolve scalar types first
    for (boolean_expression_type_name, (subgraph, boolean_expression_type)) in
        &raw_boolean_expression_types
    {
        results.push(resolve_scalar_boolean_expression_type(
            boolean_expression_type_name,
            boolean_expression_type,
            subgraph,
            data_connectors,
            data_connector_scalars,
            object_types,
            scalar_types,
            graphql_config,
            metadata_accessor,
            graphql_types,
            &mut boolean_expression_scalar_types,
            &mut issues,
        ));
    }

    // do we have any `ObjectBooleanExpressionType` to resolve? If not, don't bother with other
    // legacy stuff (as they are likely to be incomplete)
    if !metadata_accessor.object_boolean_expression_types.is_empty() {
        // collect raw data connector scalar representations, which we'll use when converting
        // old `ObjectBooleanExpressionType`s
        for open_dds::accessor::QualifiedObject {
            path: _,
            subgraph,
            object: data_connector_scalar_representation,
        } in &metadata_accessor.data_connector_scalar_representations
        {
            results.push(resolve_legacy_scalar_representation_item(
                data_connector_scalar_representation,
                subgraph,
                data_connectors,
                data_connector_scalars,
                graphql_config,
                &mut boolean_expression_scalar_types,
            ));
        }
    }

    // if everything succeeds, return results, otherwise collect all errors together
    partition_eithers::collect_any_errors(results).map(|_| ScalarBooleanExpressionsOutput {
        boolean_expression_scalar_types,
        // TODO: make sure we are adding new types to graphql_types
        issues,
    })
}

fn resolve_scalar_boolean_expression_type(
    boolean_expression_type_name: &Qualified<CustomTypeName>,
    boolean_expression_type: &open_dds::boolean_expression::BooleanExpressionTypeV1,
    subgraph: &open_dds::identifier::SubgraphName,
    data_connectors: &data_connectors::DataConnectors,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars<'_>,
    >,
    object_types: &object_types::ObjectTypesWithTypeMappings,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    graphql_config: &graphql_config::GraphqlConfig,
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    graphql_types: &mut graphql_config::GraphqlTypeNames,
    boolean_expression_scalar_types: &mut BTreeMap<
        boolean_expressions::BooleanExpressionTypeIdentifier,
        ResolvedScalarBooleanExpressionType,
    >,
    issues: &mut Vec<ScalarBooleanExpressionTypeIssue>,
) -> Result<(), ScalarBooleanExpressionTypeError> {
    if let BooleanExpressionOperand::Scalar(boolean_expression_scalar_operand) =
        &boolean_expression_type.operand
    {
        let scalar_boolean_expression_type = scalar::resolve_scalar_boolean_expression_type(
            boolean_expression_type_name,
            boolean_expression_scalar_operand,
            boolean_expression_type,
            subgraph,
            data_connectors,
            data_connector_scalars,
            object_types,
            scalar_types,
            graphql_config,
            &metadata_accessor.flags,
            graphql_types,
            issues,
        )?;

        boolean_expression_scalar_types.insert(
            boolean_expressions::BooleanExpressionTypeIdentifier::FromBooleanExpressionType(
                boolean_expression_type_name.clone(),
            ),
            scalar_boolean_expression_type,
        );
    }
    Ok(())
}

fn resolve_legacy_scalar_representation_item(
    data_connector_scalar_representation: &open_dds::types::DataConnectorScalarRepresentationV1,
    subgraph: &open_dds::identifier::SubgraphName,
    data_connectors: &data_connectors::DataConnectors,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars<'_>,
    >,
    graphql_config: &graphql_config::GraphqlConfig,
    boolean_expression_scalar_types: &mut BTreeMap<
        boolean_expressions::BooleanExpressionTypeIdentifier,
        ResolvedScalarBooleanExpressionType,
    >,
) -> Result<(), ScalarBooleanExpressionTypeError> {
    let (boolean_expression_type_identifier, scalar_boolean_expression_type) =
        legacy::resolve_data_connector_scalar_representation(
            data_connector_scalar_representation,
            subgraph,
            data_connectors,
            data_connector_scalars,
            graphql_config,
        )?;

    boolean_expression_scalar_types.insert(
        boolean_expression_type_identifier,
        scalar_boolean_expression_type,
    );

    Ok(())
}
