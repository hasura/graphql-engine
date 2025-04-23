use crate::types::subgraph::Qualified;
mod error;
mod types;

pub use error::{
    DataConnectorError, DataConnectorIssue, NamedDataConnectorError, NamedDataConnectorIssue,
};
use open_dds::{data_connector::DataConnectorName, identifier::SubgraphName};
use std::collections::BTreeMap;
pub use types::{
    ArgumentPreset, ArgumentPresetValue, CommandsResponseConfig,
    DataConnectorAggregateCapabilities, DataConnectorCapabilities, DataConnectorContext,
    DataConnectorLink, DataConnectorNestedRelationshipCapabilities,
    DataConnectorRelationalAggregateCapabilities,
    DataConnectorRelationalAggregateExpressionCapabilities,
    DataConnectorRelationalAggregateFunctionCapabilities,
    DataConnectorRelationalComparisonExpressionCapabilities,
    DataConnectorRelationalConditionalExpressionCapabilities,
    DataConnectorRelationalExpressionCapabilities, DataConnectorRelationalJoinCapabilities,
    DataConnectorRelationalJoinTypeCapabilities, DataConnectorRelationalProjectionCapabilities,
    DataConnectorRelationalQueryCapabilities, DataConnectorRelationalScalarExpressionCapabilities,
    DataConnectorRelationalSortCapabilities, DataConnectorRelationalWindowCapabilities,
    DataConnectorRelationalWindowExpressionCapabilities, DataConnectorRelationshipCapabilities,
    DataConnectorSchema, DataConnectors, DataConnectorsOutput, HttpHeadersPreset, NdcVersion,
};

/// Resolve data connectors.
pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
) -> Result<types::DataConnectorsOutput<'_>, Vec<NamedDataConnectorError>> {
    let mut data_connectors = BTreeMap::new();
    let mut issues = vec![];
    let mut results = vec![];

    for open_dds::accessor::QualifiedObject {
        path: _,
        subgraph,
        object: data_connector,
    } in &metadata_accessor.data_connectors
    {
        results.push(resolve_data_connector(
            data_connector,
            metadata_accessor,
            subgraph,
            &mut data_connectors,
            &mut issues,
        ));
    }

    // if everything succeeds, return results, otherwise collect all errors together
    partition_eithers::collect_any_errors(results).map(|_| types::DataConnectorsOutput {
        data_connectors: types::DataConnectors(data_connectors),
        issues,
    })
}

// we resolve each data connector in a different function so all `?` are captured in
// this function so we can potentially catch an error per data connector
fn resolve_data_connector<'s>(
    data_connector: &'s open_dds::data_connector::DataConnectorLinkV1,
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    subgraph: &SubgraphName,
    data_connectors: &mut BTreeMap<Qualified<DataConnectorName>, types::DataConnectorContext<'s>>,
    issues: &mut Vec<NamedDataConnectorIssue>,
) -> Result<(), NamedDataConnectorError> {
    let qualified_data_connector_name =
        Qualified::new(subgraph.clone(), data_connector.name.clone());

    let (data_connector_context, connector_issues) =
        types::DataConnectorContext::new(metadata_accessor, data_connector).map_err(|error| {
            NamedDataConnectorError {
                data_connector_name: qualified_data_connector_name.clone(),
                error,
            }
        })?;

    issues.extend(
        connector_issues
            .into_iter()
            .map(|issue| NamedDataConnectorIssue {
                data_connector_name: qualified_data_connector_name.clone(),
                issue,
            }),
    );

    if data_connectors
        .insert(
            qualified_data_connector_name.clone(),
            data_connector_context,
        )
        .is_some()
    {
        Err(NamedDataConnectorError {
            data_connector_name: qualified_data_connector_name.clone(),
            error: DataConnectorError::DuplicateDataConnectorDefinition,
        })
    } else {
        Ok(())
    }
}
