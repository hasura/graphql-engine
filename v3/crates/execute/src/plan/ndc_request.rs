use crate::ndc;
use metadata_resolve::data_connectors::NdcVersion;

pub(crate) fn make_ndc_query_request(
    query_request: ndc_models::QueryRequest,
    data_connector: &metadata_resolve::DataConnectorLink,
) -> Result<ndc::NdcQueryRequest, ndc::migration::NdcDowngradeError> {
    match data_connector.capabilities.supported_ndc_version {
        NdcVersion::V01 => {
            let v01_request = ndc::migration::v01::downgrade_v02_query_request(query_request)?;
            Ok(ndc::NdcQueryRequest::V01(v01_request))
        }
        NdcVersion::V02 => Ok(ndc::NdcQueryRequest::V02(query_request)),
    }
}

pub(crate) fn make_ndc_mutation_request(
    mutation_request: ndc_models::MutationRequest,
    data_connector: &metadata_resolve::DataConnectorLink,
) -> Result<ndc::NdcMutationRequest, ndc::migration::NdcDowngradeError> {
    match data_connector.capabilities.supported_ndc_version {
        NdcVersion::V01 => {
            let v01_request =
                ndc::migration::v01::downgrade_v02_mutation_request(mutation_request)?;
            Ok(ndc::NdcMutationRequest::V01(v01_request))
        }
        NdcVersion::V02 => Ok(ndc::NdcMutationRequest::V02(mutation_request)),
    }
}
