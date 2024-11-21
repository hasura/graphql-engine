pub mod v01;
pub mod v02;

use crate::{error, ndc};
use metadata_resolve::data_connectors::NdcVersion;
use plan_types::QueryExecutionPlan;

pub fn make_ndc_query_request(
    query_execution_plan: QueryExecutionPlan,
) -> Result<ndc::NdcQueryRequest, error::FieldError> {
    match query_execution_plan
        .data_connector
        .capabilities
        .supported_ndc_version
    {
        NdcVersion::V01 => Ok(ndc::NdcQueryRequest::V01(v01::make_query_request(
            query_execution_plan,
        )?)),
        NdcVersion::V02 => Ok(ndc::NdcQueryRequest::V02(v02::make_query_request(
            query_execution_plan,
        )?)),
    }
}
