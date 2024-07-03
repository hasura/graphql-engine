use metadata_resolve::data_connectors::NdcVersion;
use open_dds::commands::ProcedureName;

use crate::ir::commands::FunctionBasedCommand;
use crate::ir::commands::ProcedureBasedCommand;
use crate::ir::model_selection::ModelSelection;
use crate::ndc;
use crate::remote_joins::types::{JoinLocations, MonotonicCounter, RemoteJoin};

use super::commands;
use super::error;
use super::model_selection;

pub(crate) fn make_ndc_model_query_request<'s, 'ir>(
    ir: &'ir ModelSelection<'s>,
    join_id_counter: &mut MonotonicCounter,
) -> Result<(ndc::NdcQueryRequest, JoinLocations<RemoteJoin<'s, 'ir>>), error::Error> {
    match ir.data_connector.capabilities.supported_ndc_version {
        NdcVersion::V01 => {
            let (request, join_locations) = model_selection::ndc_ir(ir, join_id_counter)?;
            let v01_request = ndc::migration::v01::downgrade_v02_query_request(request)
                .map_err(error::InternalError::NdcRequestDowngradeError)?;
            Ok((ndc::NdcQueryRequest::V01(v01_request), join_locations))
        }
        NdcVersion::V02 => {
            let (request, join_locations) = model_selection::ndc_ir(ir, join_id_counter)?;
            Ok((ndc::NdcQueryRequest::V02(request), join_locations))
        }
    }
}

pub(crate) fn make_ndc_function_query_request<'s, 'ir>(
    ir: &'ir FunctionBasedCommand<'s>,
    join_id_counter: &mut MonotonicCounter,
) -> Result<(ndc::NdcQueryRequest, JoinLocations<RemoteJoin<'s, 'ir>>), error::Error> {
    match ir
        .command_info
        .data_connector
        .capabilities
        .supported_ndc_version
    {
        NdcVersion::V01 => {
            let (request, join_locations) = commands::ndc_query_ir(ir, join_id_counter)?;
            let v01_request = ndc::migration::v01::downgrade_v02_query_request(request)
                .map_err(error::InternalError::NdcRequestDowngradeError)?;
            Ok((ndc::NdcQueryRequest::V01(v01_request), join_locations))
        }
        NdcVersion::V02 => {
            let (request, join_locations) = commands::ndc_query_ir(ir, join_id_counter)?;
            Ok((ndc::NdcQueryRequest::V02(request), join_locations))
        }
    }
}

pub(crate) fn ndc_procedure_mutation_request<'s, 'ir>(
    procedure_name: &ProcedureName,
    ir: &'ir ProcedureBasedCommand<'s>,
    join_id_counter: &mut MonotonicCounter,
) -> Result<(ndc::NdcMutationRequest, JoinLocations<RemoteJoin<'s, 'ir>>), error::Error> {
    match ir
        .command_info
        .data_connector
        .capabilities
        .supported_ndc_version
    {
        NdcVersion::V01 => {
            let (request, join_locations) =
                commands::ndc_mutation_ir(procedure_name, ir, join_id_counter)?;
            let v01_request = ndc::migration::v01::downgrade_v02_mutation_request(request)
                .map_err(error::InternalError::NdcRequestDowngradeError)?;
            Ok((ndc::NdcMutationRequest::V01(v01_request), join_locations))
        }
        NdcVersion::V02 => {
            let (request, join_locations) =
                commands::ndc_mutation_ir(procedure_name, ir, join_id_counter)?;
            Ok((ndc::NdcMutationRequest::V02(request), join_locations))
        }
    }
}
