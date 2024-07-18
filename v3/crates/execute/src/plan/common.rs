use metadata_resolve::data_connectors::NdcVersion;
use open_dds::types::DataConnectorArgumentName;
use std::collections::BTreeMap;

use super::error;
use super::types;
use crate::ir::arguments as ir_arguments;

pub fn plan_ndc_arguments<'s, 'a>(
    arguments: &'a BTreeMap<DataConnectorArgumentName, ir_arguments::Argument<'s>>,
    ndc_version: NdcVersion,
    relationships: &'a mut BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
) -> Result<BTreeMap<ndc_models::ArgumentName, types::Argument<'s>>, error::Error> {
    let mut result = BTreeMap::new();
    for (argument_name, argument_value) in arguments {
        result.insert(
            ndc_models::ArgumentName::from(argument_name.as_str()),
            types::Argument::plan(argument_value, ndc_version, relationships)?,
        );
    }
    Ok(result)
}
