use open_dds::types::DataConnectorArgumentName;
use std::collections::BTreeMap;

use super::error;
use super::types;
use crate::ir::arguments as ir_arguments;
use crate::ir::selection_set::NdcRelationshipName;

pub fn plan_arguments<'s>(
    arguments: &BTreeMap<DataConnectorArgumentName, ir_arguments::Argument<'s>>,
    relationships: &mut BTreeMap<NdcRelationshipName, types::Relationship>,
) -> Result<BTreeMap<DataConnectorArgumentName, types::UnresolvedArgument<'s>>, error::Error> {
    let mut result = BTreeMap::new();
    for (argument_name, argument_value) in arguments {
        result.insert(
            argument_name.clone(),
            types::Argument::plan(argument_value, relationships)?,
        );
    }
    Ok(result)
}

pub fn plan_mutation_arguments<'s>(
    arguments: &BTreeMap<DataConnectorArgumentName, ir_arguments::Argument<'s>>,
    relationships: &mut BTreeMap<NdcRelationshipName, types::Relationship>,
) -> Result<BTreeMap<DataConnectorArgumentName, types::UnresolvedMutationArgument<'s>>, error::Error>
{
    arguments
        .iter()
        .map(|(name, argument)| {
            Ok((
                name.clone(),
                types::MutationArgument::plan(argument, relationships)?,
            ))
        })
        .collect::<Result<BTreeMap<_, _>, error::Error>>()
}
