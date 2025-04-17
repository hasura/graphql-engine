use open_dds::{models::ModelName, types::CustomTypeName};
use std::collections::BTreeMap;

use crate::types::subgraph::Qualified;

/// This isn't a particularly satisfying resolve step, as it only serves to validate
/// the output of previous steps.
/// Ideally, we could move more Relay-based resolving into this discreet step, haven't
/// investigated this too deeply yet.
pub fn resolve(
    global_id_enabled_types: BTreeMap<Qualified<CustomTypeName>, Vec<Qualified<ModelName>>>,
) -> Result<(), RelayError> {
    // To check if global_id_fields are defined in object type but no model has global_id_source set to true:
    //   - Throw an error if no model with globalIdSource:true is found for the object type.
    for (object_type, model_name_list) in global_id_enabled_types {
        if model_name_list.is_empty() {
            return Err(RelayError::GlobalIdSourceNotDefined { object_type });
        }
    }

    Ok(())
}

#[derive(Debug, thiserror::Error)]
pub enum RelayError {
    #[error(
        "'globalIdFields' for type {object_type:} found, but no model found with 'globalIdSource: true' for type {object_type:}"
    )]
    GlobalIdSourceNotDefined {
        object_type: Qualified<CustomTypeName>,
    },
    #[error(
        "Model {model_name:} is marked as a global ID source but there are no global id fields present in the related object type {type_name:}"
    )]
    NoGlobalFieldsPresentInGlobalIdSource {
        type_name: Qualified<CustomTypeName>,
        model_name: ModelName,
    },
    #[error(
        "Found multiple models  {model_1:}, {model_2:} that implement the same object type {object_type:} to be global ID sources."
    )]
    DuplicateModelGlobalIdSource {
        model_1: Qualified<ModelName>,
        model_2: Qualified<ModelName>,
        object_type: Qualified<CustomTypeName>,
    },
    #[error("model {model_name:} with arguments is unsupported as a global ID source")]
    ModelWithArgumentsAsGlobalIdSource { model_name: Qualified<ModelName> },
}
