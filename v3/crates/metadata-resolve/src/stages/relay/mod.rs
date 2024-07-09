use std::collections::BTreeMap;

use open_dds::{models::ModelName, types::CustomTypeName};

use crate::types::error::Error;

use crate::types::subgraph::Qualified;

/// This isn't a particularly satisfying resolve step, as it only serves to validate
/// the output of previous steps.
/// Ideally, we could move more Relay-based resolving into this discreet step, haven't
/// investigated this too deeply yet.
pub fn resolve(
    global_id_enabled_types: &BTreeMap<Qualified<CustomTypeName>, Vec<Qualified<ModelName>>>,
) -> Result<(), Error> {
    // To check if global_id_fields are defined in object type but no model has global_id_source set to true:
    //   - Throw an error if no model with globalIdSource:true is found for the object type.
    for (object_type, model_name_list) in global_id_enabled_types {
        if model_name_list.is_empty() {
            return Err(Error::GlobalIdSourceNotDefined {
                object_type: object_type.clone(),
            });
        }
    }

    Ok(())
}
