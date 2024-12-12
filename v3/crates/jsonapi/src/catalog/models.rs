use super::types::Model;
use crate::types::ModelWarning;
use hasura_authn_core::Role;
use metadata_resolve::{ModelWithArgumentPresets, ObjectTypeWithRelationships, Qualified};
use open_dds::types::CustomTypeName;
use std::collections::BTreeMap;

// look at permissions and work out which fields we're allowed to see
// this is quite limited and leans to be overcautious
pub fn build_model(
    model: &ModelWithArgumentPresets,
    role: &Role,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
) -> Result<Model, ModelWarning> {
    // if we have no select permission for the model, ignore it
    if !model.select_permissions.contains_key(role) {
        return Err(ModelWarning::NoSelectPermission);
    }
    object_types
        .get(&model.model.data_type)
        .ok_or_else(|| ModelWarning::NoObjectTypeFound {
            object_type_name: model.model.data_type.clone(),
        })?;

    let model_source = model
        .model
        .source
        .as_ref()
        .ok_or(ModelWarning::NoModelSource)?;

    let data_connector_name = model_source.data_connector.name.clone();

    Ok(Model {
        name: model.model.name.clone(),
        description: model.model.raw.description.clone(),
        data_type: model.model.data_type.clone(),
        data_connector_name,
        filter_expression_type: model.filter_expression_type.clone(),
    })
}
