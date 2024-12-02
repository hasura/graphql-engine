mod presets;
mod types;

use crate::stages::{command_permissions, model_permissions, object_relationships};
use crate::types::subgraph::Qualified;
use indexmap::IndexMap;
use open_dds::{commands::CommandName, models::ModelName, types::CustomTypeName};
pub use presets::ArgumentPresetError;
pub use types::{
    ArgumentNameAndPath, ArgumentPresets, ArgumentPresetsOutput, CommandWithArgumentPresets,
    ModelWithArgumentPresets,
};

use std::collections::BTreeMap;

/// resolve model and command argument presets
/// TODO: can we move `models` in here to avoid the cloning?
pub fn resolve(
    models: &IndexMap<Qualified<ModelName>, model_permissions::ModelWithPermissions>,
    commands: &IndexMap<Qualified<CommandName>, command_permissions::CommandWithPermissions>,
    object_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_relationships::ObjectTypeWithRelationships,
    >,
) -> Result<ArgumentPresetsOutput, ArgumentPresetError> {
    // models with resolved argument presets
    let mut models_with_argument_presets = IndexMap::new();
    for (model_name, model) in models {
        let argument_presets = presets::get_argument_presets_for_model(model, object_types)?;
        models_with_argument_presets.insert(
            model_name.clone(),
            ModelWithArgumentPresets {
                argument_presets,
                model: model.model.clone(),
                filter_expression_type: model.filter_expression_type.clone(),
                graphql_api: model.graphql_api.clone(),
                select_permissions: model.select_permissions.clone(),
            },
        );
    }

    let mut commands_with_argument_presets = IndexMap::new();

    for (command_name, command) in commands {
        let argument_presets = presets::get_argument_presets_for_command(command, object_types)?;
        commands_with_argument_presets.insert(
            command_name.clone(),
            CommandWithArgumentPresets {
                argument_presets,
                command: command.command.clone(),
                permissions: command.permissions.clone(),
            },
        );
    }

    Ok(ArgumentPresetsOutput {
        models: models_with_argument_presets,
        commands: commands_with_argument_presets,
    })
}
