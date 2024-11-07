use std::collections::BTreeMap;

use open_dds::{
    permissions::{Role, TypeOutputPermission},
    types::Deprecated,
};

use crate::Qualified;
use crate::{stages::object_types, ValueExpressionOrPredicate};
use open_dds::types::{CustomTypeName, FieldName};
use serde::{Deserialize, Serialize};
use std::ops::Deref;

pub struct ObjectTypesWithPermissions(
    pub BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithPermissions>,
);

impl Deref for ObjectTypesWithPermissions {
    type Target = BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithPermissions>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl ObjectTypesWithPermissions {
    pub fn get(
        &self,
        type_name: &Qualified<CustomTypeName>,
    ) -> Result<&ObjectTypeWithPermissions, object_types::ObjectTypesError> {
        self.0
            .get(type_name)
            .ok_or_else(|| object_types::ObjectTypesError::ObjectTypeNotFound {
                type_name: type_name.clone(),
            })
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct TypeInputPermission {
    pub field_presets: BTreeMap<FieldName, FieldPresetInfo>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct FieldPresetInfo {
    pub value: ValueExpressionOrPredicate,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub deprecated: Option<Deprecated>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ObjectTypeWithPermissions {
    pub object_type: object_types::ObjectTypeRepresentation,
    /// permissions on this type, when it is used in an output context (e.g. as
    /// a return type of Model or Command)
    pub type_output_permissions: BTreeMap<Role, TypeOutputPermission>,
    /// permissions on this type, when it is used in an input context (e.g. in
    /// an argument type of Model or Command)
    pub type_input_permissions: BTreeMap<Role, TypeInputPermission>,
    /// type mappings for each data connector
    pub type_mappings: object_types::DataConnectorTypeMappingsForObject,
}
