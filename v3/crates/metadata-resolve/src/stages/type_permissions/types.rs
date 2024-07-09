use std::collections::BTreeMap;

use open_dds::permissions::{Role, TypeOutputPermission, ValueExpression};

use open_dds::types::FieldName;

use crate::stages::object_types;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct TypeInputPermission {
    pub field_presets: BTreeMap<FieldName, ValueExpression>,
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
