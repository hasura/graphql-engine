use crate::stages::{object_types, type_permissions};
use crate::types::subgraph::{Qualified, QualifiedTypeReference};
use indexmap::IndexMap;
use open_dds::aggregates::AggregateExpressionName;
use open_dds::permissions::Role;
use open_dds::{commands::CommandName, models::ModelName, types::CustomTypeName};
use serde::{Deserialize, Serialize};

use crate::helpers::types::NdcColumnForComparison;
use lang_graphql::ast::common as ast;
use open_dds::arguments::ArgumentName;
use std::collections::BTreeMap;

use open_dds::relationships::{FieldAccess, RelationshipName, RelationshipType};
use open_dds::types::Deprecated;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ObjectTypeWithRelationships {
    pub object_type: object_types::ObjectTypeRepresentation,
    /// permissions on this type, when it is used in an output context (e.g. as
    /// a return type of Model or Command)
    pub type_output_permissions: BTreeMap<Role, open_dds::permissions::TypeOutputPermission>,
    /// permissions on this type, when it is used in an input context (e.g. in
    /// an argument type of Model or Command)
    pub type_input_permissions: BTreeMap<Role, type_permissions::TypeInputPermission>,
    /// any relationship fields defined on this object, indexed by field name
    /// note that a single relationship may result in the generation of multiple fields
    /// (ie normal relationship + aggregate relationship)
    pub relationship_fields: IndexMap<ast::Name, RelationshipField>,
    /// type mappings for each data connector
    pub type_mappings: object_types::DataConnectorTypeMappingsForObject,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum RelationshipTarget {
    Model(ModelRelationshipTarget),
    ModelAggregate(ModelAggregateRelationshipTarget),
    Command(CommandRelationshipTarget),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ModelRelationshipTarget {
    // TODO(Abhinav): Refactor resolved types to contain denormalized data (eg: actual resolved model)
    pub model_name: Qualified<ModelName>,
    pub relationship_type: RelationshipType,
    pub target_typename: Qualified<CustomTypeName>,
    pub mappings: Vec<RelationshipModelMapping>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ModelAggregateRelationshipTarget {
    pub model_name: Qualified<ModelName>,
    pub target_typename: Qualified<CustomTypeName>,
    pub mappings: Vec<RelationshipModelMapping>,
    pub aggregate_expression: Qualified<AggregateExpressionName>,
    pub filter_input_field_name: ast::Name,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct CommandRelationshipTarget {
    pub command_name: Qualified<CommandName>,
    pub target_type: QualifiedTypeReference,
    pub mappings: Vec<RelationshipCommandMapping>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum RelationshipTargetName {
    Model(Qualified<ModelName>),
    Command(Qualified<CommandName>),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct RelationshipModelMapping {
    pub source_field: FieldAccess,
    pub target_field: FieldAccess,
    // Optional because we allow building schema without specifying a data source
    pub target_ndc_column: Option<NdcColumnForComparison>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct RelationshipCommandMapping {
    pub source_field: FieldAccess,
    pub argument_name: ArgumentName,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct RelationshipField {
    pub field_name: ast::Name,
    pub relationship_name: RelationshipName,
    pub source: Qualified<CustomTypeName>,
    pub target: RelationshipTarget,
    pub target_capabilities: Option<RelationshipCapabilities>,
    pub description: Option<String>,
    pub deprecated: Option<Deprecated>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct RelationshipCapabilities {
    // TODO: We don't handle relationships without foreach.
    // Change this to a bool, when we support that
    pub foreach: (),
    pub relationships: bool,
    pub relationship_comparison: bool,
}

pub enum RelationshipExecutionCategory {
    // Push down relationship definition to the data connector
    Local,
    // Use foreach in the data connector to fetch related rows for multiple objects in a single request
    RemoteForEach,
}
