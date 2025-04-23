use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::sync::Arc;

use open_dds::{
    arguments::ArgumentName,
    data_connector::{DataConnectorColumnName, DataConnectorOperatorName},
    models::ModelName,
    permissions::Role,
    query::ComparisonOperator,
    relationships::{RelationshipName, RelationshipType},
    spanned::Spanned,
    types::{CustomTypeName, Deprecated, FieldName},
};

use crate::types::error::ContextualError;
use crate::types::permission::{ValueExpression, ValueExpressionOrPredicate};
use crate::types::subgraph::{Qualified, QualifiedTypeReference};
use crate::types::subgraph::{deserialize_qualified_btreemap, serialize_qualified_btreemap};
use crate::{
    helpers::typecheck,
    stages::{
        boolean_expressions, data_connectors, model_permissions, models, models_graphql,
        object_relationships, object_types,
    },
    types::error::ShouldBeAnError,
};
use error_context::{Context, Step};

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct ModelWithPermissions {
    pub model: models::Model,
    pub select_permissions: BTreeMap<Role, SelectPermission>,
    pub filter_expression_type: Option<boolean_expressions::ResolvedObjectBooleanExpressionType>,
    pub graphql_api: models_graphql::ModelGraphQlApi,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum FilterPermission {
    AllowAll,
    Filter(ModelPredicate),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct SelectPermission {
    pub filter: FilterPermission,
    // pub allow_aggregations: bool,
    pub argument_presets:
        BTreeMap<ArgumentName, (QualifiedTypeReference, ValueExpressionOrPredicate)>,
    pub allow_subscriptions: bool,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ResolvedOperator {
    pub data_connector_operator_name: DataConnectorOperatorName,
    pub comparison_operator: ComparisonOperator,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum ModelPredicate {
    UnaryFieldComparison {
        field: FieldName,
        field_parent_type: Qualified<CustomTypeName>,
        ndc_column: DataConnectorColumnName,
        operator: UnaryComparisonOperator,
        column_path: Vec<DataConnectorColumnName>,
        /// To mark a field as deprecated in the field usage while reporting query usage analytics.
        deprecated: Option<Deprecated>,
    },
    BinaryFieldComparison {
        field: FieldName,
        field_parent_type: Qualified<CustomTypeName>,
        ndc_column: DataConnectorColumnName,
        operator: ResolvedOperator,
        column_path: Vec<DataConnectorColumnName>,
        argument_type: QualifiedTypeReference,
        value: ValueExpression,
        /// To mark a field as deprecated in the field usage while reporting query usage analytics.
        deprecated: Option<Deprecated>,
    },
    Relationship {
        relationship_info: PredicateRelationshipInfo,
        column_path: Vec<DataConnectorColumnName>,
        predicate: Box<ModelPredicate>,
    },
    And(Vec<ModelPredicate>),
    Or(Vec<ModelPredicate>),
    Not(Box<ModelPredicate>),
}

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum UnaryComparisonOperator {
    IsNull,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PredicateRelationshipInfo {
    pub relationship_name: RelationshipName,
    pub relationship_type: RelationshipType,
    pub source_type: Qualified<CustomTypeName>,
    pub source_data_connector: data_connectors::DataConnectorLink,
    #[serde(
        serialize_with = "serialize_qualified_btreemap",
        deserialize_with = "deserialize_qualified_btreemap"
    )]
    pub source_type_mappings: BTreeMap<Qualified<CustomTypeName>, object_types::TypeMapping>,
    pub target_source: ModelTargetSource,
    pub target_type: Qualified<CustomTypeName>,
    pub target_model_name: Qualified<ModelName>,
    pub mappings: Vec<object_relationships::RelationshipModelMapping>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ModelTargetSource {
    pub model: Arc<models::ModelSource>,
    pub capabilities: object_relationships::RelationshipCapabilities,
}

impl ModelTargetSource {
    pub fn new(
        model: &model_permissions::ModelWithPermissions,
        relationship: &object_relationships::RelationshipField,
    ) -> Result<Option<Self>, object_relationships::RelationshipError> {
        model
            .model
            .source
            .as_ref()
            .map(|model_source| Self::from_model_source(&model_source.clone(), relationship))
            .transpose()
    }

    pub fn from_model_source(
        model_source: &Arc<models::ModelSource>,
        relationship: &object_relationships::RelationshipField,
    ) -> Result<Self, object_relationships::RelationshipError> {
        Ok(Self {
            model: model_source.clone(),
            capabilities: relationship
                .target_capabilities
                .as_ref()
                .ok_or_else(|| {
                    object_relationships::RelationshipError::NoRelationshipCapabilitiesDefined {
                        type_name: relationship.source.clone(),
                        relationship_name: relationship.relationship_name.clone(),
                        data_connector_name: model_source.data_connector.name.clone(),
                    }
                })?
                .clone(),
        })
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ModelPermissionIssue {
    #[error(
        "The role '{role}' has been defined more than once in model permissions for model '{model_name}'"
    )]
    DuplicateRole {
        role: Spanned<Role>,
        model_name: Qualified<ModelName>,
    },
    #[error(
        "Type error in preset argument {argument_name:} for role {role:} in model {model_name:}: {typecheck_issue:}"
    )]
    ModelArgumentPresetTypecheckIssue {
        role: Role,
        model_name: Qualified<ModelName>,
        argument_name: ArgumentName,
        typecheck_issue: typecheck::TypecheckIssue,
    },
}

impl ContextualError for ModelPermissionIssue {
    fn create_error_context(&self) -> Option<error_context::Context> {
        match self {
            ModelPermissionIssue::DuplicateRole { role, model_name } => {
                Some(Context::from_step(Step {
                    message: "This role is a duplicate".to_owned(),
                    path: role.path.clone(),
                    subgraph: Some(model_name.subgraph.clone()),
                }))
            }
            ModelPermissionIssue::ModelArgumentPresetTypecheckIssue { .. } => None,
        }
    }
}

impl ShouldBeAnError for ModelPermissionIssue {
    fn should_be_an_error(&self, flags: &open_dds::flags::OpenDdFlags) -> bool {
        match self {
            ModelPermissionIssue::DuplicateRole { .. } => {
                flags.contains(open_dds::flags::Flag::DisallowDuplicateModelPermissionsRoles)
            }
            ModelPermissionIssue::ModelArgumentPresetTypecheckIssue {
                typecheck_issue, ..
            } => typecheck_issue.should_be_an_error(flags),
        }
    }
}

/// The output of the model permissions stage.
pub struct ModelPermissionsOutput {
    pub permissions: IndexMap<Qualified<ModelName>, ModelWithPermissions>,
    pub issues: Vec<ModelPermissionIssue>,
}
