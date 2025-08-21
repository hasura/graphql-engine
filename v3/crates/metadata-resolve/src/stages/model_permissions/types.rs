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

use crate::types::subgraph::{deserialize_qualified_btreemap, serialize_qualified_btreemap};
use crate::{
    AllowOrDeny,
    types::subgraph::{Qualified, QualifiedTypeReference},
};
use crate::{ArgumentInfo, types::error::ContextualError};
use crate::{
    ConditionHash,
    types::permission::{ValueExpression, ValueExpressionOrPredicate},
};
use crate::{
    helpers::typecheck,
    stages::{
        boolean_expressions, data_connectors, model_permissions, models, models_graphql,
        object_relationships, object_types,
    },
    types::error::ShouldBeAnError,
};
use error_context::{Context, Step};

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct RelationalInsertPermission {
    // Empty for now, will be extended later with filter predicates
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct RelationalUpdatePermission {
    // Empty for now, will be extended later with filter predicates
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct RelationalDeletePermission {
    // Empty for now, will be extended later with filter predicates
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct ModelWithPermissions {
    pub model: models_graphql::Model,
    pub arguments: IndexMap<ArgumentName, ArgumentInfo>,
    pub permissions: ModelPermissions,
    pub filter_expression_type:
        Option<Arc<boolean_expressions::ResolvedObjectBooleanExpressionType>>,
    pub graphql_api: models_graphql::ModelGraphQlApi,
    pub description: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct SelectPermissions {
    pub by_role: BTreeMap<Role, SelectPermission>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum ModelAuthorizationRule {
    Access {
        condition: Option<ConditionHash>,
        allow_or_deny: AllowOrDeny,
    },
    Subscription {
        condition: Option<ConditionHash>,
        allow_or_deny: AllowOrDeny,
    },
    Filter {
        condition: Option<ConditionHash>,
        predicate: ModelPredicate,
    },
    // value for an argument preset. the last value wins where multiple items are used.
    ArgumentPresetValue {
        condition: Option<ConditionHash>,
        argument_name: ArgumentName,
        argument_type: QualifiedTypeReference,
        value: ValueExpression,
    },
    // boolean expression for an argument preset. if multiple items are provided for one argument
    // then we "and" them together
    ArgumentAuthPredicate {
        condition: Option<ConditionHash>,
        argument_name: ArgumentName,
        predicate: ModelPredicate,
    },
    RelationalPermission {
        condition: Option<ConditionHash>,
        allow_or_deny: AllowOrDeny,
        relational_operation: RelationalOperation,
    },
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum RelationalOperation {
    Insert,
    Update,
    Delete,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct ModelPermissions {
    pub authorization_rules: Vec<ModelAuthorizationRule>,
    pub by_role: BTreeMap<Role, ModelPermission>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct ModelPermission {
    pub select: Option<SelectPermission>,
    pub input: Option<ModelInputPermission>,
}

impl ModelPermissions {
    pub fn new() -> Self {
        Self {
            by_role: BTreeMap::new(),
            authorization_rules: Vec::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.by_role.is_empty() && self.authorization_rules.is_empty()
    }
}

impl Default for ModelPermissions {
    fn default() -> Self {
        Self::new()
    }
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
    pub allow_subscriptions: bool,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct ModelInputPermission {
    pub argument_presets:
        BTreeMap<ArgumentName, (QualifiedTypeReference, ValueExpressionOrPredicate)>,
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
    /// Note, `And(vec![])` means `const True`
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
        "Type error in preset argument {argument_name:}{} in model {model_name:}: {typecheck_issue:}",
        match role { Some(role) => format!(" for role {role:}"), None => String::new() }
    )]
    ModelArgumentPresetTypecheckIssue {
        role: Option<Role>,
        model_name: Qualified<ModelName>,
        argument_name: ArgumentName,
        typecheck_issue: typecheck::TypecheckIssue,
    },
    #[error(
        "the object type {data_type} used by model {model_name} uses rules-based authorization so will not appear in the GraphQL schema"
    )]
    ModelDataTypeUsesRulesBasedAuthorization {
        model_name: Qualified<ModelName>,
        data_type: Qualified<CustomTypeName>,
    },
    #[error(
        "the object type {argument_type} used in arguments for the model {model_name} uses rules-based authorization so any presets will not be applied in the GraphQL schema"
    )]
    ModelArgumentTypeUsesRulesBasedAuthorization {
        model_name: Qualified<ModelName>,
        argument_type: Qualified<CustomTypeName>,
    },
    #[error(
        "the model {model_name} uses rules-based authorization so will not appear in the GraphQL schema"
    )]
    ModelUsesRulesBasedAuthorization { model_name: Qualified<ModelName> },
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
            ModelPermissionIssue::ModelArgumentPresetTypecheckIssue { .. }
            | ModelPermissionIssue::ModelArgumentTypeUsesRulesBasedAuthorization { .. }
            | ModelPermissionIssue::ModelDataTypeUsesRulesBasedAuthorization { .. }
            | ModelPermissionIssue::ModelUsesRulesBasedAuthorization { .. } => None,
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
            ModelPermissionIssue::ModelDataTypeUsesRulesBasedAuthorization { .. }
            | ModelPermissionIssue::ModelUsesRulesBasedAuthorization { .. }
            | ModelPermissionIssue::ModelArgumentTypeUsesRulesBasedAuthorization { .. } => false,
        }
    }
}

/// The output of the model permissions stage.
pub struct ModelPermissionsOutput {
    pub permissions: IndexMap<Qualified<ModelName>, ModelWithPermissions>,
    pub issues: Vec<ModelPermissionIssue>,
}
