use indexmap::IndexSet;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize, de::Error};
use serde_json::Value as JsonValue;

use crate::{
    arguments::ArgumentName,
    authorization,
    commands::CommandName,
    impl_JsonSchema_with_OpenDd_for,
    models::ModelName,
    relationships::RelationshipName,
    session_variables::SessionVariableName,
    spanned::Spanned,
    traits::{self, OpenDd, OpenDdDeserializeError},
    types::{CustomTypeName, FieldName, OperatorName},
    views::ViewName,
};

#[derive(
    Serialize,
    Deserialize,
    Clone,
    Debug,
    Eq,
    PartialEq,
    PartialOrd,
    Ord,
    JsonSchema,
    Hash,
    derive_more::Display,
    opendds_derive::OpenDd,
)]
pub struct Role(pub String);

impl Role {
    pub fn new(str: &str) -> Role {
        Role(str.to_string())
    }
}

#[derive(Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
/// Preset value for an argument
pub struct ArgumentPreset {
    /// Argument name for preset
    pub argument: Spanned<ArgumentName>,
    /// Value for preset
    pub value: Spanned<ValueExpressionOrPredicate>,
}

#[derive(Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[opendd(
    as_versioned_with_definition,
    json_schema(
        title = "TypePermissions",
        example = "TypePermissions::example_v1",
        example = "TypePermissions::example_v2_role_based",
        example = "TypePermissions::example_v2_rules_based"
    )
)]
/// Definition of permissions for an OpenDD type.
pub enum TypePermissions {
    V1(TypePermissionsV1),
    V2(TypePermissionsV2),
}

impl TypePermissions {
    fn example_v1() -> serde_json::Value {
        serde_json::json!(
            {
                "kind": "TypePermissions",
                "version": "v1",
                "definition": {
                    "typeName": "article",
                    "permissions": [
                        {
                            "role": "admin",
                            "output": {
                                "allowedFields": [
                                    "article_id",
                                    "author_id",
                                    "title"
                                ]
                            }
                        },
                        {
                            "role": "user",
                            "output": {
                                "allowedFields": [
                                    "article_id",
                                    "author_id"
                                ]
                            }
                        }
                    ]
                }
            }
        )
    }

    fn example_v2_role_based() -> serde_json::Value {
        serde_json::json!(
            {
                "kind": "TypePermissions",
                "version": "v2",
                "definition": {
                    "typeName": "article",
                    "roleBased": {
                        "permissions": [
                            {
                                "role": "admin",
                                "output": {
                                    "allowedFields": [
                                        "article_id",
                                        "author_id",
                                        "title"
                                    ]
                                }
                            },
                            {
                                "role": "user",
                                "output": {
                                    "allowedFields": [
                                        "article_id",
                                        "author_id"
                                    ]
                                }
                            }
                        ]
                    }
                }
            }
        )
    }

    fn example_v2_rules_based() -> serde_json::Value {
        serde_json::json!(
        {
            "kind": "TypePermissions",
            "version": "v2",
            "definition": {
                "typeName": "movie",
                "permissions": {
                    "rulesBased": [
                        {
                            "allowFields": {
                                "condition": {
                                    "contains": {
                                        "left": {
                                            "sessionVariable": "x-hasura-role"
                                        },
                                        "right": {
                                            "literal": [
                                                "admin",
                                                "user",
                                                "user_not",
                                                "user_and",
                                                "user_or",
                                                "limited_fields_user"
                                            ]
                                        }
                                    }
                                },
                                "fields": ["movie_id", "rating", "title", "release_date"]
                            }
                        },
                        {
                            "denyFields": {
                                "condition": {
                                    "contains": {
                                        "left": {
                                            "sessionVariable": "x-hasura-role"
                                        },
                                        "right": {
                                            "literal": ["limited_fields_user"]
                                        }
                                    }
                                },
                                "fields": ["rating"]
                            }
                        }
                    ]
                }
            }
            }
        )
    }

    pub fn upgrade(self) -> TypePermissionsV2 {
        match self {
            TypePermissions::V1(v1) => TypePermissionsV2 {
                type_name: v1.type_name,
                permissions: TypePermissionOperand::RoleBased(v1.permissions),
            },
            TypePermissions::V2(v2) => v2,
        }
    }
}

#[derive(Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "TypePermissionsV1"))]
/// Definition of permissions for an OpenDD type.
pub struct TypePermissionsV1 {
    /// The name of the type for which permissions are being defined. Must be an object type.
    pub type_name: CustomTypeName,
    /// A list of type permissions, one for each role.
    pub permissions: Vec<TypePermission>,
}

#[derive(Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "TypePermissionsV1"))]
/// Definition of permissions for an OpenDD type.
pub struct TypePermissionsV2 {
    /// The name of the type for which permissions are being defined. Must be an object type.
    pub type_name: CustomTypeName,
    /// Type permissions definitions
    pub permissions: TypePermissionOperand,
}

/// Configuration for type permissions
#[derive(Serialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(externally_tagged, json_schema(title = "TypePermissionOperand"))]
pub enum TypePermissionOperand {
    /// Definition of role-based type permissions on an OpenDD object type
    #[opendd(json_schema(title = "RoleBased"))]
    RoleBased(Vec<TypePermission>),
    /// Definition of rules-based type permissions on an OpenDD object type
    #[opendd(json_schema(title = "RulesBased"))]
    RulesBased(Vec<authorization::TypeAuthorizationRule>),
}

#[derive(Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "TypePermission", example = "TypePermission::example"))]
/// Defines permissions for a particular role for a type.
pub struct TypePermission {
    /// The role for which permissions are being defined.
    pub role: Role,
    /// Permissions for this role when this type is used in an output context.
    /// If null, this type is inaccessible for this role in an output context.
    pub output: Option<TypeOutputPermission>,
    /// Permissions for this role when this type is used in an input context.
    /// If null, this type is accessible for this role in an input context.
    pub input: Option<TypeInputPermission>,
}

impl TypePermission {
    fn example() -> serde_json::Value {
        serde_json::json!(
            {
                "role": "user",
                "output": {
                    "allowedFields": [
                        "article_id",
                        "author_id"
                    ]
                },
                "input": {
                    "fieldPresets": [
                        {
                            "field": "author_id",
                            "value": {
                                "sessionVariable": "x-hasura-user-id"
                            }
                        }
                    ]
                }
            }
        )
    }
}

#[derive(Deserialize, Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(json_schema(title = "TypeOutputPermission"))]
/// Permissions for a type for a particular role when used in an output context.
pub struct TypeOutputPermission {
    /// Fields of the type that are accessible for a role
    pub allowed_fields: IndexSet<FieldName>,
    // TODO: Presets for field arguments
    // pub field_argument_presets: HashMap<FieldName, Vec<ParameterPreset>>,
}

#[derive(Deserialize, Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(json_schema(title = "TypeInputPermission"))]
/// Permissions for a type for a particular role when used in an input context.
pub struct TypeInputPermission {
    /// Preset values for fields of the type
    #[opendd(default, json_schema(default_exp = "serde_json::json!([])"))]
    pub field_presets: Vec<FieldPreset>,
}

#[derive(Deserialize, Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
/// Preset value for a field
pub struct FieldPreset {
    /// Field name for preset
    pub field: FieldName,
    /// Value for preset
    pub value: ValueExpression,
}

#[derive(Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[opendd(
    as_versioned_with_definition,
    json_schema(
        title = "ModelPermissions",
        example = "ModelPermissions::v1_field_comparison_example",
        example = "ModelPermissions::v1_relationship_comparison_example",
        example = "ModelPermissions::v2_rules_based_argument_preset",
        example = "ModelPermissions::v2_rules_based_filters",
    )
)]
/// Definition of permissions for an OpenDD model.
pub enum ModelPermissions {
    V1(ModelPermissionsV1),
    V2(ModelPermissionsV2),
}

impl ModelPermissions {
    fn v1_field_comparison_example() -> serde_json::Value {
        serde_json::json!(
            {
                "kind": "ModelPermissions",
                "version": "v1",
                "definition": {
                    "modelName": "Articles",
                    "permissions": [
                        {
                            "role": "admin",
                            "select": {
                                "filter": null
                            }
                        },
                        {
                            "role": "user",
                            "select": {
                                "filter": {
                                    "fieldComparison": {
                                        "field": "author_id",
                                        "operator": "_eq",
                                        "value": {
                                            "sessionVariable": "x-hasura-user-id"
                                        }
                                    }
                                }
                            }
                        }
                    ]
                }
            }
        )
    }

    fn v1_relationship_comparison_example() -> serde_json::Value {
        serde_json::json!(
            {
                "kind": "ModelPermissions",
                "version": "v1",
                "definition": {
                    "modelName": "Articles",
                    "permissions": [
                        {
                            "role": "admin",
                            "select": {
                                "filter": null
                            }
                        },
                        {
                            "role": "user",
                            "select": {
                                "filter": {
                                    "relationship": {
                                        "name": "author",
                                        "predicate": {
                                            "fieldComparison": {
                                                "field": "id",
                                                "operator": "_eq",
                                                "value": {
                                                    "sessionVariable": "x-hasura-user-id"
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    ]
                }
            }
        )
    }

    fn v2_rules_based_argument_preset() -> serde_json::Value {
        serde_json::json!(
        {
          "kind": "ModelPermissions",
          "version": "v2",
          "definition": {
            "modelName": "actors_by_movie",
            "permissions": {
              "rulesBased": [
                {
                  "allow": {
                    "condition": {
                      "contains": {
                        "left": {
                          "sessionVariable": "x-hasura-role"
                        },
                        "right": {
                          "literal": ["admin", "user_with_preset_movie_id"]
                        }
                      }
                    }
                  }
                },
                {
                  "presetArgument": {
                    "condition": {
                      "equal": {
                        "left": {
                          "sessionVariable": "x-hasura-role"
                        },
                        "right": {
                          "literal": "user_with_preset_movie_id"
                        }
                      }
                    },
                    "argumentName": "movie_id",
                    "value": {
                      "literal": 1
                    }
                  }
                }
              ]
            }
          }
        }
        )
    }

    fn v2_rules_based_filters() -> serde_json::Value {
        serde_json::json!(
        {
          "kind": "ModelPermissions",
          "version": "v2",
          "definition": {
            "modelName": "actors",
            "permissions": {
              "rulesBased": [
                {
                  "allow": {
                    "condition": {
                      "contains": {
                        "left": {
                          "sessionVariable": "x-hasura-role"
                        },
                        "right": {
                          "literal": [
                            "admin",
                            "object_relationship_user"
                          ]
                        }
                      }
                    }
                  }
                },
                {
                  "filter": {
                    "condition": {
                      "equal": {
                        "left": {
                          "sessionVariable": "x-hasura-role"
                        },
                        "right": {
                          "literal": "object_relationship_user"
                        }
                      }
                    },
                    "predicate": {
                      "relationship": {
                        "name": "Country",
                        "predicate": {
                          "fieldComparison": {
                            "field": "name",
                            "operator": "_eq",
                            "value": {
                              "literal": "UK"
                            }
                          }
                        }
                      }
                    }
                  }
                }
              ]
            }
          }
        }
        )
    }

    pub fn upgrade(self) -> ModelPermissionsV2 {
        match self {
            ModelPermissions::V1(v1) => ModelPermissionsV2 {
                model_name: v1.model_name,
                permissions: ModelPermissionOperand::RoleBased(v1.permissions),
            },
            ModelPermissions::V2(v2) => v2,
        }
    }
}

#[derive(Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "ModelPermissionsV1"))]
/// Definition of permissions for an OpenDD model.
pub struct ModelPermissionsV1 {
    /// The name of the model for which permissions are being defined.
    pub model_name: Spanned<ModelName>,
    /// A list of model permissions, one for each role.
    pub permissions: Vec<ModelPermission>,
}

/// Configuration for role-based model permissions
#[derive(Serialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(externally_tagged, json_schema(title = "ModelPermissionOperand"))]
pub enum ModelPermissionOperand {
    /// Definition of role-based type permissions on an OpenDD model
    #[opendd(json_schema(title = "RoleBased"))]
    RoleBased(Vec<ModelPermission>),
    /// Definition of rules-based type permissions on an OpenDD model
    #[opendd(json_schema(title = "RulesBased"))]
    RulesBased(Vec<authorization::ModelAuthorizationRule>),
}

#[derive(Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "ModelPermissionsV2"))]
/// Definition of permissions for an OpenDD model.
pub struct ModelPermissionsV2 {
    /// The name of the model for which permissions are being defined.
    pub model_name: Spanned<ModelName>,
    /// Permissions for this model
    pub permissions: ModelPermissionOperand,
}

#[derive(Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "ModelPermission", example = "ModelPermission::example"))]
/// Defines the permissions for an OpenDD model.
pub struct ModelPermission {
    /// The role for which permissions are being defined.
    pub role: Spanned<Role>,
    /// The permissions for selecting from this model for this role.
    /// If this is null, the role is not allowed to query the model.
    pub select: Option<SelectPermission>,
    /// The permissions for relational insert operations on this model for this role.
    /// If this is null, the role is not allowed to perform relational inserts on this model.
    /// This is only applicable for data connectors that support relational operations.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub relational_insert: Option<RelationalInsertPermission>,
    /// The permissions for relational update operations on this model for this role.
    /// If this is null, the role is not allowed to perform relational updates on this model.
    /// This is only applicable for data connectors that support relational operations.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub relational_update: Option<RelationalUpdatePermission>,
    /// The permissions for relational delete operations on this model for this role.
    /// If this is null, the role is not allowed to perform relational deletes on this model.
    /// This is only applicable for data connectors that support relational operations.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub relational_delete: Option<RelationalDeletePermission>,
}

impl ModelPermission {
    fn example() -> serde_json::Value {
        serde_json::json!(
            {
                "role": "user",
                "select": {
                    "filter": {
                        "fieldComparison": {
                            "field": "author_id",
                            "operator": "_eq",
                            "value": {
                                "sessionVariable": "x-hasura-user-id"
                            }
                        }
                    },
                "argument_presets": [
                {
                    "field": "likes_dogs",
                    "value": {
                        "literal": true
                    }
                }]
                },
            }
        )
    }
}

#[derive(Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "SelectPermission"))]
/// Defines the permissions for selecting a model for a role.
pub struct SelectPermission {
    /// Filter expression when selecting rows for this model.
    /// Null filter implies all rows are selectable.
    pub filter: NullableModelPredicate,
    /// Preset values for arguments for this role
    #[opendd(default, json_schema(default_exp = "serde_json::json!([])"))]
    pub argument_presets: Vec<ArgumentPreset>,
    /// Whether the role is allowed to subscribe to the root fields of this model.
    #[opendd(default, json_schema(default_exp = "serde_json::json!(false)"))]
    pub allow_subscriptions: bool,
}

// We use this instead of an Option, so that we can make the filter field in
// SelectPermission required, but still accept an explicit null value.
// This is why we also need to use serde untagged.
#[derive(Serialize, Clone, Debug, Eq, PartialEq, JsonSchema)]
#[serde(untagged)]
pub enum NullableModelPredicate {
    Null(()),
    NotNull(ModelPredicate),
}

impl traits::OpenDd for NullableModelPredicate {
    fn deserialize(
        json: serde_json::Value,
        path: jsonpath::JSONPath,
    ) -> Result<Self, traits::OpenDdDeserializeError> {
        if json.is_null() {
            Ok(NullableModelPredicate::Null(()))
        } else {
            Ok(NullableModelPredicate::NotNull(OpenDd::deserialize(
                json, path,
            )?))
        }
    }

    fn json_schema(generator: &mut schemars::r#gen::SchemaGenerator) -> schemars::schema::Schema {
        <Self as schemars::JsonSchema>::json_schema(generator)
    }

    fn _schema_name() -> String {
        <Self as schemars::JsonSchema>::schema_name()
    }

    fn _schema_is_referenceable() -> bool {
        true
    }
}

impl NullableModelPredicate {
    pub fn as_option_ref(&self) -> Option<&ModelPredicate> {
        match self {
            NullableModelPredicate::Null(()) => None,
            NullableModelPredicate::NotNull(p) => Some(p),
        }
    }
}

#[derive(Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "CommandPermission", example = "CommandPermission::example"))]
/// Defines the permissions for a role for a command.
pub struct CommandPermission {
    /// The role for which permissions are being defined.
    pub role: Role,
    /// Whether the command is executable by the role.
    pub allow_execution: bool,
    /// Preset values for arguments for this role
    #[opendd(default, json_schema(default_exp = "serde_json::json!([])"))]
    pub argument_presets: Vec<ArgumentPreset>,
}

impl CommandPermission {
    fn example() -> serde_json::Value {
        serde_json::json!(
            {
                "role": "user",
                "allowExecution": true,
                "argumentPresets": [{
                    "argument": "user_id",
                    "value": {
                        "session_variable": "x-hasura-user_id"
                    }
                }]
            }
        )
    }
}

#[derive(Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[opendd(
    as_versioned_with_definition,
    json_schema(
        title = "CommandPermissions",
        example = "CommandPermissions::v1_example",
        example = "CommandPermissions::v2_example",
    )
)]
/// Definition of permissions for an OpenDD command.
pub enum CommandPermissions {
    V1(CommandPermissionsV1),
    V2(CommandPermissionsV2),
}

impl CommandPermissions {
    fn v1_example() -> serde_json::Value {
        serde_json::json!(
            {
                "kind": "CommandPermissions",
                "version": "v1",
                "definition": {
                    "commandName": "get_article_by_id",
                    "permissions": [
                        {
                            "role": "admin",
                            "allowExecution": true
                        },
                        {
                            "role": "user",
                            "allowExecution": true
                        }
                    ]
                }
            }
        )
    }

    fn v2_example() -> serde_json::Value {
        serde_json::json!(
        {
          "kind": "CommandPermissions",
          "version": "v2",
          "definition": {
            "commandName": "get_actors_with_filter",
            "permissions": {
              "rulesBased": [
                {
                  "allow": {
                    "condition": {
                      "contains": {
                        "left": {
                          "sessionVariable": "x-hasura-role"
                        },
                        "right": {
                          "literal": ["filter_user"]
                        }
                      }
                    }
                  }
                },
                {
                  "presetArgument": {
                    "condition": {
                      "equal": {
                        "left": {
                          "sessionVariable": "x-hasura-role"
                        },
                        "right": {
                          "literal": "filter_user"
                        }
                      }
                    },
                    "argumentName": "actor_bool_exp",
                    "value": {
                      "booleanExpression": {
                        "fieldComparison": {
                          "field": "actor_id",
                          "operator": "_eq",
                          "value": {
                            "literal": 4
                          }
                        }
                      }
                    }
                  }
                }
              ]
            }
          }
        }
        )
    }

    pub fn upgrade(self) -> CommandPermissionsV2 {
        match self {
            CommandPermissions::V1(v1) => CommandPermissionsV2 {
                command_name: v1.command_name,
                permissions: CommandPermissionOperand::RoleBased(v1.permissions),
            },
            CommandPermissions::V2(v2) => v2,
        }
    }
}

#[derive(Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "CommandPermissionsV1"))]
/// Definition of permissions for an OpenDD command.
pub struct CommandPermissionsV1 {
    /// The name of the command for which permissions are being defined.
    pub command_name: CommandName,
    /// A list of command permissions, one for each role.
    pub permissions: Vec<CommandPermission>,
}

/// Configuration for role-based command permissions
#[derive(Serialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(externally_tagged, json_schema(title = "CommandPermissionOperand"))]
pub enum CommandPermissionOperand {
    /// Definition of role-based permissions on an OpenDD command
    #[opendd(json_schema(title = "RoleBased"))]
    RoleBased(Vec<CommandPermission>),
    /// Definition of a rules-based permissions on an OpenDD command
    #[opendd(json_schema(title = "RulesBased"))]
    RulesBased(Vec<authorization::CommandAuthorizationRule>),
}

#[derive(Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "CommandPermissionsV2"))]
/// Definition of permissions for an OpenDD command.
pub struct CommandPermissionsV2 {
    /// The name of the command for which permissions are being defined.
    pub command_name: CommandName,
    /// The permissions for the command.
    pub permissions: CommandPermissionOperand,
}

#[derive(Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(json_schema(title = "FieldComparisonPredicate"))]
/// Field comparison predicate filters objects based on a field value.
pub struct FieldComparisonPredicate {
    /// The field name of the object type of the model to compare.
    pub field: Spanned<FieldName>,
    /// The name of the operator to use for comparison.
    pub operator: Spanned<OperatorName>,
    /// The value expression to compare against.
    // When we support custom operators, we can make this optional
    pub value: ValueExpression,
}

#[derive(Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(json_schema(title = "FieldIsNullPredicate"))]
/// Predicate to check if the given field is null.
pub struct FieldIsNullPredicate {
    /// The name of the field that should be checked for a null value.
    pub field: Spanned<FieldName>,
}

#[derive(Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(json_schema(title = "RelationshipPredicate"))]
/// Relationship predicate filters objects of a source model based on a predicate on the related model.
pub struct RelationshipPredicate {
    /// The name of the relationship of the object type of the model to follow.
    pub name: Spanned<RelationshipName>,
    /// The predicate to apply on the related objects. If this is null, then the predicate
    /// evaluates to true as long as there is at least one related object present.
    pub predicate: Option<Box<ModelPredicate>>,
}

#[derive(Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(json_schema(title = "NestedFieldPredicate"))]
/// Nested field predicate filters objects of a source model based on a predicate on the nested
/// field.
pub struct NestedFieldPredicate {
    /// The name of the field in the object type of the model to follow.
    pub field_name: Spanned<FieldName>,
    /// The predicate to apply on the related objects.
    pub predicate: Box<ModelPredicate>,
}

// Predicates that use NDC operators pushed down to NDC. `ValueExpressions` are
// evaluated on the server.
#[derive(Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(
    externally_tagged,
    json_schema(
        title = "ModelPredicate",
        example = "ModelPredicate::field_comparison_example",
        example = "ModelPredicate::relationship_comparison_example",
        example = "ModelPredicate::and_comparisons_example",
        example = "ModelPredicate::not_comparison_example"
    )
)]
/// A predicate that can be used to restrict the objects returned when querying a model.
pub enum ModelPredicate {
    /// Filters objects based on a field value.
    FieldComparison(FieldComparisonPredicate),
    FieldIsNull(FieldIsNullPredicate),
    // TODO: Remote relationships are disallowed for now
    /// Filters objects based on the nested field of a model.
    NestedField(NestedFieldPredicate),
    /// Filters objects based on the relationship of a model.
    Relationship(RelationshipPredicate),
    #[opendd(json_schema(title = "And"))]
    /// Evaluates to true if all sub-predicates evaluate to true.
    And(Vec<ModelPredicate>),
    #[opendd(json_schema(title = "Or"))]
    /// Evaluates to true if any of the sub-predicates evaluate to true.
    Or(Vec<ModelPredicate>),
    #[opendd(json_schema(title = "Not"))]
    /// Evaluates to true if the sub-predicate evaluates to false.
    Not(Box<ModelPredicate>),
    // TODO: Figure out the story with _ceq
}

impl_JsonSchema_with_OpenDd_for!(ModelPredicate);

impl ModelPredicate {
    fn field_comparison_example() -> JsonValue {
        serde_json::from_str(
            r#"
                {
                    "fieldComparison": {
                        "field": "author_id",
                        "operator": "_eq",
                        "value": {
                            "sessionVariable": "x-hasura-user-id"
                        }
                    }
                }
            "#,
        )
        .unwrap()
    }

    fn relationship_comparison_example() -> JsonValue {
        serde_json::from_str(
            r#"{
                "relationship": {
                    "name": "author",
                    "predicate": {
                        "fieldComparison": {
                            "field": "id",
                            "operator": "_eq",
                            "value": {
                                "sessionVariable": "x-hasura-user-id"
                            }
                        }
                    }
                }
            }"#,
        )
        .unwrap()
    }

    fn and_comparisons_example() -> JsonValue {
        serde_json::from_str(
            r#"{
                "and": [
                    {
                        "fieldComparison": {
                            "field": "author_id",
                            "operator": "_eq",
                            "value": {
                                "sessionVariable": "x-hasura-user-id"
                            }
                        }
                    },
                    {
                        "fieldComparison": {
                            "field": "title",
                            "operator": "_eq",
                            "value": {
                                "literal": "Hello World"
                            }
                        }
                    }
                ]
            }"#,
        )
        .unwrap()
    }

    fn not_comparison_example() -> JsonValue {
        serde_json::from_str(
            r#"{
                "not": {
                    "fieldComparison": {
                        "field": "author_id",
                        "operator": "_eq",
                        "value": {
                            "sessionVariable": "x-hasura-user-id"
                        }
                    }
                }
            }"#,
        )
        .unwrap()
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
/// An expression which evaluates to a value that can be used in permissions and
/// various presets.
// If you are adding a new variant, make sure to add it to the `ValueExpressionImpl` enum as well.
pub enum ValueExpression {
    #[serde(alias = "value")]
    Literal(JsonValue),
    SessionVariable(SessionVariableName),
}

#[derive(Serialize, Clone, Debug, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
/// An expression which evaluates to a value that can be used in permissions and
/// various presets.
// If you are adding a new variant, make sure to add it to the `ValueExpressionOrPredicateImpl` enum as well.
pub enum ValueExpressionOrPredicate {
    #[serde(alias = "value")]
    Literal(JsonValue),
    SessionVariable(SessionVariableName),
    BooleanExpression(Box<ModelPredicate>),
}

// Similar to `EnvironmentValue`, but for ValueExpression.
// We want to add environment variables to ValueExpression and be consistent with how we define environment variables in other places.
// Specifically, we want to be able to define environment variables in the metadata as:
// ```json
// {
//     "valueFromEnv": "ENV_VAR1"
// }
// ```

/// An expression which evaluates to a value that can be used in permissions and
/// various presets.
#[derive(Serialize, Clone, Debug, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(externally_tagged, json_schema(title = "ValueExpression"))]
// Either a literal value or a session variable or a reference to a Hasura secret
pub enum ValueExpressionImpl {
    #[opendd(json_schema(title = "Literal"), alias = "value")]
    Literal(JsonValue),
    #[opendd(json_schema(title = "SessionVariable"))]
    SessionVariable(SessionVariableName),
    #[opendd(json_schema(title = "ValueFromEnv"))]
    ValueFromEnv(String),
}

// Similar to `ValueExpressionImpl`, but for ValueExpressionOrPredicate.
/// An expression which evaluates to a value that can be used in permissions and
/// various presets.
#[derive(Serialize, Clone, Debug, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(externally_tagged, json_schema(title = "ValueExpressionOrPredicate"))]
// Either a literal value or a session variable or a boolean expression or a reference to a Hasura secret
pub enum ValueExpressionOrPredicateImpl {
    #[opendd(json_schema(title = "Literal"), alias = "value")]
    Literal(JsonValue),
    #[opendd(json_schema(title = "SessionVariable"))]
    SessionVariable(SessionVariableName),
    #[opendd(json_schema(title = "BooleanExpression"))]
    BooleanExpression(Box<ModelPredicate>),
    #[opendd(json_schema(title = "ValueFromEnv"))]
    ValueFromEnv(String),
}

impl traits::OpenDd for ValueExpression {
    fn deserialize(
        json: serde_json::Value,
        _path: jsonpath::JSONPath,
    ) -> Result<Self, traits::OpenDdDeserializeError> {
        serde_path_to_error::deserialize(json).map_err(|e| traits::OpenDdDeserializeError {
            path: jsonpath::JSONPath::from_serde_path(e.path()),
            error: e.into_inner(),
        })
    }
    fn json_schema(generator: &mut schemars::r#gen::SchemaGenerator) -> schemars::schema::Schema {
        let mut s = ValueExpressionImpl::json_schema(generator);
        if let schemars::schema::Schema::Object(o) = &mut s {
            if let Some(m) = &mut o.metadata {
                m.id = Some("https://hasura.io/jsonschemas/metadata/ValueExpression".into());
            }
        }
        s
    }
    fn _schema_name() -> String {
        "ValueExpression".to_owned()
    }
    fn _schema_is_referenceable() -> bool {
        true
    }
}

impl_JsonSchema_with_OpenDd_for!(ValueExpression);

impl traits::OpenDd for ValueExpressionOrPredicate {
    fn deserialize(
        json: serde_json::Value,
        path: jsonpath::JSONPath,
    ) -> Result<Self, OpenDdDeserializeError> {
        match OpenDd::deserialize(json, path.clone())? {
            ValueExpressionOrPredicateImpl::Literal(literal) => Ok(Self::Literal(literal)),
            ValueExpressionOrPredicateImpl::SessionVariable(session_variable) => {
                Ok(Self::SessionVariable(session_variable))
            }
            ValueExpressionOrPredicateImpl::BooleanExpression(predicate) => {
                Ok(Self::BooleanExpression(predicate))
            }
            ValueExpressionOrPredicateImpl::ValueFromEnv(_) => Err(OpenDdDeserializeError {
                path,
                error: serde_json::Error::custom(
                    "valueFromEnv should have been inlined into a literal during build",
                ),
            }),
        }
    }
    fn json_schema(generator: &mut schemars::r#gen::SchemaGenerator) -> schemars::schema::Schema {
        let mut s = ValueExpressionOrPredicateImpl::json_schema(generator);
        if let schemars::schema::Schema::Object(o) = &mut s {
            if let Some(m) = &mut o.metadata {
                m.id = Some(
                    "https://hasura.io/jsonschemas/metadata/ValueExpressionOrPredicate".into(),
                );
            }
        }
        s
    }
    fn _schema_name() -> String {
        "ValueExpressionOrPredicate".to_owned()
    }
    fn _schema_is_referenceable() -> bool {
        true
    }
}

impl_JsonSchema_with_OpenDd_for!(ValueExpressionOrPredicate);

#[derive(Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(json_schema(title = "RelationalInsertPermission"))]
/// Defines the permissions for relational insert operations on a model for a role.
/// If null, the role is not allowed to perform relational inserts on this model.
/// This is only applicable for data connectors that support relational operations.
pub struct RelationalInsertPermission {
    // Empty for now, will be extended later with filter predicates and argument presets
}

#[derive(Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(json_schema(title = "RelationalUpdatePermission"))]
/// Defines the permissions for relational update operations on a model for a role.
/// If null, the role is not allowed to perform relational updates on this model.
/// This is only applicable for data connectors that support relational operations.
pub struct RelationalUpdatePermission {
    // Empty for now, will be extended later with filter predicates and argument presets
}

#[derive(Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[opendd(json_schema(title = "RelationalDeletePermission"))]
/// Defines the permissions for relational delete operations on a model for a role.
/// If null, the role is not allowed to perform relational deletes on this model.
/// This is only applicable for data connectors that support relational operations.
pub struct RelationalDeletePermission {
    // Empty for now, will be extended later with filter predicates and argument presets
}

#[derive(Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[opendd(as_versioned_with_definition, json_schema(title = "ViewPermissions",))]
/// Definition of permissions for an OpenDD view.
pub enum ViewPermissions {
    V1(ViewPermissionsV1),
}

#[derive(Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "ViewPermissionsV1"))]
/// Definition of permissions for an OpenDD view.
pub struct ViewPermissionsV1 {
    /// The name of the view for which permissions are being defined.
    pub view_name: ViewName,
    /// View permissions definitions
    pub permissions: ViewPermissionOperand,
}

/// Configuration for view permissions
#[derive(Serialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[opendd(externally_tagged, json_schema(title = "ViewPermissionOperand"))]
pub enum ViewPermissionOperand {
    /// Definition of role-based view permissions on an OpenDD view
    #[opendd(json_schema(title = "RoleBased"))]
    RoleBased(Vec<ViewPermission>),
    /// Definition of rules-based view permissions on an OpenDD view
    #[opendd(json_schema(title = "RulesBased"))]
    RulesBased(Vec<authorization::ViewAuthorizationRule>),
}

#[derive(Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "ViewPermission"))]
/// Defines the permissions for a view for a role.
pub struct ViewPermission {
    /// The role for which permissions are being defined.
    pub role: Role,
    /// Whether access is allowed or denied for this role.
    pub allow: bool,
}
