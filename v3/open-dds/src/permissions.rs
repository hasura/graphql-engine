use derive_more::Display;
use indexmap::IndexSet;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;

use crate::{
    commands::CommandName,
    models::{ModelName, OperatorName},
    relationships::RelationshipName,
    session_variables::SessionVariable,
    types::{CustomTypeName, FieldName},
};

#[derive(Serialize, Deserialize, Clone, Debug, Eq, PartialEq, JsonSchema, Hash, Display)]
pub struct Role(pub String);

impl Role {
    pub fn new(str: &str) -> Role {
        Role(str.to_string())
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, Eq, PartialEq, JsonSchema)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "TypePermissions")]
/// Definition of permissions for an OpenDD type.
pub enum TypePermissions {
    V1(TypePermissionsV1),
}

impl TypePermissions {
    pub fn upgrade(self) -> TypePermissionsV1 {
        match self {
            TypePermissions::V1(v1) => v1,
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, Eq, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "TypePermissionsV1")]
#[schemars(example = "TypePermissionsV1::example")]
/// Definition of permissions for an OpenDD type.
pub struct TypePermissionsV1 {
    /// The name of the type for which permissions are being defined. Must be an object type.
    pub type_name: CustomTypeName,
    /// A list of type permissions, one for each role.
    pub permissions: Vec<TypePermission>,
}

impl TypePermissionsV1 {
    fn example() -> Self {
        serde_json::from_str(
            r#"
            {
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
        "#,
        )
        .unwrap()
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, Eq, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "TypePermission")]
#[schemars(example = "TypePermission::example")]
/// Defines permissions for a particular role for a type.
pub struct TypePermission {
    /// The role for which permissions are being defined.
    pub role: Role,
    /// Permissions for this role when this type is used in an output context.
    /// If null, this type is inaccessible for this role in an output context.
    pub output: Option<TypeOutputPermission>,
}

impl TypePermission {
    fn example() -> Self {
        serde_json::from_str(
            r#"
            {
                "role": "user",
                "output": {
                  "allowedFields": [
                    "article_id",
                    "author_id"
                  ]
                }
            }
        "#,
        )
        .unwrap()
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, Eq, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "TypeOutputPermission")]
/// Permissions for a type for a particular role when used in an output context.
pub struct TypeOutputPermission {
    /// Fields of the type that are accessible for a role
    pub allowed_fields: IndexSet<FieldName>,
    // TODO: Presets for field arguments
    // pub field_argument_presets: HashMap<FieldName, Vec<ParameterPreset>>,
}

#[derive(Serialize, Deserialize, Clone, Debug, Eq, PartialEq, JsonSchema)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "ModelPermissions")]
/// Definition of permissions for an OpenDD model.
pub enum ModelPermissions {
    V1(ModelPermissionsV1),
}

impl ModelPermissions {
    pub fn upgrade(self) -> ModelPermissionsV1 {
        match self {
            ModelPermissions::V1(v1) => v1,
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, Eq, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "ModelPermissionsV1")]
#[schemars(example = "ModelPermissionsV1::example")]
/// Definition of permissions for an OpenDD model.
pub struct ModelPermissionsV1 {
    /// The name of the model for which permissions are being defined.
    pub model_name: ModelName,
    /// A list of model permissions, one for each role.
    pub permissions: Vec<ModelPermission>,
}

impl ModelPermissionsV1 {
    fn example() -> Self {
        serde_json::from_str(
            r#"
            {
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
        "#,
        )
        .unwrap()
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, Eq, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "ModelPermission")]
#[schemars(example = "ModelPermission::example")]
/// Defines the permissions for an OpenDD model.
pub struct ModelPermission {
    /// The role for which permissions are being defined.
    pub role: Role,
    /// The permissions for selecting from this model for this role.
    /// If this is null, the role is not allowed to query the model.
    pub select: Option<SelectPermission>,
}

impl ModelPermission {
    fn example() -> Self {
        serde_json::from_str(
            r#"
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
        "#,
        )
        .unwrap()
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, Eq, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "SelectPermission")]
/// Defines the permissions for selecting a model for a role.
pub struct SelectPermission {
    /// Filter expression when selecting rows for this model.
    /// Null filter implies all rows are selectable.
    pub filter: NullableModelPredicate,
    //TODO: Implement the following when aggregate queries are introduced
    // #[serde(default)]
    // pub allow_aggregations: bool,
}

// We use this instead of an Option, so that we can make the filter field in
// SelectPermission required, but still accept an explicit null value.
// This is why we also need to use serde untagged.
#[derive(Serialize, Deserialize, Clone, Debug, Eq, PartialEq, JsonSchema)]
#[serde(untagged)]
pub enum NullableModelPredicate {
    Null(()),
    NotNull(ModelPredicate),
}

impl NullableModelPredicate {
    pub fn as_option_ref(&self) -> Option<&ModelPredicate> {
        match self {
            NullableModelPredicate::Null(_) => None,
            NullableModelPredicate::NotNull(p) => Some(p),
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, Eq, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "CommandPermission")]
#[schemars(example = "CommandPermission::example")]
/// Defines the permissions for a role for a command.
pub struct CommandPermission {
    /// The role for which permissions are being defined.
    pub role: Role,
    // TODO: Implement predicates and presets
    /// Whether the command is executable by the role.
    pub allow_execution: bool,
}

impl CommandPermission {
    fn example() -> Self {
        serde_json::from_str(
            r#"
            {
                "role": "user",
                "allowExecution": true
            }
        "#,
        )
        .unwrap()
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, Eq, PartialEq, JsonSchema)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "CommandPermissions")]
/// Definition of permissions for an OpenDD command.
pub enum CommandPermissions {
    V1(CommandPermissionsV1),
}

impl CommandPermissions {
    pub fn upgrade(self) -> CommandPermissionsV1 {
        match self {
            CommandPermissions::V1(v1) => v1,
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, Eq, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "CommandPermissionsV1")]
#[schemars(example = "CommandPermissionsV1::example")]
/// Definition of permissions for an OpenDD command.
pub struct CommandPermissionsV1 {
    /// The name of the command for which permissions are being defined.
    pub command_name: CommandName,
    /// A list of command permissions, one for each role.
    pub permissions: Vec<CommandPermission>,
}

impl CommandPermissionsV1 {
    fn example() -> Self {
        serde_json::from_str(
            r#"
            {
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
        "#,
        )
        .unwrap()
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, Eq, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "FieldComparisonPredicate")]
/// Field comparision predicate filters objects based on a field value.
pub struct FieldComparisonPredicate {
    /// The field name of the object type of the model to compare.
    pub field: FieldName,
    /// The name of the operator to use for comparison.
    pub operator: OperatorName,
    /// The value expression to compare against.
    // When we support custom operators, we can make this optional
    pub value: ValueExpression,
}

#[derive(Serialize, Deserialize, Clone, Debug, Eq, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "RelationshipPredicate")]
/// Relationship predicate filters objects of a source model based on a predicate on the related model.
pub struct RelationshipPredicate {
    /// The name of the relationship of the object type of the model to follow.
    pub name: RelationshipName,
    /// The predicate to apply on the related objects. If this is null, then the predicate
    /// evaluates to true as long as there is at least one related object present.
    pub predicate: Option<Box<ModelPredicate>>,
}

// Predicates that use NDC operators pushed down to NDC. `ValueExpressions` are
// evaluated on the server.
#[derive(Serialize, Deserialize, Clone, Debug, Eq, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[schemars(title = "ModelPredicate")]
#[schemars(example = "ModelPredicate::example")]
#[serde(deny_unknown_fields)]
/// A predicate that can be used to restrict the objects returned when querying a model.
pub enum ModelPredicate {
    /// Filters objects based on a field value.
    FieldComparison(FieldComparisonPredicate),
    FieldIsNull {
        field: FieldName,
    },
    // TODO: Remote relationships are disallowed for now
    /// Filters objects based on the relationship of a model.
    Relationship(RelationshipPredicate),
    #[schemars(title = "And")]
    /// Evaluates to true if all sub-predicates evaluate to true.
    And(Vec<ModelPredicate>),
    #[schemars(title = "Or")]
    /// Evaluates to true if any of the sub-predicates evaluate to true.
    Or(Vec<ModelPredicate>),
    #[schemars(title = "Not")]
    /// Evaluates to true if the sub-predicate evaluates to false.
    Not(Box<ModelPredicate>),
    // TODO: Figure out the story with _ceq
}

impl ModelPredicate {
    fn example() -> Self {
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
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[schemars(title = "ValueExpression")]
/// An expression which evaluates to a value that can be used in permissions.
pub enum ValueExpression {
    #[schemars(title = "Literal")]
    Literal(JsonValue),
    #[schemars(title = "SessionVariable")]
    SessionVariable(SessionVariable),
    // TODO: Uncomment the below, once commands are supported.
    // Command {
    //     name: CommandName,
    //     arguments: HashMap<ArgumentName, ValueExpression>,
    // },
}
