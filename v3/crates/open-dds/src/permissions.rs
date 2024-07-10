use indexmap::IndexSet;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;

use crate::{
    arguments::ArgumentName,
    commands::CommandName,
    models::ModelName,
    relationships::RelationshipName,
    session_variables::SessionVariable,
    traits,
    types::{CustomTypeName, FieldName, OperatorName},
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
    pub argument: ArgumentName,
    /// Value for preset
    pub value: ValueExpressionOrPredicate,
}

#[derive(Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[opendd(
    as_versioned_with_definition,
    json_schema(title = "TypePermissions", example = "TypePermissions::example")
)]
/// Definition of permissions for an OpenDD type.
pub enum TypePermissions {
    V1(TypePermissionsV1),
}

impl TypePermissions {
    fn example() -> serde_json::Value {
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

    pub fn upgrade(self) -> TypePermissionsV1 {
        match self {
            TypePermissions::V1(v1) => v1,
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
    json_schema(title = "ModelPermissions", example = "ModelPermissions::example")
)]
/// Definition of permissions for an OpenDD model.
pub enum ModelPermissions {
    V1(ModelPermissionsV1),
}

impl ModelPermissions {
    fn example() -> serde_json::Value {
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

    pub fn upgrade(self) -> ModelPermissionsV1 {
        match self {
            ModelPermissions::V1(v1) => v1,
        }
    }
}

#[derive(Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "ModelPermissionsV1"))]
/// Definition of permissions for an OpenDD model.
pub struct ModelPermissionsV1 {
    /// The name of the model for which permissions are being defined.
    pub model_name: ModelName,
    /// A list of model permissions, one for each role.
    pub permissions: Vec<ModelPermission>,
}

#[derive(Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "ModelPermission", example = "ModelPermission::example"))]
/// Defines the permissions for an OpenDD model.
pub struct ModelPermission {
    /// The role for which permissions are being defined.
    pub role: Role,
    /// The permissions for selecting from this model for this role.
    /// If this is null, the role is not allowed to query the model.
    pub select: Option<SelectPermission>,
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
    fn deserialize(json: serde_json::Value) -> Result<Self, traits::OpenDdDeserializeError> {
        if json.is_null() {
            Ok(NullableModelPredicate::Null(()))
        } else {
            Ok(NullableModelPredicate::NotNull(
                serde_path_to_error::deserialize(json).map_err(|e| {
                    traits::OpenDdDeserializeError {
                        path: traits::JSONPath::from_serde_path(e.path()),
                        error: e.into_inner(),
                    }
                })?,
            ))
        }
    }

    fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        <Self as schemars::JsonSchema>::json_schema(gen)
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
    json_schema(title = "CommandPermissions", example = "CommandPermissions::example")
)]
/// Definition of permissions for an OpenDD command.
pub enum CommandPermissions {
    V1(CommandPermissionsV1),
}

impl CommandPermissions {
    fn example() -> serde_json::Value {
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

    pub fn upgrade(self) -> CommandPermissionsV1 {
        match self {
            CommandPermissions::V1(v1) => v1,
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

#[derive(Serialize, Deserialize, Clone, Debug, Eq, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "FieldComparisonPredicate")]
/// Field comparison predicate filters objects based on a field value.
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
#[schemars(title = "FieldIsNullPredicate")]
/// Predicate to check if the given field is null.
pub struct FieldIsNullPredicate {
    /// The name of the field that should be checked for a null value.
    pub field: FieldName,
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
    FieldIsNull(FieldIsNullPredicate),
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

#[derive(
    Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema, opendds_derive::OpenDd,
)]
#[serde(rename_all = "camelCase")]
#[schemars(title = "ValueExpression")]
/// An expression which evaluates to a value that can be used in permissions and
/// various presets.
pub enum ValueExpression {
    #[schemars(title = "Literal")]
    Literal(JsonValue),
    #[schemars(title = "SessionVariable")]
    SessionVariable(SessionVariable),
}

#[derive(
    Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema, opendds_derive::OpenDd,
)]
#[serde(rename_all = "camelCase")]
#[schemars(title = "ValueExpressionOrPredicate")]
/// An expression which evaluates to a value that can be used in permissions and
/// various presets.
pub enum ValueExpressionOrPredicate {
    #[schemars(title = "Literal")]
    Literal(JsonValue),
    #[schemars(title = "SessionVariable")]
    SessionVariable(SessionVariable),
    #[schemars(title = "BooleanExpression")]
    BooleanExpression(Box<ModelPredicate>),
}
