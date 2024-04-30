use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{
    arguments::ArgumentName,
    commands::CommandName,
    identifier::Identifier,
    impl_JsonSchema_with_OpenDd_for,
    models::ModelName,
    permissions::ValueExpression,
    types::{CustomTypeName, Deprecated, FieldName},
};

/// The name of the GraphQL relationship field.
#[derive(
    Serialize,
    Deserialize,
    Clone,
    Debug,
    PartialEq,
    Eq,
    derive_more::Display,
    Hash,
    opendds_derive::OpenDd,
)]
pub struct RelationshipName(pub Identifier);

impl_JsonSchema_with_OpenDd_for!(RelationshipName);

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
/// Type of the relationship.
#[schemars(title = "RelationshipType")]
pub enum RelationshipType {
    /// Select one related object from the target.
    Object,
    /// Select multiple related objects from the target.
    Array,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "ModelRelationshipTarget")]
/// The target model for a relationship.
pub struct ModelRelationshipTarget {
    /// The name of the data model.
    pub name: ModelName,
    /// The subgraph of the target model. Defaults to the current subgraph.
    subgraph: Option<String>,
    /// Type of the relationship - object or array.
    pub relationship_type: RelationshipType,
}

impl ModelRelationshipTarget {
    pub fn subgraph(&self) -> Option<&str> {
        self.subgraph.as_deref()
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "CommandRelationshipTarget")]
/// The target command for a relationship.
pub struct CommandRelationshipTarget {
    /// The name of the command.
    pub name: CommandName,
    /// The subgraph of the target command. Defaults to the current subgraph.
    pub subgraph: Option<String>,
}

#[derive(
    Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema, opendds_derive::OpenDd,
)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "RelationshipTarget")]
#[schemars(example = "RelationshipTarget::example")]
/// The target for a relationship.
pub enum RelationshipTarget {
    Model(ModelRelationshipTarget),
    Command(CommandRelationshipTarget),
}

impl RelationshipTarget {
    fn example() -> Self {
        serde_json::from_str(
            r#"
            {
                "model": {
                  "name": "Articles",
                  "relationshipType": "Array"
                }
            }
        "#,
        )
        .unwrap()
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "RelationshipSourceFieldAccess")]
pub struct FieldAccess {
    pub field_name: FieldName,
    // #[serde(default)]
    // pub arguments: HashMap<ArgumentName, ValueExpression>,
}

#[derive(
    Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema, opendds_derive::OpenDd,
)]
#[serde(rename_all = "camelCase")]
#[schemars(title = "RelationshipMappingSource")]
/// The source configuration for a relationship mapping.
pub enum RelationshipMappingSource {
    #[schemars(title = "SourceValue")]
    Value(ValueExpression),
    #[schemars(title = "SourceField")]
    FieldPath(Vec<FieldAccess>),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "ArgumentMappingTarget")]
pub struct ArgumentMappingTarget {
    pub argument_name: ArgumentName,
}

#[derive(
    Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema, opendds_derive::OpenDd,
)]
#[serde(rename_all = "camelCase")]
#[schemars(title = "RelationshipMappingTarget")]
/// The target configuration for a relationship mapping.
pub enum RelationshipMappingTarget {
    #[schemars(title = "TargetArgument")]
    Argument(ArgumentMappingTarget),
    #[schemars(title = "TargetModelField")]
    ModelField(Vec<FieldAccess>),
}

#[derive(Serialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(
    title = "RelationshipMapping",
    example = "RelationshipMapping::example"
))]
/// Definition of a how a particular field in the source maps to a target field or argument.
pub struct RelationshipMapping {
    /// The source configuration for this relationship mapping.
    pub source: RelationshipMappingSource,
    /// The target configuration for this relationship mapping.
    pub target: RelationshipMappingTarget,
}

impl RelationshipMapping {
    fn example() -> serde_json::Value {
        serde_json::json!(
            {
                "source": {
                    "fieldPath": [
                        {
                            "fieldName": "author_id"
                        }
                    ]
                },
                "target": {
                    "modelField": [
                        {
                            "fieldName": "author_id"
                        }
                    ]
                }
            }
        )
    }
}

#[derive(Serialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[opendd(
    as_versioned_with_definition,
    json_schema(title = "Relationship", example = "Relationship::example")
)]
/// Definition of a relationship on an OpenDD type which allows it to be extended with related models or commands.
pub enum Relationship {
    V1(RelationshipV1),
}

impl Relationship {
    fn example() -> serde_json::Value {
        serde_json::json!(
            {
                "kind": "Relationship",
                "version": "v1",
                "definition": {
                    "name": "Articles",
                    "source": "author",
                    "target": {
                        "model": {
                            "name": "Articles",
                            "subgraph": null,
                            "relationshipType": "Array"
                        }
                    },
                    "mapping": [
                        {
                            "source": {
                                "fieldPath": [
                                    {
                                        "fieldName": "author_id"
                                    }
                                ]
                            },
                            "target": {
                                "modelField": [
                                    {
                                        "fieldName": "author_id"
                                    }
                                ]
                            }
                        }
                    ],
                    "description": "Articles written by an author"
                }
            }
        )
    }

    pub fn upgrade(self) -> RelationshipV1 {
        match self {
            Relationship::V1(v1) => v1,
        }
    }
}

#[derive(Serialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "RelationshipV1"))]
/// Definition of a relationship on an OpenDD type which allows it to be extended with related models or commands.
pub struct RelationshipV1 {
    /// The name of the relationship.
    pub name: RelationshipName,
    /// The source type of the relationship.
    #[serde(alias = "source")]
    #[opendd(alias = "source")]
    pub source_type: CustomTypeName,
    /// The target of the relationship.
    pub target: RelationshipTarget,
    /// The mapping configuration of source to target for the relationship.
    pub mapping: Vec<RelationshipMapping>,
    /// The description of the relationship.
    /// Gets added to the description of the relationship in the graphql schema.
    pub description: Option<String>,
    /// Whether this relationship is deprecated.
    /// If set, the deprecation status is added to the relationship field's graphql schema.
    pub deprecated: Option<Deprecated>,
}
