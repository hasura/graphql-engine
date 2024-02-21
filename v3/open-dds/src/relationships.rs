use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{
    arguments::ArgumentName,
    commands::CommandName,
    models::ModelName,
    permissions::ValueExpression,
    types::{CustomTypeName, FieldName},
};

/// The name of the GraphQL relationship field.
#[derive(
    Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema, derive_more::Display, Hash,
)]
pub struct RelationshipName(pub String);

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
    // Deprecated, this solely exits for backwards compatibility till all the
    // tooling moves to the subgraph terminology
    namespace: Option<String>,
    /// The subgraph of the target model. Defaults to the current subgraph.
    subgraph: Option<String>,
    /// Type of the relationship - object or array.
    pub relationship_type: RelationshipType,
}

impl ModelRelationshipTarget {
    pub fn subgraph(&self) -> Option<&str> {
        self.subgraph
            .as_ref()
            .or(self.namespace.as_ref())
            .map(|x| x.as_str())
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

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
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

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
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

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[schemars(title = "RelationshipMappingTarget")]
/// The target configuration for a relationship mapping.
pub enum RelationshipMappingTarget {
    #[schemars(title = "TargetArgument")]
    Argument(ArgumentMappingTarget),
    #[schemars(title = "TargetModelField")]
    ModelField(Vec<FieldAccess>),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(deny_unknown_fields)]
#[schemars(title = "RelationshipMapping")]
#[schemars(example = "RelationshipMapping::example")]
/// Definition of a how a particular field in the source maps to a target field or argument.
pub struct RelationshipMapping {
    /// The source configuration for this relationship mapping.
    pub source: RelationshipMappingSource,
    /// The target configuration for this relationship mapping.
    pub target: RelationshipMappingTarget,
}

impl RelationshipMapping {
    fn example() -> Self {
        serde_json::from_str(
            r#"
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
        "#,
        )
        .unwrap()
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "Relationship")]
/// Definition of a relationship on an OpenDD type which allows it to be extended with related models or commands.
pub enum Relationship {
    V1(RelationshipV1),
}

impl Relationship {
    pub fn upgrade(self) -> RelationshipV1 {
        match self {
            Relationship::V1(v1) => v1,
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "RelationshipV1")]
#[schemars(example = "RelationshipV1::example")]
/// Definition of a relationship on an OpenDD type which allows it to be extended with related models or commands.
pub struct RelationshipV1 {
    /// The name of the relationship.
    pub name: RelationshipName,
    /// The source type of the relationship.
    pub source: CustomTypeName,
    /// The target of the relationship.
    pub target: RelationshipTarget,
    /// The mapping configuration of source to target for the relationship.
    pub mapping: Vec<RelationshipMapping>,
    /// The description of the relationship.  
    /// Gets added to the description of the relationship in the graphql schema.
    pub description: Option<String>,
}

impl RelationshipV1 {
    fn example() -> Self {
        serde_json::from_str(
            r#"
            {
                "source": "author",
                "name": "Articles",
                "description": "Articles written by an author",
                "target": {
                  "model": {
                    "name": "Articles",
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
                ]
            }
        "#,
        )
        .unwrap()
    }
}
