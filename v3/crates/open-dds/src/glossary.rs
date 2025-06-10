use serde::Serialize;

use crate::{identifier::Identifier, permissions::Role, spanned::Spanned, str_newtype};

#[derive(Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[opendd(
    as_versioned_with_definition,
    json_schema(title = "Glossary", example = "Glossary::example")
)]
/// Definition of a glossary item
pub enum Glossary {
    V1(GlossaryV1),
}

impl Glossary {
    fn example() -> serde_json::Value {
        serde_json::json!(
            {
                "kind": "Glossary",
                "version": "v1",
                "definition": {
                    "name": "Surfing",
                    "terms": [
                      {
                          "name": "Bailing",
                          "description": "Letting go of your surfboard"
                      },
                      {
                          "name": "Deck",
                          "description": "The top of the surfboard"
                      },
                      {
                          "name": "Knot",
                          "description": "A unit of speed equal to one nautical mile per hour"
                      }
                    ],
                    "permissions": [
                        {
                            "role": "surfer",
                            "allowView": true
                        }
                    ]
                }
            }
        )
    }

    pub fn upgrade(self) -> GlossaryV1 {
        match self {
            Glossary::V1(v1) => v1,
        }
    }
}

str_newtype!(GlossaryName over Identifier | doc "The name of a glossary.");

#[derive(Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
/// Definition of a glossary item - version 1.
pub struct GlossaryV1 {
    /// The name of this glossary
    pub name: Spanned<GlossaryName>,
    /// A map of domain terms to their meanings
    pub terms: Vec<GlossaryTerm>,
    /// Which roles are allowed to view this glossary
    pub permissions: Vec<GlossaryPermission>,
}

#[derive(Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
/// A single domain term and its definition
pub struct GlossaryTerm {
    /// A map of domain terms to their meanings
    pub name: Spanned<GlossaryTermName>,
    /// Description of this domain term
    pub description: GlossaryTermDescription,
}

str_newtype!(GlossaryTermName | doc "The name of an domain term.");

str_newtype!(GlossaryTermDescription | doc "The description of an domain term.");

#[derive(Serialize, Clone, Debug, Eq, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
/// The permissions a role has to view this glossary item
pub struct GlossaryPermission {
    /// The role for which permissions are being defined.
    pub role: Spanned<Role>,
    /// Can this role view this glossary item?  
    pub allow_view: bool,
}
