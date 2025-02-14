use crate::parse;
use hasura_authn_core::Role;
use metadata_resolve::Qualified;
use open_dds::{
    identifier::SubgraphName, models::ModelName, relationships::RelationshipType,
    types::CustomTypeName,
};
use std::collections::BTreeMap;
use tracing_util::{ErrorVisibility, TraceableError};

#[derive(Debug, Clone)]
pub enum Warning {
    Role { role: Role, warning: RoleWarning },
}

#[derive(Debug, Clone)]
pub enum RoleWarning {
    Model {
        model_name: Qualified<ModelName>,
        warning: ModelWarning,
    },
    ObjectType {
        object_type_name: Qualified<CustomTypeName>,
        warning: ObjectTypeWarning,
    },
}

// if we exclude something, let's say why
#[derive(Debug, Clone)]
#[allow(clippy::enum_variant_names)]
pub enum ObjectTypeWarning {
    NoObjectTypePermission {},
    NestedObjectNotFound {
        object_type_name: Qualified<CustomTypeName>,
    },
}

// if we exclude something, let's say why
#[derive(Debug, Clone)]
#[allow(clippy::enum_variant_names)]
pub enum ModelWarning {
    NoSelectPermission,
    NoObjectTypeFound {
        object_type_name: Qualified<CustomTypeName>,
    },
    NoModelSource,
}

#[derive(Debug, derive_more::Display)]
pub enum RequestError {
    NotFound,
    BadRequest(String),
    InternalError(InternalError),
    PlanError(plan::PlanError),
    ExecuteError(execute::FieldError),
    ParseError(parse::ParseError),
}

impl RequestError {
    pub fn to_error_response(&self) -> serde_json::Value {
        match self {
            RequestError::ParseError(err) => serde_json::json!({"error": err}),
            RequestError::BadRequest(err) => serde_json::json!({"error": err}),
            RequestError::NotFound => {
                serde_json::json!({"error": "invalid route or path"})
            }
            RequestError::InternalError(InternalError::EmptyQuerySet)
            | RequestError::PlanError(
                plan::PlanError::Internal(_)
                | plan::PlanError::InternalError(_)
                | plan::PlanError::ArgumentPresetExecutionError(_)
                | plan::PlanError::Relationship(_)
                | plan::PlanError::OrderBy(_)
                | plan::PlanError::External(_),
            ) => {
                serde_json::json!({"error": "Internal error" })
            }
            RequestError::PlanError(plan::PlanError::Permission(_msg)) => {
                serde_json::json!({"error": "Access forbidden" })
            }
            RequestError::ExecuteError(field_error) => {
                serde_json::json!({"error": field_error.to_string() })
            }
        }
    }
}

#[derive(Debug, derive_more::Display)]
pub enum InternalError {
    EmptyQuerySet,
}

impl TraceableError for RequestError {
    fn visibility(&self) -> ErrorVisibility {
        ErrorVisibility::User
    }
}

/// Model related info derived from URI path
#[allow(dead_code)]
pub struct ModelInfo {
    pub subgraph: SubgraphName,
    pub name: ModelName,
    /// value of the unique identifier of the model.
    // TODO: this won't be a string always
    pub unique_identifier: Option<String>,
    /// path to a relationship; like `["artist", "albums", "tracks"]`
    pub relationship: Vec<String>,
}

/// A tree of relationships, used in processing of relationships in the JSON:API response creation
#[derive(Default)]
pub struct RelationshipTree {
    pub relationships: BTreeMap<String, RelationshipNode>,
}

pub struct RelationshipNode {
    pub object_type: Qualified<CustomTypeName>,
    pub relationship_type: RelationshipType,
    pub is_command_relationship: bool,
    pub nested: RelationshipTree,
}
