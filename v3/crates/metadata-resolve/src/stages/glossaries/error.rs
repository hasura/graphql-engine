use error_context::{Context, Step};
use hasura_authn_core::Role;
use open_dds::{
    glossary::{GlossaryName, GlossaryTermName},
    spanned::Spanned,
};

use crate::types::{error::ContextualError, subgraph::Qualified};

#[derive(thiserror::Error, Debug)]
#[allow(clippy::enum_variant_names)]
pub enum GlossaryError {
    #[error("Duplicate glossary name '{glossary_name}'")]
    DuplicateName {
        glossary_name: Spanned<Qualified<GlossaryName>>,
    },
    #[error("Duplicate glossary term name '{term_name}' in glossary '{glossary_name}'")]
    DuplicateTermName {
        glossary_name: Spanned<Qualified<GlossaryName>>,
        term_name: Spanned<GlossaryTermName>,
    },
    #[error("Duplicate glossary permission for role '{role}' in glossary '{glossary_name}'")]
    DuplicatePermission {
        glossary_name: Spanned<Qualified<GlossaryName>>,
        role: Spanned<Role>,
    },
}

impl ContextualError for GlossaryError {
    fn create_error_context(&self) -> Option<Context> {
        match self {
            Self::DuplicateName { glossary_name } => Some(Context::from_step(Step {
                message: format!("Duplicate glossary name '{}'", glossary_name.value.name),
                path: glossary_name.path.clone(),
                subgraph: Some(glossary_name.value.subgraph.clone()),
            })),

            Self::DuplicateTermName {
                glossary_name,
                term_name,
            } => Some(Context::from_step(Step {
                message: format!(
                    "Duplicate glossary term name '{}' in glossary '{}'",
                    term_name.value, glossary_name.value.name
                ),
                path: term_name.path.clone(),
                subgraph: Some(glossary_name.value.subgraph.clone()),
            })),

            Self::DuplicatePermission {
                glossary_name,
                role,
            } => Some(Context::from_step(Step {
                message: format!(
                    "Duplicate glossary permission for role '{}' in glossary '{}'",
                    role.value, glossary_name.value.name
                ),
                path: role.path.clone(),
                subgraph: Some(glossary_name.value.subgraph.clone()),
            })),
        }
    }
}
