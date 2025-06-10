use std::collections::{BTreeMap, BTreeSet};

use error_context::{Context, Step};
use hasura_authn_core::Role;
use open_dds::{
    glossary::{GlossaryName, GlossaryTermDescription, GlossaryTermName},
    spanned::Spanned,
};
use serde::{Deserialize, Serialize};

use crate::types::{
    error::{ContextualError, ShouldBeAnError},
    subgraph::Qualified,
};

pub struct GlossaryOutput {
    pub glossaries: BTreeMap<Qualified<GlossaryName>, Glossary>,
    pub issues: Vec<GlossaryIssue>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct Glossary {
    pub name: Qualified<GlossaryName>,
    pub terms: BTreeMap<GlossaryTermName, GlossaryTermDescription>,
    pub roles_with_access: BTreeSet<Role>,
}

#[derive(thiserror::Error, Debug)]
#[allow(clippy::enum_variant_names)]
pub enum GlossaryIssue {
    #[error(
        "Duplicate glossary term name: Term '{term_name}' for role '{role}' in glossary '{glossary_name}' is already defined in glossary '{other_glossary_name}'"
    )]
    DuplicateTermInAnotherGlossary {
        glossary_name: Spanned<Qualified<GlossaryName>>,
        term_name: Spanned<GlossaryTermName>,
        role: Spanned<Role>,
        other_glossary_name: Spanned<Qualified<GlossaryName>>,
    },
}

impl ShouldBeAnError for GlossaryIssue {
    fn should_be_an_error(&self, _flags: &open_dds::flags::OpenDdFlags) -> bool {
        false
    }
}

impl ContextualError for GlossaryIssue {
    fn create_error_context(&self) -> Option<Context> {
        match self {
            Self::DuplicateTermInAnotherGlossary {
                glossary_name,
                term_name,
                role,
                other_glossary_name,
            } => Some(
                Context::from_step(Step {
                    message: format!(
                        "Term name '{}' for role '{}' defined in glossary '{}'",
                        term_name.value, role.value, glossary_name.value.name,
                    ),
                    path: term_name.path.clone(),
                    subgraph: Some(glossary_name.value.subgraph.clone()),
                })
                .append(Step {
                    message: format!(
                        "Term is also defined in glossary '{}'",
                        other_glossary_name.value.name,
                    ),
                    path: other_glossary_name.path.clone(),
                    subgraph: Some(other_glossary_name.value.subgraph.clone()),
                }),
            ),
        }
    }
}
