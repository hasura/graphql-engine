mod error;
mod types;

use hasura_authn_core::Role;
use open_dds::{
    glossary::{GlossaryName, GlossaryPermission, GlossaryTerm, GlossaryTermName},
    spanned::Spanned,
};
use std::collections::{BTreeMap, BTreeSet};

use crate::types::subgraph::Qualified;
pub use error::GlossaryError;
pub use types::{Glossary, GlossaryIssue, GlossaryOutput};

// resolve glossaries. we only care about duplicate glossary names and duplicate term names within a glossary
// since permissions are simple, we just drop any reference to a role that has no access to a
// glossary
pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
) -> Result<GlossaryOutput, Vec<GlossaryError>> {
    let mut glossaries = BTreeMap::new();
    let mut results = Vec::new();
    let mut term_names_by_role = BTreeMap::new();
    let mut issues = Vec::new();

    for open_dds::accessor::QualifiedObject {
        path: _,
        subgraph,
        object: glossary,
    } in &metadata_accessor.glossaries
    {
        results.push(process_glossary(
            Qualified::new(subgraph.clone(), glossary.name.clone()).transpose_spanned(),
            glossary,
            &mut glossaries,
            &mut term_names_by_role,
            &mut issues,
        ));
    }

    // Return all accumulated errors or return mutable maps containing glossaries and issues
    partition_eithers::collect_any_errors(results).map(|_| GlossaryOutput { glossaries, issues })
}

fn process_glossary(
    glossary_name: Spanned<Qualified<GlossaryName>>,
    glossary: &open_dds::glossary::GlossaryV1,
    // This mutable map accumulates the glossaries
    resolved_glossaries: &mut BTreeMap<Qualified<GlossaryName>, Glossary>,
    term_names_by_role: &mut BTreeMap<
        Role,
        BTreeMap<GlossaryTermName, Spanned<Qualified<GlossaryName>>>,
    >,
    issues: &mut Vec<GlossaryIssue>,
) -> Result<(), GlossaryError> {
    let mut terms = BTreeMap::new();
    for GlossaryTerm { name, description } in &glossary.terms {
        if terms.contains_key(&name.value) {
            return Err(GlossaryError::DuplicateTermName {
                glossary_name,
                term_name: name.clone(),
            });
        }

        terms.insert(name.value.clone(), description.clone());
    }

    let mut roles_with_access = BTreeSet::new();
    let mut roles_defined = BTreeSet::new();
    for GlossaryPermission { role, allow_view } in &glossary.permissions {
        if roles_defined.contains(&role.value) {
            return Err(GlossaryError::DuplicatePermission {
                glossary_name,
                role: role.clone(),
            });
        }
        roles_defined.insert(role.value.clone());
        if *allow_view {
            roles_with_access.insert(role.value.clone());
        }
    }

    // check for duplicate term names across glossaries
    for GlossaryTerm { name, .. } in &glossary.terms {
        // maintain a map of all terms defined by role to avoid duplicates across glossaries
        for GlossaryPermission { role, .. } in &glossary.permissions {
            if let Some(other_glossary_name) = term_names_by_role
                .entry(role.value.clone())
                .or_default()
                .insert(name.value.clone(), glossary_name.clone())
            {
                issues.push(GlossaryIssue::DuplicateTermInAnotherGlossary {
                    glossary_name: glossary_name.clone(),
                    role: role.clone(),
                    term_name: name.clone(),
                    other_glossary_name,
                });
            }
        }
    }

    let glossary = Glossary {
        name: glossary_name.value.clone(),
        terms,
        roles_with_access,
    };

    if resolved_glossaries
        .insert(glossary_name.value.clone(), glossary)
        .is_some()
    {
        Err(GlossaryError::DuplicateName { glossary_name })
    } else {
        Ok(())
    }
}
