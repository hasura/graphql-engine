use hasura_authn_core::SessionVariables;
use metadata_resolve::{Conditions, ViewAuthorizationRule};

use crate::{
    ConditionCache, ConditionError, condition::evaluate_optional_condition_hash,
    has_access::HasAccess,
};

pub fn evaluate_view_authorization_rules(
    rules: &Vec<ViewAuthorizationRule>,
    session_variables: &SessionVariables,
    conditions: &Conditions,
    condition_cache: &mut ConditionCache,
) -> Result<bool, ConditionError> {
    let mut has_access = HasAccess::default();

    for rule in rules {
        match rule {
            ViewAuthorizationRule::Access {
                condition,
                allow_or_deny,
            } => {
                if evaluate_optional_condition_hash(
                    condition.as_ref(),
                    session_variables,
                    conditions,
                    condition_cache,
                )? {
                    has_access.set(allow_or_deny);
                }
            }
        }
    }
    Ok(has_access.has_access())
}
