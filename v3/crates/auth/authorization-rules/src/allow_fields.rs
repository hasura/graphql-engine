use std::collections::BTreeSet;

use hasura_authn_core::SessionVariables;
use indexmap::IndexMap;
use metadata_resolve::{Conditions, FieldAuthorizationRule};
use open_dds::types::FieldName;

use crate::{
    ConditionCache,
    condition::{ConditionError, evaluate_optional_condition_hash},
};

// Given a vector of field authorization rules, evaluate them and return the set of fields that
// should be allowed for this request.
//
// The following permission primitives are available when defining a permissions rule for a type.
// - Allow access to certain fields
// - Deny access to certain fields
//
// If a field is both allowed and denied, it will be denied.
pub fn evaluate_field_authorization_rules<'a, A>(
    rule: &'a Vec<FieldAuthorizationRule>,
    all_fields: &'a IndexMap<FieldName, A>,
    session_variables: &SessionVariables,
    conditions: &Conditions,
    condition_cache: &mut ConditionCache,
) -> Result<IndexMap<&'a FieldName, &'a A>, ConditionError> {
    let mut allowed_fields = BTreeSet::new();
    let mut denied_fields = BTreeSet::new();

    for field_rule in rule {
        match field_rule {
            FieldAuthorizationRule::AllowFields { fields, condition } => {
                if evaluate_optional_condition_hash(
                    condition.as_ref(),
                    session_variables,
                    conditions,
                    condition_cache,
                )? {
                    for field in fields {
                        allowed_fields.insert(field);
                    }
                }
            }
            FieldAuthorizationRule::DenyFields { fields, condition } => {
                if evaluate_optional_condition_hash(
                    condition.as_ref(),
                    session_variables,
                    conditions,
                    condition_cache,
                )? {
                    for field in fields {
                        denied_fields.insert(field);
                    }
                }
            }
        }
    }

    let field_names_to_keep: BTreeSet<&FieldName> =
        allowed_fields.difference(&denied_fields).copied().collect();

    Ok(all_fields
        .iter()
        .filter_map(|(field_name, field)| {
            if field_names_to_keep.contains(field_name) {
                Some((field_name, field))
            } else {
                None
            }
        })
        .collect())
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use super::*;
    use hasura_authn_core::{Identity, Role};
    use indexmap::IndexMap;
    use metadata_resolve::{BinaryOperation, Condition, FieldAuthorizationRule, ValueExpression};
    use open_dds::identifier::Identifier;

    #[test]
    fn test_evaluate_field_authorization_rules() {
        let true_val = ValueExpression::Literal(serde_json::Value::Bool(true));

        let equals = |left: ValueExpression, right: ValueExpression| Condition::BinaryOperation {
            left,
            right,
            op: BinaryOperation::Equals,
        };

        let session_variables = BTreeMap::new();

        let mut condition_cache = ConditionCache::new();

        // return an arbitrary identity with role emulation enabled
        let authorization = Identity::admin(Role::new("admin"));
        let role = Role::new("admin");
        let role_authorization = authorization.get_role_authorization(Some(&role)).unwrap();

        let session = role_authorization.build_session(session_variables);

        let mut all_fields = IndexMap::new();

        all_fields.insert(FieldName::new(Identifier::new("field1").unwrap()), ());
        all_fields.insert(FieldName::new(Identifier::new("field2").unwrap()), ());
        all_fields.insert(FieldName::new(Identifier::new("field3").unwrap()), ());

        // by default, allow nothing
        assert_eq!(
            evaluate_field_authorization_rules(
                &vec![],
                &all_fields,
                &session.variables,
                &Conditions::new(),
                &mut condition_cache
            )
            .unwrap()
            .len(),
            0
        );

        let fields_list = vec![
            FieldName::new(Identifier::new("field1").unwrap()),
            FieldName::new(Identifier::new("field2").unwrap()),
            FieldName::new(Identifier::new("field3").unwrap()),
        ];

        let mut conditions = Conditions::new();

        let allow_condition = equals(true_val.clone(), true_val.clone());

        let condition_id = conditions.add(allow_condition);

        let allow_all_fields_rule = FieldAuthorizationRule::AllowFields {
            fields: fields_list.clone(),
            condition: Some(condition_id),
        };

        assert_eq!(
            evaluate_field_authorization_rules(
                &vec![allow_all_fields_rule.clone()],
                &all_fields,
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap()
            .len(),
            3
        );

        let deny_condition = equals(true_val.clone(), true_val);

        let condition_id = conditions.add(deny_condition);

        let deny_all_fields_rule = FieldAuthorizationRule::DenyFields {
            fields: fields_list,
            condition: Some(condition_id),
        };

        assert_eq!(
            evaluate_field_authorization_rules(
                &vec![deny_all_fields_rule.clone()],
                &all_fields,
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap()
            .len(),
            0
        );

        // deny takes precedence
        assert_eq!(
            evaluate_field_authorization_rules(
                &vec![allow_all_fields_rule.clone(), deny_all_fields_rule.clone()],
                &all_fields,
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap()
            .len(),
            0
        );

        // ...irrespective of ordering
        assert_eq!(
            evaluate_field_authorization_rules(
                &vec![allow_all_fields_rule, deny_all_fields_rule],
                &all_fields,
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap()
            .len(),
            0
        );
    }
}
