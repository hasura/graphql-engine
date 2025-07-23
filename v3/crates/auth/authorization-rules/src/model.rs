use std::collections::BTreeMap;

use super::condition::evaluate_optional_condition_hash;
use crate::{ArgumentPolicy, ConditionCache, condition::ConditionError, has_access::HasAccess};
use hasura_authn_core::SessionVariables;
use metadata_resolve::{
    Conditions, ModelAuthorizationRule, ModelPredicate, RelationalDeletePermission,
    RelationalInsertPermission, RelationalOperation, RelationalUpdatePermission,
};
use open_dds::query::ArgumentName;

#[derive(Debug, PartialEq, Eq)]
// where possible, return references to the original data in the metadata
// to avoid cloning them until we actually need them
pub struct ModelPermission<'a> {
    pub filters: Vec<&'a ModelPredicate>,
    pub allow_subscriptions: bool,
    pub argument_presets: BTreeMap<&'a ArgumentName, ArgumentPolicy<'a>>,
    pub relational_insert: Option<RelationalInsertPermission>,
    pub relational_update: Option<RelationalUpdatePermission>,
    pub relational_delete: Option<RelationalDeletePermission>,
}

impl Default for ModelPermission<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl ModelPermission<'_> {
    pub fn new() -> Self {
        Self {
            filters: vec![],
            allow_subscriptions: false,
            argument_presets: BTreeMap::new(),
            relational_insert: None,
            relational_update: None,
            relational_delete: None,
        }
    }
}

// given a vector of model authorization rules, evaluate them and return all permissions
// that apply if the user is allowed to access the model at all.
pub fn evaluate_model_authorization_rules<'a>(
    model_rules: &'a Vec<ModelAuthorizationRule>,
    session_variables: &SessionVariables,
    conditions: &Conditions,
    condition_cache: &mut ConditionCache,
) -> Result<Option<ModelPermission<'a>>, ConditionError> {
    // are we even allowed to access this model?
    let mut access = HasAccess::default();

    // are we allowed to subscribe to this model?
    let mut subscriptions = HasAccess::default();

    // collect all the actual filtering we're going to do
    let mut select_permissions = vec![];

    // collect all the argument presets
    let mut argument_presets = BTreeMap::new();

    // relational permissions
    let mut relational_insert = HasAccess::default();
    let mut relational_update = HasAccess::default();
    let mut relational_delete = HasAccess::default();

    for model_rule in model_rules {
        match model_rule {
            ModelAuthorizationRule::Access {
                allow_or_deny,
                condition,
            } => {
                if evaluate_optional_condition_hash(
                    condition.as_ref(),
                    session_variables,
                    conditions,
                    condition_cache,
                )? {
                    access.set(allow_or_deny);
                }
            }
            ModelAuthorizationRule::Subscription {
                allow_or_deny,
                condition,
            } => {
                if evaluate_optional_condition_hash(
                    condition.as_ref(),
                    session_variables,
                    conditions,
                    condition_cache,
                )? {
                    subscriptions.set(allow_or_deny);
                }
            }
            ModelAuthorizationRule::Filter {
                predicate,
                condition,
            } => {
                if evaluate_optional_condition_hash(
                    condition.as_ref(),
                    session_variables,
                    conditions,
                    condition_cache,
                )? {
                    select_permissions.push(predicate);
                }
            }
            ModelAuthorizationRule::ArgumentPresetValue {
                argument_name,
                argument_type,
                value,
                condition,
            } => {
                if evaluate_optional_condition_hash(
                    condition.as_ref(),
                    session_variables,
                    conditions,
                    condition_cache,
                )? {
                    argument_presets.insert(
                        argument_name,
                        ArgumentPolicy::ValueExpression {
                            argument_type,
                            value_expression: value,
                        },
                    );
                }
            }
            ModelAuthorizationRule::ArgumentAuthPredicate {
                argument_name,
                predicate,
                condition,
            } => {
                if evaluate_optional_condition_hash(
                    condition.as_ref(),
                    session_variables,
                    conditions,
                    condition_cache,
                )? {
                    match argument_presets.get_mut(argument_name) {
                        // if there are already items, add to them
                        Some(ArgumentPolicy::BooleanExpression { predicates }) => {
                            predicates.push(predicate);
                        }
                        // no item for the argument yet, insert new item
                        None => {
                            let _ = argument_presets.insert(
                                argument_name,
                                ArgumentPolicy::BooleanExpression {
                                    predicates: vec![predicate],
                                },
                            );
                        }
                        Some(ArgumentPolicy::ValueExpression { .. }) => {
                            // throw an error as we have a mixture of preset types
                            // this _shouldn't_ happen due to build time checks that a
                            // preset value matches the underlying argument type
                            return Err(
                                ConditionError::CouldNotCombinePredicateAndLiteralArgumentPresets {
                                    argument_name: argument_name.clone(),
                                },
                            );
                        }
                    }
                }
            }
            ModelAuthorizationRule::RelationalPermission {
                allow_or_deny,
                relational_operation,
                condition,
            } => {
                if evaluate_optional_condition_hash(
                    condition.as_ref(),
                    session_variables,
                    conditions,
                    condition_cache,
                )? {
                    match relational_operation {
                        RelationalOperation::Insert => {
                            relational_insert.set(allow_or_deny);
                        }
                        RelationalOperation::Update => {
                            relational_update.set(allow_or_deny);
                        }
                        RelationalOperation::Delete => {
                            relational_delete.set(allow_or_deny);
                        }
                    }
                }
            }
        }
    }

    if !access.has_access() {
        // no access to this model
        return Ok(None);
    }

    Ok(Some(ModelPermission {
        filters: select_permissions,
        argument_presets,
        allow_subscriptions: subscriptions.has_access(),
        relational_insert: if relational_insert.has_access() {
            Some(RelationalInsertPermission {})
        } else {
            None
        },
        relational_update: if relational_update.has_access() {
            Some(RelationalUpdatePermission {})
        } else {
            None
        },
        relational_delete: if relational_delete.has_access() {
            Some(RelationalDeletePermission {})
        } else {
            None
        },
    }))
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use crate::{
        ArgumentPolicy, ConditionCache,
        model::{ModelPermission, evaluate_model_authorization_rules},
    };

    use hasura_authn_core::{Identity, Role};
    use metadata_resolve::{
        AllowOrDeny, BinaryOperation, Condition, Conditions, ModelAuthorizationRule,
        ModelPredicate, Qualified, RelationalDeletePermission, RelationalInsertPermission,
        RelationalOperation, RelationalUpdatePermission, UnaryComparisonOperator, ValueExpression,
    };
    use open_dds::{
        identifier::{Identifier, SubgraphName},
        query::ArgumentName,
        types::{CustomTypeName, FieldName},
    };

    #[test]
    fn test_evaluate_model_authorization_rules() {
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

        // by default, allow nothing
        assert_eq!(
            evaluate_model_authorization_rules(
                &vec![],
                &session.variables,
                &Conditions::new(),
                &mut condition_cache
            )
            .unwrap(),
            None
        );

        let mut conditions = Conditions::new();

        let allow_condition = equals(true_val.clone(), true_val.clone());

        let condition_id = conditions.add(allow_condition);

        let allow_rule = ModelAuthorizationRule::Access {
            condition: Some(condition_id),
            allow_or_deny: AllowOrDeny::Allow,
        };

        assert_eq!(
            evaluate_model_authorization_rules(
                &vec![allow_rule.clone()],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            Some(ModelPermission::new())
        );

        let deny_condition = equals(true_val.clone(), true_val);

        let condition_id = conditions.add(deny_condition);

        let deny_rule = ModelAuthorizationRule::Access {
            condition: Some(condition_id),
            allow_or_deny: AllowOrDeny::Deny,
        };

        assert_eq!(
            evaluate_model_authorization_rules(
                &vec![deny_rule.clone()],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            None
        );

        // deny takes precedence
        assert_eq!(
            evaluate_model_authorization_rules(
                &vec![allow_rule.clone(), deny_rule.clone()],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            None
        );

        // ...irrespective of ordering
        assert_eq!(
            evaluate_model_authorization_rules(
                &vec![deny_rule, allow_rule.clone()],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            None
        );

        // we collect all the relevant filters and combine them with AND
        let predicate = ModelPredicate::UnaryFieldComparison {
            field: FieldName::new(Identifier::new("horse").unwrap()),
            field_parent_type: Qualified::new(
                SubgraphName::new_without_validation("subgraph"),
                CustomTypeName(Identifier::new("Integer").unwrap()),
            ),
            ndc_column: "column".into(),
            operator: UnaryComparisonOperator::IsNull,
            column_path: vec![],
            deprecated: None,
        };

        let filter_rule = ModelAuthorizationRule::Filter {
            predicate: predicate.clone(),
            condition: None,
        };

        assert_eq!(
            evaluate_model_authorization_rules(
                &vec![filter_rule.clone(), allow_rule.clone(), filter_rule],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            Some(ModelPermission {
                filters: vec![&predicate, &predicate],
                ..ModelPermission::new()
            })
        );

        // default is not subscription
        assert_eq!(
            evaluate_model_authorization_rules(
                &vec![allow_rule.clone()],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            Some(ModelPermission::new())
        );

        let deny_subscription_rule = ModelAuthorizationRule::Subscription {
            allow_or_deny: AllowOrDeny::Deny,
            condition: None,
        };

        // deny subscription
        assert_eq!(
            evaluate_model_authorization_rules(
                &vec![allow_rule.clone(), deny_subscription_rule.clone()],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            Some(ModelPermission::new())
        );

        let allow_subscription_rule = ModelAuthorizationRule::Subscription {
            allow_or_deny: AllowOrDeny::Allow,
            condition: None,
        };

        // allow subscription
        assert_eq!(
            evaluate_model_authorization_rules(
                &vec![allow_rule.clone(), allow_subscription_rule.clone()],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            Some(ModelPermission {
                allow_subscriptions: true,
                ..ModelPermission::new()
            })
        );

        // deny takes precedence
        assert_eq!(
            evaluate_model_authorization_rules(
                &vec![
                    allow_rule.clone(),
                    deny_subscription_rule.clone(),
                    allow_subscription_rule.clone()
                ],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            Some(ModelPermission::new())
        );

        assert_eq!(
            evaluate_model_authorization_rules(
                &vec![allow_rule, allow_subscription_rule, deny_subscription_rule],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            Some(ModelPermission::new())
        );
    }

    #[test]
    fn test_evaluate_model_argument_preset_boolean_expression_rule() {
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

        let mut conditions = Conditions::new();

        let allow_condition = equals(true_val.clone(), true_val);

        let condition_id = conditions.add(allow_condition);

        let allow_model_access_rule = ModelAuthorizationRule::Access {
            allow_or_deny: AllowOrDeny::Allow,
            condition: Some(condition_id),
        };

        // no presets set
        assert_eq!(
            evaluate_model_authorization_rules(
                &vec![allow_model_access_rule.clone()],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            Some(ModelPermission::new())
        );

        // we collect all the relevant filters and combine them with AND
        let predicate = ModelPredicate::UnaryFieldComparison {
            field: FieldName::new(Identifier::new("horse").unwrap()),
            field_parent_type: Qualified::new(
                SubgraphName::new_without_validation("subgraph"),
                CustomTypeName(Identifier::new("Integer").unwrap()),
            ),
            ndc_column: "column".into(),
            operator: UnaryComparisonOperator::IsNull,
            column_path: vec![],
            deprecated: None,
        };

        let one_predicate_rule = ModelAuthorizationRule::ArgumentAuthPredicate {
            argument_name: ArgumentName::new(Identifier::new("filter").unwrap()),
            predicate: predicate.clone(),
            condition: Some(condition_id),
        };

        // one filter is set
        assert_eq!(
            evaluate_model_authorization_rules(
                &vec![allow_model_access_rule.clone(), one_predicate_rule.clone()],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            Some(ModelPermission {
                argument_presets: BTreeMap::from_iter(vec![(
                    &ArgumentName::new(Identifier::new("filter").unwrap()),
                    ArgumentPolicy::BooleanExpression {
                        predicates: vec![&predicate]
                    }
                )]),
                ..ModelPermission::new()
            })
        );

        // two filters are set
        assert_eq!(
            evaluate_model_authorization_rules(
                &vec![
                    allow_model_access_rule,
                    one_predicate_rule.clone(),
                    one_predicate_rule
                ],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            Some(ModelPermission {
                argument_presets: BTreeMap::from_iter(vec![(
                    &ArgumentName::new(Identifier::new("filter").unwrap()),
                    ArgumentPolicy::BooleanExpression {
                        predicates: vec![&predicate, &predicate]
                    }
                )]),
                ..ModelPermission::new()
            })
        );
    }

    #[test]
    fn test_evaluate_model_relational_insert_rule() {
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

        let mut conditions = Conditions::new();

        let allow_condition = equals(true_val.clone(), true_val);

        let condition_id = conditions.add(allow_condition);

        let allow_model_access_rule = ModelAuthorizationRule::Access {
            allow_or_deny: AllowOrDeny::Allow,
            condition: Some(condition_id),
        };

        // no relational operation permissions set
        assert_eq!(
            evaluate_model_authorization_rules(
                &vec![allow_model_access_rule.clone()],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            Some(ModelPermission::new())
        );

        let allow_insert_rule = ModelAuthorizationRule::RelationalPermission {
            allow_or_deny: AllowOrDeny::Allow,
            relational_operation: RelationalOperation::Insert,
            condition: Some(condition_id),
        };

        // insert is allowed
        assert_eq!(
            evaluate_model_authorization_rules(
                &vec![allow_model_access_rule.clone(), allow_insert_rule.clone()],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            Some(ModelPermission {
                relational_insert: Some(RelationalInsertPermission {}),
                ..ModelPermission::new()
            })
        );

        let deny_insert_rule = ModelAuthorizationRule::RelationalPermission {
            allow_or_deny: AllowOrDeny::Deny,
            relational_operation: RelationalOperation::Insert,
            condition: Some(condition_id),
        };

        // deny takes precedence over allow
        assert_eq!(
            evaluate_model_authorization_rules(
                &vec![allow_model_access_rule, allow_insert_rule, deny_insert_rule],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            Some(ModelPermission::new())
        );
    }

    #[test]
    fn test_evaluate_model_relational_update_rule() {
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

        let mut conditions = Conditions::new();

        let allow_condition = equals(true_val.clone(), true_val);

        let condition_id = conditions.add(allow_condition);

        let allow_model_access_rule = ModelAuthorizationRule::Access {
            allow_or_deny: AllowOrDeny::Allow,
            condition: Some(condition_id),
        };

        // no relational operation permissions set
        assert_eq!(
            evaluate_model_authorization_rules(
                &vec![allow_model_access_rule.clone()],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            Some(ModelPermission::new())
        );

        let allow_update_rule = ModelAuthorizationRule::RelationalPermission {
            allow_or_deny: AllowOrDeny::Allow,
            relational_operation: RelationalOperation::Update,
            condition: Some(condition_id),
        };

        // update is allowed
        assert_eq!(
            evaluate_model_authorization_rules(
                &vec![allow_model_access_rule.clone(), allow_update_rule.clone()],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            Some(ModelPermission {
                relational_update: Some(RelationalUpdatePermission {}),
                ..ModelPermission::new()
            })
        );

        let deny_update_rule = ModelAuthorizationRule::RelationalPermission {
            allow_or_deny: AllowOrDeny::Deny,
            relational_operation: RelationalOperation::Update,
            condition: Some(condition_id),
        };

        // deny takes precedence over allow
        assert_eq!(
            evaluate_model_authorization_rules(
                &vec![allow_model_access_rule, allow_update_rule, deny_update_rule],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            Some(ModelPermission::new())
        );
    }

    #[test]
    fn test_evaluate_model_relational_delete_rule() {
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

        let mut conditions = Conditions::new();

        let allow_condition = equals(true_val.clone(), true_val);

        let condition_id = conditions.add(allow_condition);

        let allow_model_access_rule = ModelAuthorizationRule::Access {
            allow_or_deny: AllowOrDeny::Allow,
            condition: Some(condition_id),
        };

        // no relational operation permissions set
        assert_eq!(
            evaluate_model_authorization_rules(
                &vec![allow_model_access_rule.clone()],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            Some(ModelPermission::new())
        );

        let allow_delete_rule = ModelAuthorizationRule::RelationalPermission {
            allow_or_deny: AllowOrDeny::Allow,
            relational_operation: RelationalOperation::Delete,
            condition: Some(condition_id),
        };

        // delete is allowed
        assert_eq!(
            evaluate_model_authorization_rules(
                &vec![allow_model_access_rule.clone(), allow_delete_rule.clone()],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            Some(ModelPermission {
                relational_delete: Some(RelationalDeletePermission {}),
                ..ModelPermission::new()
            })
        );

        let deny_delete_rule = ModelAuthorizationRule::RelationalPermission {
            allow_or_deny: AllowOrDeny::Deny,
            relational_operation: RelationalOperation::Delete,
            condition: Some(condition_id),
        };

        // deny takes precedence over allow
        assert_eq!(
            evaluate_model_authorization_rules(
                &vec![allow_model_access_rule, allow_delete_rule, deny_delete_rule],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            Some(ModelPermission::new())
        );
    }
}
