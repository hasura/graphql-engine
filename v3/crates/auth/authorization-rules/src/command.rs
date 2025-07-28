use std::collections::BTreeMap;

use super::has_access::HasAccess;
use hasura_authn_core::SessionVariables;
use metadata_resolve::{
    CommandAuthorizationRule, Conditions, ModelPredicate, QualifiedTypeReference, ValueExpression,
};
use open_dds::query::ArgumentName;

use crate::{
    ConditionCache,
    condition::{ConditionError, evaluate_optional_condition_hash},
};

#[derive(Debug, PartialEq, Eq)]
pub enum ArgumentPolicy<'a> {
    ValueExpression {
        argument_type: &'a QualifiedTypeReference,
        value_expression: &'a ValueExpression,
    },
    BooleanExpression {
        predicates: Vec<&'a ModelPredicate>,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub struct CommandPermission<'a> {
    pub argument_presets: BTreeMap<&'a ArgumentName, ArgumentPolicy<'a>>,
}

// Given a vector of field authorization rules, evaluate them and return the set of fields that
// should be allowed for this request.
//
// The following permission primitives are available when defining a permissions rule for a type.
// - Allow access to certain fields
// - Deny access to certain fields
//
// If a field is both allowed and denied, it will be denied.
pub fn evaluate_command_authorization_rules<'a>(
    rule: &'a Vec<CommandAuthorizationRule>,
    session_variables: &SessionVariables,
    conditions: &Conditions,
    condition_cache: &mut ConditionCache,
) -> Result<Option<CommandPermission<'a>>, ConditionError> {
    let mut allow_access = HasAccess::default();
    let mut argument_presets = BTreeMap::new();

    for command_rule in rule {
        match command_rule {
            CommandAuthorizationRule::Access {
                allow_or_deny,
                condition,
            } => {
                if evaluate_optional_condition_hash(
                    condition.as_ref(),
                    session_variables,
                    conditions,
                    condition_cache,
                )? {
                    allow_access.set(allow_or_deny);
                }
            }
            CommandAuthorizationRule::ArgumentPresetValue {
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
            CommandAuthorizationRule::ArgumentAuthPredicate {
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
        }
    }

    if !allow_access.has_access() {
        return Ok(None);
    }

    Ok(Some(CommandPermission { argument_presets }))
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use super::*;
    use hasura_authn_core::{Identity, Role};
    use metadata_resolve::{
        AllowOrDeny, BinaryOperation, Condition, Qualified, QualifiedBaseType, QualifiedTypeName,
        UnaryComparisonOperator, ValueExpression,
    };
    use open_dds::{
        identifier::{Identifier, SubgraphName},
        types::{CustomTypeName, FieldName},
    };

    #[test]
    fn test_evaluate_command_access_rule() {
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
            evaluate_command_authorization_rules(
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

        let allow_command_access_rule = CommandAuthorizationRule::Access {
            allow_or_deny: AllowOrDeny::Allow,
            condition: Some(condition_id),
        };

        // allow command access
        assert_eq!(
            evaluate_command_authorization_rules(
                &vec![allow_command_access_rule.clone()],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            Some(CommandPermission {
                argument_presets: BTreeMap::new()
            })
        );

        let deny_condition = equals(true_val.clone(), true_val);

        let condition_id = conditions.add(deny_condition);

        let deny_command_access_rule = CommandAuthorizationRule::Access {
            allow_or_deny: AllowOrDeny::Deny,
            condition: Some(condition_id),
        };

        assert_eq!(
            evaluate_command_authorization_rules(
                &vec![deny_command_access_rule.clone()],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            None
        );

        // deny takes precedence
        assert_eq!(
            evaluate_command_authorization_rules(
                &vec![
                    deny_command_access_rule.clone(),
                    allow_command_access_rule.clone()
                ],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            None
        );

        // ...irrespective of ordering
        assert_eq!(
            evaluate_command_authorization_rules(
                &vec![allow_command_access_rule, deny_command_access_rule],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            None
        );
    }

    #[test]
    fn test_evaluate_command_argument_preset_literal_rule() {
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

        let allow_command_access_rule = CommandAuthorizationRule::Access {
            allow_or_deny: AllowOrDeny::Allow,
            condition: Some(condition_id),
        };

        // no presets set
        assert_eq!(
            evaluate_command_authorization_rules(
                &vec![allow_command_access_rule.clone()],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            Some(CommandPermission {
                argument_presets: BTreeMap::new()
            })
        );

        let argument_type = QualifiedTypeReference {
            underlying_type: QualifiedBaseType::Named(QualifiedTypeName::Inbuilt(
                open_dds::types::InbuiltType::String,
            )),
            nullable: false,
        };

        let name_1_value_expression =
            ValueExpression::Literal(serde_json::Value::String("Mr Horse".to_string()));

        let name_argument_literal_rule = CommandAuthorizationRule::ArgumentPresetValue {
            argument_name: ArgumentName::new(Identifier::new("name").unwrap()),
            argument_type: argument_type.clone(),
            value: name_1_value_expression.clone(),
            condition: Some(condition_id),
        };

        // name is set
        assert_eq!(
            evaluate_command_authorization_rules(
                &vec![
                    allow_command_access_rule.clone(),
                    name_argument_literal_rule.clone()
                ],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            Some(CommandPermission {
                argument_presets: BTreeMap::from_iter(vec![(
                    &ArgumentName::new(Identifier::new("name").unwrap()),
                    ArgumentPolicy::ValueExpression {
                        argument_type: &argument_type,
                        value_expression: &name_1_value_expression
                    }
                )])
            })
        );

        let name_2_value_expression =
            ValueExpression::Literal(serde_json::Value::String("Mr Horse 2".to_string()));

        let name_argument_literal_rule_2 = CommandAuthorizationRule::ArgumentPresetValue {
            argument_name: ArgumentName::new(Identifier::new("name").unwrap()),
            argument_type: argument_type.clone(),
            value: name_2_value_expression.clone(),
            condition: Some(condition_id),
        };

        // name is set to second value
        assert_eq!(
            evaluate_command_authorization_rules(
                &vec![
                    allow_command_access_rule,
                    name_argument_literal_rule,
                    name_argument_literal_rule_2
                ],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            Some(CommandPermission {
                argument_presets: BTreeMap::from_iter(vec![(
                    &ArgumentName::new(Identifier::new("name").unwrap()),
                    ArgumentPolicy::ValueExpression {
                        argument_type: &argument_type,
                        value_expression: &name_2_value_expression
                    }
                )])
            })
        );
    }

    #[test]
    fn test_evaluate_command_argument_preset_boolean_expression_rule() {
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

        let allow_command_access_rule = CommandAuthorizationRule::Access {
            allow_or_deny: AllowOrDeny::Allow,
            condition: Some(condition_id),
        };

        // no presets set
        assert_eq!(
            evaluate_command_authorization_rules(
                &vec![allow_command_access_rule.clone()],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            Some(CommandPermission {
                argument_presets: BTreeMap::new()
            })
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

        let one_predicate_rule = CommandAuthorizationRule::ArgumentAuthPredicate {
            argument_name: ArgumentName::new(Identifier::new("filter").unwrap()),
            predicate: predicate.clone(),
            condition: Some(condition_id),
        };

        // one filter is set
        assert_eq!(
            evaluate_command_authorization_rules(
                &vec![
                    allow_command_access_rule.clone(),
                    one_predicate_rule.clone()
                ],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            Some(CommandPermission {
                argument_presets: BTreeMap::from_iter(vec![(
                    &ArgumentName::new(Identifier::new("filter").unwrap()),
                    ArgumentPolicy::BooleanExpression {
                        predicates: vec![&predicate]
                    }
                )])
            })
        );

        // two filters are set
        assert_eq!(
            evaluate_command_authorization_rules(
                &vec![
                    allow_command_access_rule,
                    one_predicate_rule.clone(),
                    one_predicate_rule
                ],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            Some(CommandPermission {
                argument_presets: BTreeMap::from_iter(vec![(
                    &ArgumentName::new(Identifier::new("filter").unwrap()),
                    ArgumentPolicy::BooleanExpression {
                        predicates: vec![&predicate, &predicate]
                    }
                )])
            })
        );
    }
}
