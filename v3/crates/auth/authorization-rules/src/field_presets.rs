use std::collections::BTreeMap;

use hasura_authn_core::SessionVariables;
use metadata_resolve::{Conditions, TypeInputAuthorizationRule, ValueExpression};
use open_dds::types::FieldName;

use crate::{
    ConditionCache,
    condition::{ConditionError, evaluate_optional_condition_hash},
};

#[derive(Debug, PartialEq, Eq)]
pub struct ObjectInputPolicy<'metadata> {
    pub field_presets: BTreeMap<&'metadata FieldName, &'metadata ValueExpression>,
}

pub fn evaluate_type_input_authorization_rules<'a>(
    rules: &'a Vec<TypeInputAuthorizationRule>,
    session_variables: &SessionVariables,
    conditions: &Conditions,
    condition_cache: &mut ConditionCache,
) -> Result<ObjectInputPolicy<'a>, ConditionError> {
    let mut field_presets = BTreeMap::new();

    for type_input_rule in rules {
        match type_input_rule {
            TypeInputAuthorizationRule::FieldPresetValue {
                field_name,
                value,
                condition,
            } => {
                if evaluate_optional_condition_hash(
                    condition.as_ref(),
                    session_variables,
                    conditions,
                    condition_cache,
                )? {
                    field_presets.insert(field_name, value);
                }
            }
        }
    }

    Ok(ObjectInputPolicy { field_presets })
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use super::*;
    use hasura_authn_core::{Identity, Role};
    use metadata_resolve::{BinaryOperation, Condition, ValueExpression};
    use open_dds::{identifier::Identifier, types::FieldName};

    #[test]
    fn test_evaluate_field_preset_rules() {
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

        // no presets set
        assert_eq!(
            evaluate_type_input_authorization_rules(
                &vec![],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            ObjectInputPolicy {
                field_presets: BTreeMap::new()
            }
        );

        let name_1_value_expression =
            ValueExpression::Literal(serde_json::Value::String("Mr Horse".to_string()));

        let name_argument_literal_rule = TypeInputAuthorizationRule::FieldPresetValue {
            field_name: FieldName::new(Identifier::new("name").unwrap()),
            value: name_1_value_expression.clone(),
            condition: Some(condition_id),
        };

        // name is set
        assert_eq!(
            evaluate_type_input_authorization_rules(
                &vec![name_argument_literal_rule.clone()],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            ObjectInputPolicy {
                field_presets: BTreeMap::from_iter(vec![(
                    &FieldName::new(Identifier::new("name").unwrap()),
                    &name_1_value_expression
                )])
            }
        );

        let name_2_value_expression =
            ValueExpression::Literal(serde_json::Value::String("Mr Horse 2".to_string()));

        let name_argument_literal_rule_2 = TypeInputAuthorizationRule::FieldPresetValue {
            field_name: FieldName::new(Identifier::new("name").unwrap()),
            value: name_2_value_expression.clone(),
            condition: Some(condition_id),
        };

        // name is set to second value
        assert_eq!(
            evaluate_type_input_authorization_rules(
                &vec![name_argument_literal_rule, name_argument_literal_rule_2],
                &session.variables,
                &conditions,
                &mut condition_cache
            )
            .unwrap(),
            ObjectInputPolicy {
                field_presets: BTreeMap::from_iter(vec![(
                    &FieldName::new(Identifier::new("name").unwrap()),
                    &name_2_value_expression
                )])
            }
        );
    }
}
