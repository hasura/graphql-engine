use crate::stages::model_permissions;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash)]
pub enum ValueExpression {
    Literal(serde_json::Value),
    SessionVariable(open_dds::session_variables::SessionVariableReference),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum ValueExpressionOrPredicate {
    Literal(serde_json::Value),
    SessionVariable(open_dds::session_variables::SessionVariableReference),
    BooleanExpression(Box<model_permissions::ModelPredicate>),
}

impl ValueExpressionOrPredicate {
    pub fn split_predicate(self) -> Result<ValueExpression, model_permissions::ModelPredicate> {
        match self {
            ValueExpressionOrPredicate::BooleanExpression(p) => Err(*p),
            ValueExpressionOrPredicate::Literal(value) => Ok(ValueExpression::Literal(value)),
            ValueExpressionOrPredicate::SessionVariable(session_variable) => {
                Ok(ValueExpression::SessionVariable(session_variable))
            }
        }
    }
}

pub fn resolve_value_expression(
    flags: &open_dds::flags::OpenDdFlags,
    value_expression_input: open_dds::permissions::ValueExpression,
) -> ValueExpression {
    match value_expression_input {
        open_dds::permissions::ValueExpression::SessionVariable(session_variable) => {
            ValueExpression::SessionVariable(hasura_authn_core::SessionVariableReference {
                name: session_variable,
                passed_as_json: flags.contains(open_dds::flags::Flag::JsonSessionVariables),
                disallow_unknown_fields: flags
                    .contains(open_dds::flags::Flag::DisallowUnknownValuesInArguments),
            })
        }
        open_dds::permissions::ValueExpression::Literal(json_value) => {
            ValueExpression::Literal(json_value)
        }
    }
}
