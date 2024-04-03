use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum ValueExpression {
    Literal(serde_json::Value),
    SessionVariable(open_dds::session_variables::SessionVariable),
}

/// resolve a ValueExpression. This is currently a no-op but will be more involved
/// once we add BooleanExpression
pub(crate) fn resolve_value_expression(
    value_expression: open_dds::permissions::ValueExpression,
) -> ValueExpression {
    match value_expression {
        open_dds::permissions::ValueExpression::Literal(literal) => {
            ValueExpression::Literal(literal)
        }
        open_dds::permissions::ValueExpression::SessionVariable(session_variable) => {
            ValueExpression::SessionVariable(session_variable)
        }
    }
}
