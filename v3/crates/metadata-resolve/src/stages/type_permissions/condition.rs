use open_dds::authorization::Comparison;

use crate::types::condition::{BinaryOperation, Condition};
use crate::types::permission::resolve_value_expression;

use crate::UnaryOperation;

pub fn resolve_condition(
    condition: &open_dds::authorization::Condition,
    flags: &open_dds::flags::OpenDdFlags,
) -> Condition {
    match condition {
        open_dds::authorization::Condition::And(conditions) => Condition::All(
            conditions
                .iter()
                .map(|c| resolve_condition(c, flags))
                .collect(),
        ),
        open_dds::authorization::Condition::Or(conditions) => Condition::Any(
            conditions
                .iter()
                .map(|c| resolve_condition(c, flags))
                .collect(),
        ),
        open_dds::authorization::Condition::Not(condition) => {
            Condition::Not(Box::new(resolve_condition(condition.as_ref(), flags)))
        }
        open_dds::authorization::Condition::Equal(Comparison { left, right }) => {
            Condition::BinaryOperation {
                op: BinaryOperation::Equals,
                left: resolve_value_expression(flags, left.clone()),
                right: resolve_value_expression(flags, right.clone()),
            }
        }
        open_dds::authorization::Condition::Contains(Comparison { left, right }) => {
            Condition::BinaryOperation {
                op: BinaryOperation::Contains,
                left: resolve_value_expression(flags, left.clone()),
                right: resolve_value_expression(flags, right.clone()),
            }
        }
        open_dds::authorization::Condition::GreaterThan(Comparison { left, right }) => {
            Condition::BinaryOperation {
                op: BinaryOperation::GreaterThan,
                left: resolve_value_expression(flags, left.clone()),
                right: resolve_value_expression(flags, right.clone()),
            }
        }
        open_dds::authorization::Condition::LessThan(Comparison { left, right }) => {
            Condition::BinaryOperation {
                op: BinaryOperation::LessThan,
                left: resolve_value_expression(flags, left.clone()),
                right: resolve_value_expression(flags, right.clone()),
            }
        }
        open_dds::authorization::Condition::GreaterThanOrEqual(Comparison { left, right }) => {
            Condition::BinaryOperation {
                op: BinaryOperation::GreaterThanOrEqual,
                left: resolve_value_expression(flags, left.clone()),
                right: resolve_value_expression(flags, right.clone()),
            }
        }
        open_dds::authorization::Condition::LessThanOrEqual(Comparison { left, right }) => {
            Condition::BinaryOperation {
                op: BinaryOperation::LessThanOrEqual,
                left: resolve_value_expression(flags, left.clone()),
                right: resolve_value_expression(flags, right.clone()),
            }
        }
        open_dds::authorization::Condition::IsNull(value) => Condition::UnaryOperation {
            op: UnaryOperation::IsNull,
            value: resolve_value_expression(flags, value.clone()),
        },
    }
}
