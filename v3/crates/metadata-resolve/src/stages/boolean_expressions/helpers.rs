use open_dds::{boolean_expression::BooleanExpressionLogicalOperators, types::CustomTypeName};

use crate::types::subgraph::Qualified;

use super::error::BooleanExpressionError;
use super::types::IncludeLogicalOperators;

pub(crate) fn lookup_raw_boolean_expression<'a>(
    parent_boolean_expression_name: &Qualified<CustomTypeName>,
    boolean_expression_name: &Qualified<CustomTypeName>,
    raw_boolean_expression_types: &'a super::object::RawBooleanExpressionTypes<'a>,
) -> Result<
    &'a (
        &'a open_dds::identifier::SubgraphName,
        &'a open_dds::boolean_expression::BooleanExpressionTypeV1,
    ),
    BooleanExpressionError,
> {
    raw_boolean_expression_types
        .get(boolean_expression_name)
        .ok_or_else(
            || BooleanExpressionError::BooleanExpressionCouldNotBeFound {
                parent_boolean_expression: parent_boolean_expression_name.clone(),
                child_boolean_expression: boolean_expression_name.clone(),
            },
        )
}

pub fn resolve_logical_operators(
    logical_operators: &BooleanExpressionLogicalOperators,
) -> IncludeLogicalOperators {
    if logical_operators.enable {
        IncludeLogicalOperators::Yes
    } else {
        IncludeLogicalOperators::No
    }
}
