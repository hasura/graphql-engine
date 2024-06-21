use super::types::{IncludeIsNull, IncludeLogicalOperators};
use crate::types::error::{BooleanExpressionError, Error};
use crate::Qualified;
use open_dds::{
    boolean_expression::{BooleanExpressionIsNull, BooleanExpressionLogicalOperators},
    types::CustomTypeName,
};
use std::collections::BTreeMap;

pub(crate) fn lookup_raw_boolean_expression<'a>(
    parent_boolean_expression_name: &Qualified<CustomTypeName>,
    boolean_expression_name: &Qualified<CustomTypeName>,
    raw_boolean_expression_types: &'a BTreeMap<
        Qualified<CustomTypeName>,
        (
            &String,
            &open_dds::boolean_expression::BooleanExpressionTypeV1,
        ),
    >,
) -> Result<
    &'a (
        &'a String,
        &'a open_dds::boolean_expression::BooleanExpressionTypeV1,
    ),
    Error,
> {
    raw_boolean_expression_types
        .get(boolean_expression_name)
        .ok_or_else(|| {
            BooleanExpressionError::BooleanExpressionCouldNotBeFound {
                parent_boolean_expression: parent_boolean_expression_name.clone(),
                child_boolean_expression: boolean_expression_name.clone(),
            }
            .into()
        })
}

pub(crate) fn resolve_is_null(is_null: &BooleanExpressionIsNull) -> IncludeIsNull {
    if is_null.enable {
        IncludeIsNull::Yes
    } else {
        IncludeIsNull::No
    }
}

pub(crate) fn resolve_logical_operators(
    logical_operators: &BooleanExpressionLogicalOperators,
) -> IncludeLogicalOperators {
    if logical_operators.enable {
        IncludeLogicalOperators::Yes
    } else {
        IncludeLogicalOperators::No
    }
}
