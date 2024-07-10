//! Functions for typechecking JSON literals against expected types
use crate::types::subgraph;
use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
/// Errors that can occur when typechecking a value
pub enum TypecheckError {
    #[error("Expected a value of type {expected:} but got value {actual:}")]
    ScalarTypeMismatch {
        expected: open_dds::types::InbuiltType,
        actual: serde_json::Value,
    },
    #[error("Error in array item: {inner_error:}")]
    ArrayItemMismatch { inner_error: Box<TypecheckError> },
    #[error("Expected an array but instead got value {value:}")]
    NonArrayValue { value: serde_json::Value },
    #[error("Expected a non-null value but received null")]
    NullInNonNullableColumn,
}

/// These are run at schema resolve time, so that we can warn the user against
/// using the wrong types in their engine metadata.
/// If the values are passed in a session variable then there is nothing we can do at this point
/// and we must rely on run time casts
pub fn typecheck_value_expression(
    ty: &subgraph::QualifiedTypeReference,
    value_expression: &open_dds::permissions::ValueExpression,
) -> Result<(), TypecheckError> {
    match &value_expression {
        open_dds::permissions::ValueExpression::SessionVariable(_) => Ok(()),
        open_dds::permissions::ValueExpression::Literal(json_value) => {
            typecheck_qualified_type_reference(ty, json_value)
        }
    }
}

/// These are run at schema resolve time, so that we can warn the user against
/// using the wrong types in their engine metadata.
/// If the values are passed in a session variable then there is nothing we can do at this point
/// and we must rely on run time casts
pub fn typecheck_value_expression_or_predicate(
    ty: &subgraph::QualifiedTypeReference,
    value_expression: &open_dds::permissions::ValueExpressionOrPredicate,
) -> Result<(), TypecheckError> {
    match &value_expression {
        open_dds::permissions::ValueExpressionOrPredicate::SessionVariable(_)
        | open_dds::permissions::ValueExpressionOrPredicate::BooleanExpression(_) => Ok(()),
        open_dds::permissions::ValueExpressionOrPredicate::Literal(json_value) => {
            typecheck_qualified_type_reference(ty, json_value)
        }
    }
}

/// check whether a serde_json::Value matches our expected type
/// currently only works for primitive types (Int, String, etc)
/// and arrays of those types
pub fn typecheck_qualified_type_reference(
    ty: &subgraph::QualifiedTypeReference,
    value: &serde_json::Value,
) -> Result<(), TypecheckError> {
    match (&ty.underlying_type, value) {
        // check null values are allowed
        (_, serde_json::Value::Null) => {
            if ty.nullable {
                Ok(())
            } else {
                Err(TypecheckError::NullInNonNullableColumn)
            }
        }
        // check basic inbuilt types
        (subgraph::QualifiedBaseType::Named(subgraph::QualifiedTypeName::Inbuilt(inbuilt)), _) => {
            typecheck_inbuilt_type(inbuilt, value)
        }
        // check each item in an array
        (subgraph::QualifiedBaseType::List(inner_type), serde_json::Value::Array(array_values)) => {
            array_values.iter().try_for_each(|array_value| {
                typecheck_qualified_type_reference(inner_type, array_value).map_err(|inner_error| {
                    TypecheckError::ArrayItemMismatch {
                        inner_error: Box::new(inner_error),
                    }
                })
            })
        }
        // array expected, non-array value
        (subgraph::QualifiedBaseType::List(_), value) => Err(TypecheckError::NonArrayValue {
            value: value.clone(),
        }),

        // we don't current check custom named types, so we just assume the values are OK
        (subgraph::QualifiedBaseType::Named(subgraph::QualifiedTypeName::Custom(_)), _) => Ok(()),
    }
}

/// check a JSON value matches an expected inbuilt primitive type
fn typecheck_inbuilt_type(
    inbuilt: &open_dds::types::InbuiltType,
    value: &serde_json::Value,
) -> Result<(), TypecheckError> {
    match (inbuilt, value) {
        (
            open_dds::types::InbuiltType::Int | open_dds::types::InbuiltType::Float,
            serde_json::Value::Number(_),
        )
        | (
            open_dds::types::InbuiltType::String | open_dds::types::InbuiltType::ID,
            serde_json::Value::String(_),
        )
        | (open_dds::types::InbuiltType::Boolean, serde_json::Value::Bool(_)) => Ok(()),
        _ => Err(TypecheckError::ScalarTypeMismatch {
            expected: inbuilt.clone(),
            actual: value.clone(),
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::{subgraph, typecheck_qualified_type_reference, TypecheckError};
    use serde_json::json;

    fn int_type(nullable: bool) -> subgraph::QualifiedTypeReference {
        subgraph::QualifiedTypeReference {
            nullable,
            underlying_type: subgraph::QualifiedBaseType::Named(
                subgraph::QualifiedTypeName::Inbuilt(open_dds::types::InbuiltType::Int),
            ),
        }
    }

    fn string_type() -> subgraph::QualifiedTypeReference {
        subgraph::QualifiedTypeReference {
            nullable: false,
            underlying_type: subgraph::QualifiedBaseType::Named(
                subgraph::QualifiedTypeName::Inbuilt(open_dds::types::InbuiltType::String),
            ),
        }
    }

    fn boolean_type() -> subgraph::QualifiedTypeReference {
        subgraph::QualifiedTypeReference {
            nullable: false,
            underlying_type: subgraph::QualifiedBaseType::Named(
                subgraph::QualifiedTypeName::Inbuilt(open_dds::types::InbuiltType::Boolean),
            ),
        }
    }

    fn float_type() -> subgraph::QualifiedTypeReference {
        subgraph::QualifiedTypeReference {
            nullable: false,
            underlying_type: subgraph::QualifiedBaseType::Named(
                subgraph::QualifiedTypeName::Inbuilt(open_dds::types::InbuiltType::Float),
            ),
        }
    }

    fn id_type() -> subgraph::QualifiedTypeReference {
        subgraph::QualifiedTypeReference {
            nullable: false,
            underlying_type: subgraph::QualifiedBaseType::Named(
                subgraph::QualifiedTypeName::Inbuilt(open_dds::types::InbuiltType::ID),
            ),
        }
    }

    fn array_of(item: subgraph::QualifiedTypeReference) -> subgraph::QualifiedTypeReference {
        subgraph::QualifiedTypeReference {
            nullable: false,
            underlying_type: subgraph::QualifiedBaseType::List(Box::new(item)),
        }
    }

    #[test]
    fn test_typecheck_int() {
        let ty = int_type(false);
        // `Int` accepts any JSON number
        let value = json!(1);

        assert_eq!(typecheck_qualified_type_reference(&ty, &value), Ok(()));
    }

    #[test]
    fn test_typecheck_float() {
        let ty = float_type();
        // `Float` accepts any JSON number
        let value = json!(123.123);

        assert_eq!(typecheck_qualified_type_reference(&ty, &value), Ok(()));
    }

    #[test]
    fn test_typecheck_string() {
        let ty = string_type();
        let value = json!("dog");

        assert_eq!(typecheck_qualified_type_reference(&ty, &value), Ok(()));
    }

    #[test]
    fn test_typecheck_boolean() {
        let ty = boolean_type();
        let value = json!(true);

        assert_eq!(typecheck_qualified_type_reference(&ty, &value), Ok(()));
    }

    #[test]
    fn test_typecheck_id() {
        let ty = id_type();
        let value = json!("12312312sdwfdsff123123");

        assert_eq!(typecheck_qualified_type_reference(&ty, &value), Ok(()));
    }

    #[test]
    fn test_typecheck_array_of_boolean() {
        let ty = array_of(boolean_type());
        let value = json!([true, false]);

        assert_eq!(typecheck_qualified_type_reference(&ty, &value), Ok(()));
    }

    #[test]
    fn test_typecheck_nested_array_of_boolean() {
        let ty = array_of(array_of(boolean_type()));
        let value = json!([[true, false]]);

        assert_eq!(typecheck_qualified_type_reference(&ty, &value), Ok(()));
    }

    #[test]
    fn test_typecheck_does_not_accept_mixture_of_int_and_boolean_in_array_of_boolean() {
        let ty = array_of(boolean_type());
        let value = json!([true, 123]);

        assert_eq!(
            typecheck_qualified_type_reference(&ty, &value),
            Err(TypecheckError::ArrayItemMismatch {
                inner_error: Box::new(TypecheckError::ScalarTypeMismatch {
                    expected: open_dds::types::InbuiltType::Boolean,
                    actual: serde_json::Value::Number(123.into())
                })
            })
        );
    }

    #[test]
    fn test_typecheck_does_not_accept_single_boolean_in_array_of_boolean() {
        let ty = array_of(boolean_type());
        let value = json!(true);

        assert_eq!(
            typecheck_qualified_type_reference(&ty, &value),
            Err(TypecheckError::NonArrayValue { value })
        );
    }

    #[test]
    fn test_typecheck_int_does_not_accept_null_value() {
        let ty = int_type(false);
        let value = json!(null);

        assert_eq!(
            typecheck_qualified_type_reference(&ty, &value),
            Err(TypecheckError::NullInNonNullableColumn)
        );
    }

    #[test]
    fn test_typecheck_nullable_int_accepts_null_value() {
        let ty = int_type(true);
        let value = json!(null);

        assert_eq!(typecheck_qualified_type_reference(&ty, &value), Ok(()));
    }

    #[test]
    fn test_typecheck_int_does_not_accept_string_value() {
        let ty = int_type(false);
        let value = json!("dog");

        assert_eq!(
            typecheck_qualified_type_reference(&ty, &value),
            Err(TypecheckError::ScalarTypeMismatch {
                expected: open_dds::types::InbuiltType::Int,
                actual: value.clone()
            })
        );
    }
}
