//! Functions for typechecking JSON literals against expected types
use std::collections::{BTreeMap, BTreeSet};

use crate::stages::object_types;
use crate::types::error::ShouldBeAnError;
use crate::{Qualified, QualifiedBaseType, QualifiedTypeName, QualifiedTypeReference};
use open_dds::flags::Flag;
use open_dds::types::{CustomTypeName, FieldName};
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

#[derive(Error, Debug, PartialEq)]
/// Issues that can occur when typechecking a value against an object type
pub enum TypecheckIssue {
    #[error("Expected an object value of type {expected:} but got value {actual:}")]
    ObjectTypeMismatch {
        expected: Qualified<CustomTypeName>,
        actual: serde_json::Value,
    },

    #[error("Typecheck failed for field {field_name:} in object type {object_type:}: {error:}")]
    ObjectTypeField {
        field_name: FieldName,
        object_type: Qualified<CustomTypeName>,
        error: TypecheckError,
    },

    #[error(
        "Found a literal value but the argument is a boolean expression type '{boolean_expression_type_name}'"
    )]
    LiteralValueUsedForBooleanExpression {
        boolean_expression_type_name: Qualified<CustomTypeName>,
    },
}

impl ShouldBeAnError for TypecheckIssue {
    fn should_be_an_error(&self, flags: &open_dds::flags::OpenDdFlags) -> bool {
        match self {
            TypecheckIssue::ObjectTypeField { .. } | TypecheckIssue::ObjectTypeMismatch { .. } => {
                flags.contains(Flag::TypecheckObjectTypeValuesInPresets)
            }
            TypecheckIssue::LiteralValueUsedForBooleanExpression { .. } => {
                flags.contains(Flag::DisallowLiteralsAsBooleanExpressionArguments)
            }
        }
    }
}

/// These are run at metadata resolve time, so that we can warn the user against
/// using the wrong types in their engine metadata.
/// If the values are passed in a session variable then there is nothing we can do at this point
/// and we must rely on run time casts
pub fn typecheck_value_expression(
    object_types: &BTreeMap<&Qualified<CustomTypeName>, &object_types::ObjectTypeRepresentation>,
    boolean_expression_type_names: &BTreeSet<&Qualified<CustomTypeName>>,
    ty: &QualifiedTypeReference,
    value_expression: &open_dds::permissions::ValueExpression,
) -> Result<Vec<TypecheckIssue>, TypecheckError> {
    let mut issues = Vec::new();
    match &value_expression {
        open_dds::permissions::ValueExpression::SessionVariable(_) => {}
        open_dds::permissions::ValueExpression::Literal(json_value) => {
            typecheck_qualified_type_reference(
                object_types,
                boolean_expression_type_names,
                ty,
                json_value,
                &mut issues,
            )?;
        }
    }
    Ok(issues)
}

/// check whether a serde_json::Value matches our expected type
/// currently only works for primitive types (Int, String, etc)
/// and arrays of those types
pub fn typecheck_qualified_type_reference(
    object_types: &BTreeMap<&Qualified<CustomTypeName>, &object_types::ObjectTypeRepresentation>,
    boolean_expression_type_names: &BTreeSet<&Qualified<CustomTypeName>>,
    ty: &QualifiedTypeReference,
    value: &serde_json::Value,
    issues: &mut Vec<TypecheckIssue>,
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
        (QualifiedBaseType::Named(QualifiedTypeName::Inbuilt(inbuilt)), _) => {
            typecheck_inbuilt_type(inbuilt, value)
        }
        // check each item in an array
        (QualifiedBaseType::List(inner_type), serde_json::Value::Array(array_values)) => {
            array_values.iter().try_for_each(|array_value| {
                typecheck_qualified_type_reference(
                    object_types,
                    boolean_expression_type_names,
                    inner_type,
                    array_value,
                    issues,
                )
                .map_err(|inner_error| TypecheckError::ArrayItemMismatch {
                    inner_error: Box::new(inner_error),
                })
            })
        }
        // array expected, non-array value
        (QualifiedBaseType::List(_), value) => Err(TypecheckError::NonArrayValue {
            value: value.clone(),
        }),

        // check custom types
        (QualifiedBaseType::Named(QualifiedTypeName::Custom(custom_type)), _) => {
            typecheck_custom_type(
                object_types,
                boolean_expression_type_names,
                custom_type,
                value,
                issues,
            );
            Ok(())
        }
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
            expected: *inbuilt,
            actual: value.clone(),
        }),
    }
}

/// check a JSON value matches an expected custom type.
fn typecheck_custom_type(
    object_types: &BTreeMap<&Qualified<CustomTypeName>, &object_types::ObjectTypeRepresentation>,
    boolean_expression_type_names: &BTreeSet<&Qualified<CustomTypeName>>,
    custom_type: &Qualified<CustomTypeName>,
    value: &serde_json::Value,
    issues: &mut Vec<TypecheckIssue>,
) {
    // If the type is an object type, typecheck the value against the object type fields
    // If the type is a scalar type, ignore.
    if let Some(object_type) = object_types.get(custom_type) {
        if let serde_json::Value::Object(object_value) = value {
            for (field_name, field_definition) in &object_type.fields {
                let field_value = object_value
                    .get(field_name.as_str())
                    .unwrap_or_else(|| &serde_json::Value::Null);
                if let Err(e) = typecheck_qualified_type_reference(
                    object_types,
                    boolean_expression_type_names,
                    &field_definition.field_type,
                    field_value,
                    issues,
                ) {
                    issues.push(TypecheckIssue::ObjectTypeField {
                        field_name: field_name.clone(),
                        object_type: custom_type.clone(),
                        error: e,
                    });
                }
            }
        } else {
            // Raise an issue if the value is not an object
            issues.push(TypecheckIssue::ObjectTypeMismatch {
                expected: custom_type.clone(),
                actual: value.clone(),
            });
        }
    } else if boolean_expression_type_names.contains(custom_type) {
        // boolean expressions shouldn't be provided as literals
        issues.push(TypecheckIssue::LiteralValueUsedForBooleanExpression {
            boolean_expression_type_name: custom_type.clone(),
        });
    }
}

#[cfg(test)]
mod tests {
    use std::collections::{BTreeMap, BTreeSet};

    use super::{TypecheckError, typecheck_qualified_type_reference};
    use crate::{
        Qualified, QualifiedBaseType, QualifiedTypeName, QualifiedTypeReference,
        stages::object_types,
    };
    use indexmap::IndexMap;
    use open_dds::{
        identifier::{Identifier, SubgraphName},
        types::{CustomTypeName, FieldName},
    };
    use serde_json::json;

    fn empty_object_types<'a>()
    -> BTreeMap<&'a Qualified<CustomTypeName>, &'a object_types::ObjectTypeRepresentation> {
        BTreeMap::new()
    }

    fn int_type(nullable: bool) -> QualifiedTypeReference {
        QualifiedTypeReference {
            nullable,
            underlying_type: QualifiedBaseType::Named(QualifiedTypeName::Inbuilt(
                open_dds::types::InbuiltType::Int,
            )),
        }
    }

    fn string_type() -> QualifiedTypeReference {
        QualifiedTypeReference {
            nullable: false,
            underlying_type: QualifiedBaseType::Named(QualifiedTypeName::Inbuilt(
                open_dds::types::InbuiltType::String,
            )),
        }
    }

    fn boolean_type() -> QualifiedTypeReference {
        QualifiedTypeReference {
            nullable: false,
            underlying_type: QualifiedBaseType::Named(QualifiedTypeName::Inbuilt(
                open_dds::types::InbuiltType::Boolean,
            )),
        }
    }

    fn float_type() -> QualifiedTypeReference {
        QualifiedTypeReference {
            nullable: false,
            underlying_type: QualifiedBaseType::Named(QualifiedTypeName::Inbuilt(
                open_dds::types::InbuiltType::Float,
            )),
        }
    }

    fn id_type() -> QualifiedTypeReference {
        QualifiedTypeReference {
            nullable: false,
            underlying_type: QualifiedBaseType::Named(QualifiedTypeName::Inbuilt(
                open_dds::types::InbuiltType::ID,
            )),
        }
    }

    fn array_of(item: QualifiedTypeReference) -> QualifiedTypeReference {
        QualifiedTypeReference {
            nullable: false,
            underlying_type: QualifiedBaseType::List(Box::new(item)),
        }
    }

    #[test]
    fn test_typecheck_int() {
        let ty = int_type(false);
        // `Int` accepts any JSON number
        let value = json!(1);

        assert_eq!(
            typecheck_qualified_type_reference(
                &empty_object_types(),
                &BTreeSet::new(),
                &ty,
                &value,
                &mut vec![]
            ),
            Ok(())
        );
    }

    #[test]
    fn test_typecheck_float() {
        let ty = float_type();
        // `Float` accepts any JSON number
        let value = json!(123.123);

        assert_eq!(
            typecheck_qualified_type_reference(
                &empty_object_types(),
                &BTreeSet::new(),
                &ty,
                &value,
                &mut vec![]
            ),
            Ok(())
        );
    }

    #[test]
    fn test_typecheck_string() {
        let ty = string_type();
        let value = json!("dog");

        assert_eq!(
            typecheck_qualified_type_reference(
                &empty_object_types(),
                &BTreeSet::new(),
                &ty,
                &value,
                &mut vec![]
            ),
            Ok(())
        );
    }

    #[test]
    fn test_typecheck_boolean() {
        let ty = boolean_type();
        let value = json!(true);

        assert_eq!(
            typecheck_qualified_type_reference(
                &empty_object_types(),
                &BTreeSet::new(),
                &ty,
                &value,
                &mut vec![]
            ),
            Ok(())
        );
    }

    #[test]
    fn test_typecheck_id() {
        let ty = id_type();
        let value = json!("12312312sdwfdsff123123");

        assert_eq!(
            typecheck_qualified_type_reference(
                &empty_object_types(),
                &BTreeSet::new(),
                &ty,
                &value,
                &mut vec![]
            ),
            Ok(())
        );
    }

    #[test]
    fn test_typecheck_array_of_boolean() {
        let ty = array_of(boolean_type());
        let value = json!([true, false]);

        assert_eq!(
            typecheck_qualified_type_reference(
                &empty_object_types(),
                &BTreeSet::new(),
                &ty,
                &value,
                &mut vec![]
            ),
            Ok(())
        );
    }

    #[test]
    fn test_typecheck_nested_array_of_boolean() {
        let ty = array_of(array_of(boolean_type()));
        let value = json!([[true, false]]);

        assert_eq!(
            typecheck_qualified_type_reference(
                &empty_object_types(),
                &BTreeSet::new(),
                &ty,
                &value,
                &mut vec![]
            ),
            Ok(())
        );
    }

    #[test]
    fn test_typecheck_does_not_accept_mixture_of_int_and_boolean_in_array_of_boolean() {
        let ty = array_of(boolean_type());
        let value = json!([true, 123]);

        assert_eq!(
            typecheck_qualified_type_reference(
                &empty_object_types(),
                &BTreeSet::new(),
                &ty,
                &value,
                &mut vec![]
            ),
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
            typecheck_qualified_type_reference(
                &empty_object_types(),
                &BTreeSet::new(),
                &ty,
                &value,
                &mut vec![]
            ),
            Err(TypecheckError::NonArrayValue { value })
        );
    }

    #[test]
    fn test_typecheck_int_does_not_accept_null_value() {
        let ty = int_type(false);
        let value = json!(null);

        assert_eq!(
            typecheck_qualified_type_reference(
                &empty_object_types(),
                &BTreeSet::new(),
                &ty,
                &value,
                &mut vec![]
            ),
            Err(TypecheckError::NullInNonNullableColumn)
        );
    }

    #[test]
    fn test_typecheck_nullable_int_accepts_null_value() {
        let ty = int_type(true);
        let value = json!(null);

        assert_eq!(
            typecheck_qualified_type_reference(
                &empty_object_types(),
                &BTreeSet::new(),
                &ty,
                &value,
                &mut vec![]
            ),
            Ok(())
        );
    }

    #[test]
    fn test_typecheck_int_does_not_accept_string_value() {
        let ty = int_type(false);
        let value = json!("dog");

        assert_eq!(
            typecheck_qualified_type_reference(
                &empty_object_types(),
                &BTreeSet::new(),
                &ty,
                &value,
                &mut vec![]
            ),
            Err(TypecheckError::ScalarTypeMismatch {
                expected: open_dds::types::InbuiltType::Int,
                actual: value.clone()
            })
        );
    }

    fn create_test_object_type() -> (
        Qualified<CustomTypeName>,
        object_types::ObjectTypeRepresentation,
    ) {
        let custom_type = Qualified {
            subgraph: SubgraphName::new_inline_static("default"),
            name: CustomTypeName(Identifier::new("Person").unwrap()),
        };

        let mut fields = IndexMap::new();
        fields.insert(
            FieldName::new(Identifier::new("name").unwrap()),
            object_types::FieldDefinition {
                field_type: string_type(),
                description: None,
                deprecated: None,
                field_arguments: IndexMap::new(),
            },
        );
        fields.insert(
            FieldName::new(Identifier::new("age").unwrap()),
            object_types::FieldDefinition {
                field_type: int_type(false),
                description: None,
                deprecated: None,
                field_arguments: IndexMap::new(),
            },
        );

        let object_type = object_types::ObjectTypeRepresentation {
            fields,
            global_id_fields: vec![],
            apollo_federation_config: None,
            graphql_output_type_name: None,
            graphql_input_type_name: None,
            description: None,
        };

        (custom_type, object_type)
    }

    #[test]
    fn test_typecheck_custom_object_type() {
        let (custom_type, object_type) = create_test_object_type();

        // Create object_types map with our test type
        let mut object_types = BTreeMap::new();
        object_types.insert(&custom_type, &object_type);

        // Create the qualified type reference for our custom type
        let ty = QualifiedTypeReference {
            nullable: false,
            underlying_type: QualifiedBaseType::Named(QualifiedTypeName::Custom(
                custom_type.clone(),
            )),
        };

        // Test valid object
        let valid_value = json!({
            "name": "John Doe",
            "age": 30
        });

        assert_eq!(
            typecheck_qualified_type_reference(
                &object_types,
                &BTreeSet::new(),
                &ty,
                &valid_value,
                &mut vec![]
            ),
            Ok(())
        );

        // Test invalid object (wrong type for age)
        let invalid_value = json!({
            "name": "John Doe",
            "age": "thirty"
        });
        let mut issues = vec![];
        typecheck_qualified_type_reference(
            &object_types,
            &BTreeSet::new(),
            &ty,
            &invalid_value,
            &mut issues,
        )
        .unwrap();
        assert_eq!(
            issues.into_iter().next().unwrap().to_string(),
            format!(
                "Typecheck failed for field age in object type Person (in subgraph default): Expected a value of type Int but got value \"thirty\""
            )
        );

        // Test invalid object (missing required field)
        let missing_field_value = json!({
            "name": "John Doe"
        });

        let mut issues = vec![];
        typecheck_qualified_type_reference(
            &object_types,
            &BTreeSet::new(),
            &ty,
            &missing_field_value,
            &mut issues,
        )
        .unwrap();
        assert_eq!(
            issues.into_iter().next().unwrap().to_string(),
            format!(
                "Typecheck failed for field age in object type Person (in subgraph default): Expected a non-null value but received null"
            )
        );

        // Test non-object value
        let non_object_value = json!("not an object");

        let mut issues = vec![];
        typecheck_qualified_type_reference(
            &object_types,
            &BTreeSet::new(),
            &ty,
            &non_object_value,
            &mut issues,
        )
        .unwrap();
        assert_eq!(
            issues.into_iter().next().unwrap().to_string(),
            format!(
                "Expected an object value of type Person (in subgraph default) but got value \"not an object\""
            )
        );
    }
}
