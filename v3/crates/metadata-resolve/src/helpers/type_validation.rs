use crate::stages::data_connector_scalar_types;
use crate::types::subgraph::{QualifiedBaseType, QualifiedTypeName, QualifiedTypeReference};
use ndc_models;

#[derive(Debug, thiserror::Error)]
#[error(
    "Type '{opendd_type:}' is not compatible with NDC type '{}' - {issue_message:}",
    show_ndc_type(ndc_type)
)]
pub struct TypeCompatibilityIssue {
    opendd_type: QualifiedTypeReference,
    ndc_type: ndc_models::Type,
    issue_message: String,
}

/// Validate OpenDd type compatibility with NDC type
pub fn validate_type_compatibility(
    data_connector_scalars: &data_connector_scalar_types::DataConnectorScalars,
    opendd_type: &QualifiedTypeReference,
    ndc_type: &ndc_models::Type,
) -> Option<TypeCompatibilityIssue> {
    validate_type_structure(
        data_connector_scalars,
        &opendd_type.underlying_type,
        ndc_type,
        opendd_type.nullable,
    )
    .map(|issue_message| TypeCompatibilityIssue {
        opendd_type: opendd_type.clone(),
        ndc_type: ndc_type.clone(),
        issue_message,
    })
}

/// Validate that the OpenDD type structure matches the NDC type structure
fn validate_type_structure(
    data_connector_scalars: &data_connector_scalar_types::DataConnectorScalars,
    opendd_type: &QualifiedBaseType,
    ndc_type: &ndc_models::Type,
    is_nullable: bool,
) -> Option<String> {
    match (ndc_type, is_nullable) {
        (ndc_models::Type::Nullable { underlying_type }, true) => {
            // This is valid. Now let's check the underlying type by assuming it's not nullable now.
            validate_type_structure(data_connector_scalars, opendd_type, underlying_type, false)
        }
        (_unexpected_nullable_type, true) => Some("Expected a nullable type".to_string()),
        (ndc_models::Type::Nullable { .. }, false) => {
            Some("Expected a non-nullable type".to_string())
        }
        (non_nullable_ndc_type, false) => {
            // Now we can check the type structure
            match (opendd_type, non_nullable_ndc_type) {
                (
                    QualifiedBaseType::Named(opendd_named_type),
                    ndc_models::Type::Named {
                        name: ndc_named_type,
                    },
                ) => {
                    let mut issue = None;
                    // Validate the leaf scalar types
                    // If the OpenDD type is an inbuilt type, check that the NDC type compatible to that inbuilt type
                    if let (
                        QualifiedTypeName::Inbuilt(inbuilt_type),
                        Some(data_connector_scalar_type),
                    ) = (
                        opendd_named_type,
                        data_connector_scalars
                            .by_ndc_type
                            .get(ndc_named_type.as_str()),
                    ) {
                        let ndc_scalar_representation =
                            &data_connector_scalar_type.scalar_type.representation;
                        if let Some(mapped_inbuilt_type) =
                            map_ndc_type_representation_to_inbuilt_type(ndc_scalar_representation)
                        {
                            if mapped_inbuilt_type != *inbuilt_type {
                                issue = Some(format!(
                                    "Inbuilt type '{inbuilt_type:}' is not compatible with the data connector's type representation '{ndc_scalar_representation:?}'"
                                ));
                            }
                        }
                    }
                    issue
                }
                (
                    QualifiedBaseType::List(list_opendd_type),
                    ndc_models::Type::Array { element_type },
                ) => {
                    // This is valid
                    // Now recurse to check the element type
                    validate_type_structure(
                        data_connector_scalars,
                        &list_opendd_type.underlying_type,
                        element_type,
                        list_opendd_type.nullable,
                    )
                }
                (
                    QualifiedBaseType::Named(QualifiedTypeName::Custom(_)), // Custom boolean expression type
                    ndc_models::Type::Predicate { .. },                     // Predicate type
                ) => {
                    // This is valid
                    None
                }
                (_, _) => {
                    // This is invalid
                    Some("Type mismatch".to_string())
                }
            }
        }
    }
}

// Map an NDC type representation to an OpenDD inbuilt type
fn map_ndc_type_representation_to_inbuilt_type(
    ndc_representation: &ndc_models::TypeRepresentation,
) -> Option<open_dds::types::InbuiltType> {
    match ndc_representation {
        ndc_models::TypeRepresentation::String
        | ndc_models::TypeRepresentation::UUID
        | ndc_models::TypeRepresentation::Date
        | ndc_models::TypeRepresentation::Timestamp
        | ndc_models::TypeRepresentation::TimestampTZ
        | ndc_models::TypeRepresentation::Bytes
        | ndc_models::TypeRepresentation::Enum { .. } => Some(open_dds::types::InbuiltType::String),

        ndc_models::TypeRepresentation::Int8
        | ndc_models::TypeRepresentation::Int16
        | ndc_models::TypeRepresentation::Int32
        | ndc_models::TypeRepresentation::Int64 => Some(open_dds::types::InbuiltType::Int),

        ndc_models::TypeRepresentation::Float32 | ndc_models::TypeRepresentation::Float64 => {
            Some(open_dds::types::InbuiltType::Float)
        }

        ndc_models::TypeRepresentation::Boolean => Some(open_dds::types::InbuiltType::Boolean),

        // For types that don't map directly to inbuilt types, return None
        // None indicates the representation can accept any valid JSON value
        // This includes high-precision numeric types that shouldn't be constrained to standard float/int
        ndc_models::TypeRepresentation::BigInteger
        | ndc_models::TypeRepresentation::BigDecimal
        | ndc_models::TypeRepresentation::Geography
        | ndc_models::TypeRepresentation::Geometry
        | ndc_models::TypeRepresentation::JSON => None,
    }
}

pub(crate) fn show_ndc_type(ndc_type: &ndc_models::Type) -> String {
    match ndc_type {
        // nullable type
        ndc_models::Type::Nullable { underlying_type } => {
            // remove trailing '!', if any, from underlying type
            show_ndc_type(underlying_type)
                .trim_end_matches('!')
                .to_string()
        }
        // rest all are non-nullable
        ndc_models::Type::Named { name } => format!("{name:}!"),
        ndc_models::Type::Array { element_type } => {
            format!("[{}]!", show_ndc_type(element_type))
        }
        ndc_models::Type::Predicate { object_type_name } => {
            format!("Predicate({object_type_name:})!")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::show_ndc_type;
    use ndc_models::Type;

    #[test]
    fn test_show_ndc_type_named() {
        // Test named type - non-nullable
        assert_eq!(show_ndc_type(&Type::Named { name: "Foo".into() }), "Foo!");
    }

    #[test]
    fn test_show_ndc_type_nullable() {
        // Test nullable type
        assert_eq!(
            show_ndc_type(&Type::Nullable {
                underlying_type: Box::new(Type::Named { name: "Foo".into() })
            }),
            "Foo",
        );
    }

    #[test]
    fn test_show_ndc_type_array() {
        // Test array type
        assert_eq!(
            show_ndc_type(&Type::Array {
                element_type: Box::new(Type::Named { name: "Foo".into() })
            }),
            "[Foo!]!"
        );
    }

    #[test]
    fn test_show_ndc_type_nullable_array() {
        // Test nullable array type
        assert_eq!(
            show_ndc_type(&Type::Nullable {
                underlying_type: Box::new(Type::Array {
                    element_type: Box::new(Type::Named { name: "Foo".into() })
                })
            }),
            "[Foo!]",
        );
    }

    #[test]
    fn test_show_ndc_type_nullable_array_nullable() {
        // Test nullable array type with nullable elements
        assert_eq!(
            show_ndc_type(&Type::Nullable {
                underlying_type: Box::new(Type::Nullable {
                    underlying_type: Box::new(Type::Array {
                        element_type: Box::new(Type::Nullable {
                            underlying_type: Box::new(Type::Named { name: "Foo".into() })
                        })
                    })
                })
            }),
            "[Foo]",
        );
    }

    #[test]
    fn test_show_ndc_type_predicate() {
        // Test predicate type
        assert_eq!(
            show_ndc_type(&Type::Predicate {
                object_type_name: "Foo".into()
            }),
            "Predicate(Foo)!",
        );
    }

    #[test]
    fn test_show_ndc_type_nullable_predicate() {
        // Test nullable predicate type
        assert_eq!(
            show_ndc_type(&Type::Nullable {
                underlying_type: Box::new(Type::Predicate {
                    object_type_name: "Foo".into()
                })
            }),
            "Predicate(Foo)",
        );
    }
}
