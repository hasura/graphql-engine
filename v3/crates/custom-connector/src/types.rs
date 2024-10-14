use ndc_models;
use std::collections::BTreeMap;

pub mod actor;
pub mod genre;
pub mod institution;
pub mod location;
pub mod login;
pub mod movie;
pub mod name_query;
pub mod staff_member;

pub(crate) fn scalar_types() -> BTreeMap<ndc_models::ScalarTypeName, ndc_models::ScalarType> {
    BTreeMap::from_iter([
        (
            "String".into(),
            ndc_models::ScalarType {
                representation: Some(ndc_models::TypeRepresentation::String),
                aggregate_functions: BTreeMap::from_iter([
                    (
                        "max".into(),
                        ndc_models::AggregateFunctionDefinition {
                            result_type: ndc_models::Type::Nullable {
                                underlying_type: Box::new(ndc_models::Type::Named {
                                    name: "String".into(),
                                }),
                            },
                        },
                    ),
                    (
                        "min".into(),
                        ndc_models::AggregateFunctionDefinition {
                            result_type: ndc_models::Type::Nullable {
                                underlying_type: Box::new(ndc_models::Type::Named {
                                    name: "String".into(),
                                }),
                            },
                        },
                    ),
                ]),
                comparison_operators: BTreeMap::from_iter([
                    (
                        "like".into(),
                        ndc_models::ComparisonOperatorDefinition::Custom {
                            argument_type: ndc_models::Type::Named {
                                name: "String".into(),
                            },
                        },
                    ),
                    (
                        "_eq".into(),
                        ndc_models::ComparisonOperatorDefinition::Equal,
                    ),
                ]),
            },
        ),
        (
            "Int".into(),
            ndc_models::ScalarType {
                representation: Some(ndc_models::TypeRepresentation::Int32),
                aggregate_functions: BTreeMap::from_iter([
                    (
                        "max".into(),
                        ndc_models::AggregateFunctionDefinition {
                            result_type: ndc_models::Type::Nullable {
                                underlying_type: Box::new(ndc_models::Type::Named {
                                    name: "Int".into(),
                                }),
                            },
                        },
                    ),
                    (
                        "min".into(),
                        ndc_models::AggregateFunctionDefinition {
                            result_type: ndc_models::Type::Nullable {
                                underlying_type: Box::new(ndc_models::Type::Named {
                                    name: "Int".into(),
                                }),
                            },
                        },
                    ),
                ]),
                comparison_operators: BTreeMap::from_iter([(
                    "_eq".into(),
                    ndc_models::ComparisonOperatorDefinition::Equal,
                )]),
            },
        ),
        (
            "Bool".into(),
            ndc_models::ScalarType {
                representation: Some(ndc_models::TypeRepresentation::Boolean),
                aggregate_functions: BTreeMap::new(),
                comparison_operators: BTreeMap::from_iter([(
                    "eq".into(),
                    ndc_models::ComparisonOperatorDefinition::Custom {
                        argument_type: ndc_models::Type::Named {
                            name: "Bool".into(),
                        },
                    },
                )]),
            },
        ),
        (
            "Actor_Name".into(),
            ndc_models::ScalarType {
                representation: Some(ndc_models::TypeRepresentation::String),
                aggregate_functions: BTreeMap::new(),
                comparison_operators: BTreeMap::new(),
            },
        ),
        (
            "HeaderMap".into(),
            ndc_models::ScalarType {
                representation: Some(ndc_models::TypeRepresentation::JSON),
                aggregate_functions: BTreeMap::new(),
                comparison_operators: BTreeMap::new(),
            },
        ),
    ])
}

pub(crate) fn object_types() -> BTreeMap<ndc_models::ObjectTypeName, ndc_models::ObjectType> {
    BTreeMap::from_iter([
        ("actor".into(), actor::definition()),
        ("movie".into(), movie::definition()),
        ("genre".into(), genre::definition()),
        ("name_query".into(), name_query::definition()),
        ("institution".into(), institution::definition()),
        ("location".into(), location::definition()),
        ("staff_member".into(), staff_member::definition()),
        ("login_response".into(), login::definition_login_response()),
        (
            "session_response".into(),
            login::definition_session_response(),
        ),
        ("session_info".into(), login::definition_session_info()),
    ])
}
