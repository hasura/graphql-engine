use ndc_models;
use std::collections::BTreeMap;

pub mod actor;
pub mod city;
pub mod continent;
pub mod country;
pub mod evaluated_institution;
pub mod genre;
pub mod institution;
pub mod location;
pub mod location_pascalcase;
pub mod login;
pub mod movie;
pub mod name_query;
pub mod staff_member;
pub mod r#where;
pub mod where_int;
pub mod where_string;

pub(crate) fn scalar_types() -> BTreeMap<ndc_models::ScalarTypeName, ndc_models::ScalarType> {
    BTreeMap::from_iter([
        (
            "String".into(),
            ndc_models::ScalarType {
                representation: ndc_models::TypeRepresentation::String,
                aggregate_functions: BTreeMap::from_iter([
                    ("max".into(), ndc_models::AggregateFunctionDefinition::Max),
                    ("min".into(), ndc_models::AggregateFunctionDefinition::Min),
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
                    (
                        "starts_with".into(),
                        ndc_models::ComparisonOperatorDefinition::Custom {
                            argument_type: ndc_models::Type::Named {
                                name: "String".into(),
                            },
                        },
                    ),
                    (
                        "ends_with".into(),
                        ndc_models::ComparisonOperatorDefinition::Custom {
                            argument_type: ndc_models::Type::Named {
                                name: "String".into(),
                            },
                        },
                    ),
                    (
                        "_contains".into(),
                        ndc_models::ComparisonOperatorDefinition::Custom {
                            argument_type: ndc_models::Type::Named {
                                name: "String".into(),
                            },
                        },
                    ),
                    (
                        "istarts_with".into(),
                        ndc_models::ComparisonOperatorDefinition::Custom {
                            argument_type: ndc_models::Type::Named {
                                name: "String".into(),
                            },
                        },
                    ),
                    (
                        "iends_with".into(),
                        ndc_models::ComparisonOperatorDefinition::Custom {
                            argument_type: ndc_models::Type::Named {
                                name: "String".into(),
                            },
                        },
                    ),
                    (
                        "_icontains".into(),
                        ndc_models::ComparisonOperatorDefinition::Custom {
                            argument_type: ndc_models::Type::Named {
                                name: "String".into(),
                            },
                        },
                    ),
                ]),
                extraction_functions: BTreeMap::new(),
            },
        ),
        (
            "Date".into(),
            ndc_models::ScalarType {
                representation: ndc_models::TypeRepresentation::Date,
                aggregate_functions: BTreeMap::from_iter([]),
                comparison_operators: BTreeMap::from_iter([(
                    "_eq".into(),
                    ndc_models::ComparisonOperatorDefinition::Equal,
                )]),
                extraction_functions: BTreeMap::from_iter([
                    (
                        "year".into(),
                        ndc_models::ExtractionFunctionDefinition::Year {
                            result_type: ndc_models::ScalarTypeName::from("Int"),
                        },
                    ),
                    (
                        "month".into(),
                        ndc_models::ExtractionFunctionDefinition::Month {
                            result_type: ndc_models::ScalarTypeName::from("Int"),
                        },
                    ),
                    (
                        "day".into(),
                        ndc_models::ExtractionFunctionDefinition::Day {
                            result_type: ndc_models::ScalarTypeName::from("Int"),
                        },
                    ),
                ]),
            },
        ),
        (
            "Int".into(),
            ndc_models::ScalarType {
                representation: ndc_models::TypeRepresentation::Int32,
                aggregate_functions: BTreeMap::from_iter([
                    ("max".into(), ndc_models::AggregateFunctionDefinition::Max),
                    ("min".into(), ndc_models::AggregateFunctionDefinition::Min),
                ]),
                comparison_operators: BTreeMap::from_iter([(
                    "_eq".into(),
                    ndc_models::ComparisonOperatorDefinition::Equal,
                )]),
                extraction_functions: BTreeMap::new(),
            },
        ),
        (
            "BigInt".into(),
            ndc_models::ScalarType {
                representation: ndc_models::TypeRepresentation::BigInteger,
                aggregate_functions: BTreeMap::from_iter([
                    ("max".into(), ndc_models::AggregateFunctionDefinition::Max),
                    ("min".into(), ndc_models::AggregateFunctionDefinition::Min),
                ]),
                comparison_operators: BTreeMap::from_iter([(
                    "_eq".into(),
                    ndc_models::ComparisonOperatorDefinition::Equal,
                )]),
                extraction_functions: BTreeMap::new(),
            },
        ),
        (
            "Int64".into(),
            ndc_models::ScalarType {
                representation: ndc_models::TypeRepresentation::Int64,
                aggregate_functions: BTreeMap::from_iter([
                    ("max".into(), ndc_models::AggregateFunctionDefinition::Max),
                    ("min".into(), ndc_models::AggregateFunctionDefinition::Min),
                ]),
                comparison_operators: BTreeMap::from_iter([(
                    "_eq".into(),
                    ndc_models::ComparisonOperatorDefinition::Equal,
                )]),
                extraction_functions: BTreeMap::new(),
            },
        ),
        (
            "Bool".into(),
            ndc_models::ScalarType {
                representation: ndc_models::TypeRepresentation::Boolean,
                aggregate_functions: BTreeMap::new(),
                comparison_operators: BTreeMap::from_iter([(
                    "eq".into(),
                    ndc_models::ComparisonOperatorDefinition::Custom {
                        argument_type: ndc_models::Type::Named {
                            name: "Bool".into(),
                        },
                    },
                )]),
                extraction_functions: BTreeMap::new(),
            },
        ),
        (
            "Actor_Name".into(),
            ndc_models::ScalarType {
                representation: ndc_models::TypeRepresentation::String,
                aggregate_functions: BTreeMap::new(),
                comparison_operators: BTreeMap::new(),
                extraction_functions: BTreeMap::new(),
            },
        ),
        (
            "HeaderMap".into(),
            ndc_models::ScalarType {
                representation: ndc_models::TypeRepresentation::JSON,
                aggregate_functions: BTreeMap::new(),
                comparison_operators: BTreeMap::new(),
                extraction_functions: BTreeMap::new(),
            },
        ),
        (
            "YesNo".into(),
            ndc_models::ScalarType {
                representation: ndc_models::TypeRepresentation::Enum {
                    one_of: vec!["yes".into(), "no".into()],
                },
                aggregate_functions: BTreeMap::new(),
                comparison_operators: BTreeMap::new(),
                extraction_functions: BTreeMap::new(),
            },
        ),
    ])
}

pub(crate) fn object_types() -> BTreeMap<ndc_models::ObjectTypeName, ndc_models::ObjectType> {
    BTreeMap::from_iter([
        ("actor".into(), actor::definition()),
        ("city".into(), city::definition()),
        ("country".into(), country::definition()),
        ("continent".into(), continent::definition()),
        (
            "evaluated_institution".into(),
            evaluated_institution::definition(),
        ),
        ("movie".into(), movie::definition()),
        ("genre".into(), genre::definition()),
        ("name_query".into(), name_query::definition()),
        ("institution".into(), institution::definition()),
        ("location".into(), location::definition()),
        (
            "location_pascalcase".into(),
            location_pascalcase::definition(),
        ),
        ("staff_member".into(), staff_member::definition()),
        ("login_response".into(), login::definition_login_response()),
        (
            "session_response".into(),
            login::definition_session_response(),
        ),
        ("session_info".into(), login::definition_session_info()),
        ("where".into(), r#where::definition()),
        ("where_string".into(), where_string::definition()),
        ("where_int".into(), where_int::definition()),
    ])
}
