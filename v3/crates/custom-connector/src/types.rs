use ndc_models;
use std::collections::BTreeMap;

pub mod actor;
pub mod institution;
pub mod location;
pub mod name_query;
pub mod staff_member;

pub(crate) fn scalar_types() -> BTreeMap<String, ndc_models::ScalarType> {
    BTreeMap::from_iter([
        (
            "String".into(),
            ndc_models::ScalarType {
                representation: Some(ndc_models::TypeRepresentation::String),
                aggregate_functions: BTreeMap::new(),
                comparison_operators: BTreeMap::from_iter([(
                    "like".into(),
                    ndc_models::ComparisonOperatorDefinition::Custom {
                        argument_type: ndc_models::Type::Named {
                            name: "String".into(),
                        },
                    },
                )]),
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
                comparison_operators: BTreeMap::from_iter([]),
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
    ])
}

pub(crate) fn object_types() -> BTreeMap<String, ndc_models::ObjectType> {
    let movie_type = ndc_models::ObjectType {
        description: Some("A movie".into()),
        fields: BTreeMap::from_iter([
            (
                "id".into(),
                ndc_models::ObjectField {
                    description: Some("The movie's primary key".into()),
                    r#type: ndc_models::Type::Named { name: "Int".into() },
                },
            ),
            (
                "title".into(),
                ndc_models::ObjectField {
                    description: Some("The movie's title".into()),
                    r#type: ndc_models::Type::Named {
                        name: "String".into(),
                    },
                },
            ),
            (
                "rating".into(),
                ndc_models::ObjectField {
                    description: Some("The movie's rating".into()),
                    r#type: ndc_models::Type::Named { name: "Int".into() },
                },
            ),
        ]),
    };

    BTreeMap::from_iter([
        ("actor".into(), actor::definition()),
        ("movie".into(), movie_type),
        ("name_query".into(), name_query::definition()),
        ("institution".into(), institution::definition()),
        ("location".into(), location::definition()),
        ("staff_member".into(), staff_member::definition()),
    ])
}
