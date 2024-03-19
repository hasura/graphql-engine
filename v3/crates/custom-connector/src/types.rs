use ndc_client::models;
use std::collections::BTreeMap;

pub mod actor;
pub mod name_query;

pub(crate) fn scalar_types() -> BTreeMap<String, models::ScalarType> {
    BTreeMap::from_iter([
        (
            "String".into(),
            models::ScalarType {
                aggregate_functions: BTreeMap::new(),
                comparison_operators: BTreeMap::from_iter([(
                    "like".into(),
                    models::ComparisonOperatorDefinition::Custom {
                        argument_type: models::Type::Named {
                            name: "String".into(),
                        },
                    },
                )]),
            },
        ),
        (
            "Int".into(),
            models::ScalarType {
                aggregate_functions: BTreeMap::from_iter([
                    (
                        "max".into(),
                        models::AggregateFunctionDefinition {
                            result_type: models::Type::Nullable {
                                underlying_type: Box::new(models::Type::Named {
                                    name: "Int".into(),
                                }),
                            },
                        },
                    ),
                    (
                        "min".into(),
                        models::AggregateFunctionDefinition {
                            result_type: models::Type::Nullable {
                                underlying_type: Box::new(models::Type::Named {
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
            models::ScalarType {
                aggregate_functions: BTreeMap::new(),
                comparison_operators: BTreeMap::new(),
            },
        ),
    ])
}

pub(crate) fn object_types() -> BTreeMap<String, models::ObjectType> {
    let movie_type = models::ObjectType {
        description: Some("A movie".into()),
        fields: BTreeMap::from_iter([
            (
                "id".into(),
                models::ObjectField {
                    description: Some("The movie's primary key".into()),
                    r#type: models::Type::Named { name: "Int".into() },
                },
            ),
            (
                "title".into(),
                models::ObjectField {
                    description: Some("The movie's title".into()),
                    r#type: models::Type::Named {
                        name: "String".into(),
                    },
                },
            ),
            (
                "rating".into(),
                models::ObjectField {
                    description: Some("The movie's rating".into()),
                    r#type: models::Type::Named { name: "Int".into() },
                },
            ),
        ]),
    };

    let institution_type = models::ObjectType {
        description: Some("An institution".into()),
        fields: BTreeMap::from_iter([
            (
                "id".into(),
                models::ObjectField {
                    description: Some("The institution's primary key".into()),
                    r#type: models::Type::Named { name: "Int".into() },
                },
            ),
            (
                "name".into(),
                models::ObjectField {
                    description: Some("The institution's name".into()),
                    r#type: models::Type::Named {
                        name: "String".into(),
                    },
                },
            ),
            (
                "location".into(),
                models::ObjectField {
                    description: Some("The institution's location".into()),
                    r#type: models::Type::Named {
                        name: "location".into(),
                    },
                },
            ),
            (
                "staff".into(),
                models::ObjectField {
                    description: Some("The institution's staff".into()),
                    r#type: models::Type::Array {
                        element_type: Box::new(models::Type::Named {
                            name: "staff_member".into(),
                        }),
                    },
                },
            ),
            (
                "departments".into(),
                models::ObjectField {
                    description: Some("The institution's departments".into()),
                    r#type: models::Type::Array {
                        element_type: Box::new(models::Type::Named {
                            name: "String".into(),
                        }),
                    },
                },
            ),
        ]),
    };

    let location_type = models::ObjectType {
        description: Some("A location".into()),
        fields: BTreeMap::from_iter([
            (
                "city".into(),
                models::ObjectField {
                    description: Some("The location's city".into()),
                    r#type: models::Type::Named {
                        name: "String".into(),
                    },
                },
            ),
            (
                "country".into(),
                models::ObjectField {
                    description: Some("The location's country".into()),
                    r#type: models::Type::Named {
                        name: "String".into(),
                    },
                },
            ),
            (
                "campuses".into(),
                models::ObjectField {
                    description: Some("The location's campuses".into()),
                    r#type: models::Type::Array {
                        element_type: Box::new(models::Type::Named {
                            name: "String".into(),
                        }),
                    },
                },
            ),
        ]),
    };

    let staff_member_type = models::ObjectType {
        description: Some("A staff member".into()),
        fields: BTreeMap::from_iter([
            (
                "first_name".into(),
                models::ObjectField {
                    description: Some("The staff member's first name".into()),
                    r#type: models::Type::Named {
                        name: "String".into(),
                    },
                },
            ),
            (
                "last_name".into(),
                models::ObjectField {
                    description: Some("The staff member's last name".into()),
                    r#type: models::Type::Named {
                        name: "String".into(),
                    },
                },
            ),
            (
                "specialities".into(),
                models::ObjectField {
                    description: Some("The staff member's specialities".into()),
                    r#type: models::Type::Array {
                        element_type: Box::new(models::Type::Named {
                            name: "String".into(),
                        }),
                    },
                },
            ),
        ]),
    };

    BTreeMap::from_iter([
        ("actor".into(), actor::definition()),
        ("movie".into(), movie_type),
        ("name_query".into(), name_query::definition()),
        ("institution".into(), institution_type),
        ("location".into(), location_type),
        ("staff_member".into(), staff_member_type),
    ])
}
