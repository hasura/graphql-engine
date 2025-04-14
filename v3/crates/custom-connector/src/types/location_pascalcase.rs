use std::collections::BTreeMap;

use ndc_models;

use crate::arguments::argument_string;

pub(crate) fn definition() -> ndc_models::ObjectType {
    ndc_models::ObjectType {
        description: Some("A location, but with pascal-case properties".into()),
        fields: BTreeMap::from_iter([
            (
                "City".into(),
                ndc_models::ObjectField {
                    description: Some("The location's city".into()),
                    r#type: ndc_models::Type::Named {
                        name: "String".into(),
                    },
                    arguments: argument_string(),
                },
            ),
            (
                "Country".into(),
                ndc_models::ObjectField {
                    description: Some("The location's country".into()),
                    r#type: ndc_models::Type::Named {
                        name: "String".into(),
                    },
                    arguments: argument_string(),
                },
            ),
            (
                "CountryId".into(),
                ndc_models::ObjectField {
                    description: Some("The location's country ID".into()),
                    r#type: ndc_models::Type::Named { name: "Int".into() },
                    arguments: BTreeMap::new(),
                },
            ),
            (
                "Campuses".into(),
                ndc_models::ObjectField {
                    description: Some("The location's campuses".into()),
                    r#type: ndc_models::Type::Array {
                        element_type: Box::new(ndc_models::Type::Named {
                            name: "String".into(),
                        }),
                    },
                    arguments: BTreeMap::new(),
                },
            ),
        ]),
        foreign_keys: BTreeMap::new(),
    }
}
