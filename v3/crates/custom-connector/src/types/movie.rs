use std::collections::BTreeMap;

use ndc_models;

use crate::arguments::{argument_any, argument_string};

pub(crate) fn definition() -> ndc_models::ObjectType {
    ndc_models::ObjectType {
        description: Some("A movie".into()),
        fields: BTreeMap::from_iter([
            (
                "id".into(),
                ndc_models::ObjectField {
                    description: Some("The movie's primary key".into()),
                    r#type: ndc_models::Type::Named { name: "Int".into() },
                    arguments: argument_any(),
                },
            ),
            (
                "title".into(),
                ndc_models::ObjectField {
                    description: Some("The movie's title".into()),
                    r#type: ndc_models::Type::Named {
                        name: "String".into(),
                    },
                    arguments: argument_string(),
                },
            ),
            (
                "rating".into(),
                ndc_models::ObjectField {
                    description: Some("The movie's rating".into()),
                    r#type: ndc_models::Type::Named { name: "Int".into() },
                    arguments: argument_any(),
                },
            ),
            (
                "genres".into(),
                ndc_models::ObjectField {
                    description: Some("The movie's genres".into()),
                    r#type: ndc_models::Type::Array {
                        element_type: Box::new(ndc_models::Type::Named {
                            name: "genre".into(),
                        }),
                    },
                    arguments: BTreeMap::new(),
                },
            ),
        ]),
        foreign_keys: BTreeMap::new(),
    }
}
