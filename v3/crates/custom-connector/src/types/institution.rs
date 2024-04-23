use std::collections::BTreeMap;

use ndc_models;

pub(crate) fn definition() -> ndc_models::ObjectType {
    ndc_models::ObjectType {
        description: Some("An institution".into()),
        fields: BTreeMap::from_iter([
            (
                "id".into(),
                ndc_models::ObjectField {
                    description: Some("The institution's primary key".into()),
                    r#type: ndc_models::Type::Named { name: "Int".into() },
                },
            ),
            (
                "name".into(),
                ndc_models::ObjectField {
                    description: Some("The institution's name".into()),
                    r#type: ndc_models::Type::Named {
                        name: "String".into(),
                    },
                },
            ),
            (
                "location".into(),
                ndc_models::ObjectField {
                    description: Some("The institution's location".into()),
                    r#type: ndc_models::Type::Named {
                        name: "location".into(),
                    },
                },
            ),
            (
                "staff".into(),
                ndc_models::ObjectField {
                    description: Some("The institution's staff".into()),
                    r#type: ndc_models::Type::Array {
                        element_type: Box::new(ndc_models::Type::Named {
                            name: "staff_member".into(),
                        }),
                    },
                },
            ),
            (
                "departments".into(),
                ndc_models::ObjectField {
                    description: Some("The institution's departments".into()),
                    r#type: ndc_models::Type::Array {
                        element_type: Box::new(ndc_models::Type::Named {
                            name: "String".into(),
                        }),
                    },
                },
            ),
        ]),
    }
}
