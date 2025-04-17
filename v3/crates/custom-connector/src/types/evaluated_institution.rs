use std::collections::BTreeMap;

use ndc_models;

pub(crate) fn definition() -> ndc_models::ObjectType {
    ndc_models::ObjectType {
        description: Some("An institution and its evaluation result".into()),
        fields: BTreeMap::from_iter([
            (
                "institution".into(),
                ndc_models::ObjectField {
                    description: Some("The institution".into()),
                    r#type: ndc_models::Type::Named {
                        name: "institution".into(),
                    },
                    arguments: BTreeMap::new(),
                },
            ),
            (
                "evaluation_result".into(),
                ndc_models::ObjectField {
                    description: Some("The institution's evaluation result".into()),
                    r#type: ndc_models::Type::Named {
                        name: "Bool".into(),
                    },
                    arguments: BTreeMap::new(),
                },
            ),
        ]),
        foreign_keys: BTreeMap::new(),
    }
}
