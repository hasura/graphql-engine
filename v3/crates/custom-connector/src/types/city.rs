use std::collections::BTreeMap;

pub(crate) fn definition() -> ndc_models::ObjectType {
    ndc_models::ObjectType {
        description: Some("A city".into()),
        fields: BTreeMap::from_iter([(
            "name".into(),
            ndc_models::ObjectField {
                description: Some("The city's name".into()),
                r#type: ndc_models::Type::Named {
                    name: "String".into(),
                },
                arguments: BTreeMap::new(),
            },
        )]),
        foreign_keys: BTreeMap::new(),
    }
}
