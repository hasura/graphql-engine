use std::collections::BTreeMap;

pub(crate) fn definition() -> ndc_models::ObjectType {
    ndc_models::ObjectType {
        description: Some("A country".into()),
        fields: BTreeMap::from_iter([
            (
                "id".into(),
                ndc_models::ObjectField {
                    description: Some("The country's primary key".into()),
                    r#type: ndc_models::Type::Named { name: "Int".into() },
                    arguments: BTreeMap::new(),
                },
            ),
            (
                "name".into(),
                ndc_models::ObjectField {
                    description: Some("The country's name".into()),
                    r#type: ndc_models::Type::Named {
                        name: "String".into(),
                    },
                    arguments: BTreeMap::new(),
                },
            ),
            (
                "area_km2".into(),
                ndc_models::ObjectField {
                    description: Some("The country's area size in square kilometers".into()),
                    r#type: ndc_models::Type::Named { name: "Int".into() },
                    arguments: BTreeMap::new(),
                },
            ),
            (
                "cities".into(),
                ndc_models::ObjectField {
                    description: Some("The cities in the country".into()),
                    r#type: ndc_models::Type::Array {
                        element_type: Box::new(ndc_models::Type::Named {
                            name: "city".into(),
                        }),
                    },
                    arguments: BTreeMap::new(),
                },
            ),
        ]),
        foreign_keys: BTreeMap::new(),
    }
}
