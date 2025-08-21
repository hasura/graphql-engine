use std::collections::BTreeMap;

use ndc_models;

pub(crate) fn definition() -> ndc_models::ObjectType {
    ndc_models::ObjectType {
        description: Some("A where comparison over ints".into()),
        fields: BTreeMap::from_iter([
            (
                "_eq".into(),
                ndc_models::ObjectField {
                    description: Some("Optional equality check".into()),
                    r#type: ndc_models::Type::Nullable {
                        underlying_type: Box::new(ndc_models::Type::Named { name: "int".into() }),
                    },
                    arguments: BTreeMap::new(),
                },
            ),
            (
                "_lt".into(),
                ndc_models::ObjectField {
                    description: Some("Optional less-than check".into()),
                    r#type: ndc_models::Type::Nullable {
                        underlying_type: Box::new(ndc_models::Type::Named { name: "int".into() }),
                    },
                    arguments: BTreeMap::new(),
                },
            ),
            (
                "_gt".into(),
                ndc_models::ObjectField {
                    description: Some("Optional less-than check".into()),
                    r#type: ndc_models::Type::Nullable {
                        underlying_type: Box::new(ndc_models::Type::Named { name: "int".into() }),
                    },
                    arguments: BTreeMap::new(),
                },
            ),
        ]),
        foreign_keys: BTreeMap::new(),
    }
}
