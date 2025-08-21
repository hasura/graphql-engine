use std::collections::BTreeMap;

use ndc_models;

pub(crate) fn definition() -> ndc_models::ObjectType {
    ndc_models::ObjectType {
        description: Some("A where clause".into()),
        fields: BTreeMap::from_iter([
            (
                "name".into(),
                ndc_models::ObjectField {
                    description: Some("Optional filtering over name".into()),
                    r#type: ndc_models::Type::Nullable {
                        underlying_type: Box::new(ndc_models::Type::Named {
                            name: "where_string".into(),
                        }),
                    },
                    arguments: BTreeMap::new(),
                },
            ),
            (
                "age".into(),
                ndc_models::ObjectField {
                    description: Some("Optional filtering over age".into()),
                    r#type: ndc_models::Type::Nullable {
                        underlying_type: Box::new(ndc_models::Type::Named {
                            name: "where_int".into(),
                        }),
                    },
                    arguments: BTreeMap::new(),
                },
            ),
        ]),
        foreign_keys: BTreeMap::new(),
    }
}
