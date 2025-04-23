use std::{collections::BTreeMap, sync::Arc};

use ndc_models;

use crate::arguments::argument_string;

use datafusion::arrow::datatypes::Field;

pub(crate) fn definition() -> ndc_models::ObjectType {
    ndc_models::ObjectType {
        description: Some("A location".into()),
        fields: BTreeMap::from_iter([
            (
                "city".into(),
                ndc_models::ObjectField {
                    description: Some("The location's city".into()),
                    r#type: ndc_models::Type::Named {
                        name: "String".into(),
                    },
                    arguments: argument_string(),
                },
            ),
            (
                "country".into(),
                ndc_models::ObjectField {
                    description: Some("The location's country".into()),
                    r#type: ndc_models::Type::Named {
                        name: "String".into(),
                    },
                    arguments: argument_string(),
                },
            ),
            (
                "country_id".into(),
                ndc_models::ObjectField {
                    description: Some("The location's country ID".into()),
                    r#type: ndc_models::Type::Named { name: "Int".into() },
                    arguments: BTreeMap::new(),
                },
            ),
            (
                "campuses".into(),
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

pub(crate) fn arrow_type() -> datafusion::arrow::datatypes::DataType {
    datafusion::arrow::datatypes::DataType::Struct(datafusion::arrow::datatypes::Fields::from(
        vec![
            Field::new("city", datafusion::arrow::datatypes::DataType::Utf8, true),
            Field::new(
                "country",
                datafusion::arrow::datatypes::DataType::Utf8,
                true,
            ),
            Field::new(
                "campuses",
                datafusion::arrow::datatypes::DataType::List(Arc::new(Field::new(
                    "item",
                    datafusion::arrow::datatypes::DataType::Utf8,
                    true,
                ))),
                true,
            ),
        ],
    ))
}
