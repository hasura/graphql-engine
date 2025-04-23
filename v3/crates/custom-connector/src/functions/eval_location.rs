use std::collections::BTreeMap;

use ndc_models;

use crate::{
    arguments::{
        check_all_arguments_used, parse_expression_argument, parse_nullable_object_argument,
    },
    query::{Result, eval_expression},
    state::{AppState, Row},
};

pub(crate) fn function_info() -> ndc_models::FunctionInfo {
    ndc_models::FunctionInfo {
        name: "eval_location".into(),
        description: Some(
            "Evaluates a submitted location object against the provided boolean expression".into(),
        ),
        arguments: BTreeMap::from_iter([
            (
                "location".into(),
                ndc_models::ArgumentInfo {
                    description: Some("The location to evaluate".into()),
                    argument_type: ndc_models::Type::Nullable {
                        underlying_type: Box::new(ndc_models::Type::Named {
                            name: "location_pascalcase".into(),
                        }),
                    },
                },
            ),
            (
                "check".into(),
                ndc_models::ArgumentInfo {
                    description: Some(
                        "The boolean expression to evaluate the location against".into(),
                    ),
                    argument_type: ndc_models::Type::Predicate {
                        object_type_name: "location_pascalcase".into(),
                    },
                },
            ),
        ]),
        result_type: ndc_models::Type::Named {
            name: "Bool".into(),
        },
    }
}

pub(crate) fn rows(
    arguments: &BTreeMap<ndc_models::ArgumentName, serde_json::Value>,
    collection_relationships: &BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
    state: &AppState,
) -> Result<Vec<Row>> {
    let mut arguments = arguments
        .iter()
        .map(|(k, v)| (k.clone(), v))
        .collect::<BTreeMap<_, _>>();
    let location = parse_nullable_object_argument("location", &mut arguments)?;
    let check = parse_expression_argument("check", &mut arguments)?;
    check_all_arguments_used(&arguments)?;

    let evaluation_result = if let Some(location) = location {
        let location_row = location
            .iter()
            .map(|(k, v)| (ndc_models::FieldName::from(k.as_str()), v.clone()))
            .collect::<BTreeMap<_, _>>();

        eval_expression(
            collection_relationships,
            &BTreeMap::new(),
            state,
            &check,
            &location_row,
            &location_row,
        )?
    } else {
        false
    };

    let return_value = serde_json::Value::Bool(evaluation_result);

    Ok(vec![BTreeMap::from_iter([(
        "__value".into(),
        return_value,
    )])])
}
