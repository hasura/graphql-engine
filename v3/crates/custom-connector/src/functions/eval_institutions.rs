use std::collections::BTreeMap;

use ndc_models;

use crate::{
    arguments::{check_all_arguments_used, parse_expression_argument, parse_object_array_argument},
    query::{Result, eval_expression},
    state::{AppState, Row},
};

pub(crate) fn function_info() -> ndc_models::FunctionInfo {
    ndc_models::FunctionInfo {
        name: "eval_institutions".into(),
        description: Some(
            "Evaluates submitted institution objects against the provided boolean expression"
                .into(),
        ),
        arguments: BTreeMap::from_iter([
            (
                "institutions".into(),
                ndc_models::ArgumentInfo {
                    description: Some("The institutions to evaluate".into()),
                    argument_type: ndc_models::Type::Array {
                        element_type: Box::new(ndc_models::Type::Named {
                            name: "institution".into(),
                        }),
                    },
                },
            ),
            (
                "check".into(),
                ndc_models::ArgumentInfo {
                    description: Some(
                        "The boolean expression to evaluate the institutions against".into(),
                    ),
                    argument_type: ndc_models::Type::Predicate {
                        object_type_name: "institution".into(),
                    },
                },
            ),
        ]),
        result_type: ndc_models::Type::Array {
            element_type: Box::new(ndc_models::Type::Named {
                name: "evaluated_institution".into(),
            }),
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
    let institutions = parse_object_array_argument("institutions", &mut arguments)?;
    let check = parse_expression_argument("check", &mut arguments)?;
    check_all_arguments_used(&arguments)?;

    let evaluated_institutions = institutions
        .into_iter()
        .map(|institution| {
            let institution_row = institution
                .iter()
                .map(|(k, v)| (ndc_models::FieldName::from(k.as_str()), v.clone()))
                .collect::<BTreeMap<_, _>>();

            let evaluation_result = eval_expression(
                collection_relationships,
                &BTreeMap::new(),
                state,
                &check,
                &institution_row,
                &institution_row,
            )?;

            Ok(serde_json::json!({
                "institution": institution,
                "evaluation_result": evaluation_result
            }))
        })
        .collect::<Result<Vec<_>>>()?;
    let return_value = serde_json::Value::Array(evaluated_institutions);

    Ok(vec![BTreeMap::from_iter([(
        "__value".into(),
        return_value,
    )])])
}
