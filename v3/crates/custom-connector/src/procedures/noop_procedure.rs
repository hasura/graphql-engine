use std::collections::BTreeMap;

use ndc_models;

use crate::{arguments::check_all_arguments_used, query::Result};

pub(crate) fn procedure_info() -> ndc_models::ProcedureInfo {
    ndc_models::ProcedureInfo {
        name: "noop_procedure".into(),
        description: Some(
            "Procedure which does not perform any actual mutuations on the data".into(),
        ),
        arguments: BTreeMap::new(),
        result_type: ndc_models::Type::Nullable {
            underlying_type: Box::new(ndc_models::Type::Named {
                name: "String".into(),
            }),
        },
    }
}

pub(crate) fn execute(
    arguments: &BTreeMap<ndc_models::ArgumentName, serde_json::Value>,
) -> Result<serde_json::Value> {
    check_all_arguments_used(arguments)?;

    Ok(serde_json::Value::String("Noop Procedure".to_string()))
}
