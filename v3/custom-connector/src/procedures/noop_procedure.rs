use std::collections::BTreeMap;

use ndc_client::models;

use crate::query::Result;

pub(crate) fn procedure_info() -> models::ProcedureInfo {
    models::ProcedureInfo {
        name: "noop_procedure".into(),
        description: Some(
            "Procedure which does not perform any actual mutuations on the data".into(),
        ),
        arguments: BTreeMap::new(),
        result_type: models::Type::Nullable {
            underlying_type: Box::new(models::Type::Named {
                name: "String".into(),
            }),
        },
    }
}

pub(crate) fn execute() -> Result<serde_json::Value> {
    Ok(serde_json::Value::String("Noop Procedure".to_string()))
}
