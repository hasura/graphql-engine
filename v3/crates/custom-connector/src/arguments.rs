use std::collections::BTreeMap;

use axum::{http::StatusCode, Json};
use ndc_models::Argument;
use sha2::{Digest, Sha224, Sha256, Sha384, Sha512};

use crate::query::Result;

pub fn argument_string() -> BTreeMap<String, ndc_models::ArgumentInfo> {
    BTreeMap::from_iter([
        (
            "change_case".into(),
            ndc_models::ArgumentInfo {
                description: Some("Change the case of a string".into()),
                argument_type: ndc_models::Type::Named {
                    name: "String".into(),
                },
            },
        ),
        (
            "limit".into(),
            ndc_models::ArgumentInfo {
                description: Some("Limit the length of a string".into()),
                argument_type: ndc_models::Type::Named { name: "Int".into() },
            },
        ),
        (
            "offset".into(),
            ndc_models::ArgumentInfo {
                description: Some("Offset the length of a string".into()),
                argument_type: ndc_models::Type::Named { name: "Int".into() },
            },
        ),
        (
            "hash".into(),
            ndc_models::ArgumentInfo {
                description: Some("Hash a string".into()),
                argument_type: ndc_models::Type::Named {
                    name: "String".into(),
                },
            },
        ),
    ])
}

pub fn argument_any() -> BTreeMap<String, ndc_models::ArgumentInfo> {
    BTreeMap::from_iter([(
        "hash".into(),
        ndc_models::ArgumentInfo {
            description: Some("Calculate hash".into()),
            argument_type: ndc_models::Type::Named {
                name: "String".into(),
            },
        },
    )])
}

pub fn apply_arguments(
    result: ndc_models::RowFieldValue,
    arguments: &BTreeMap<String, ndc_models::Argument>,
    variables: &BTreeMap<String, serde_json::Value>,
) -> Result<ndc_models::RowFieldValue> {
    let mut json_result = result.0;
    for (argument_name, argument) in arguments {
        let argument_value = match argument {
            Argument::Literal { value } => Ok(value),
            Argument::Variable { name } => variables.get(name.as_str()).ok_or((
                StatusCode::BAD_REQUEST,
                Json(ndc_models::ErrorResponse {
                    message: "invalid variable name".into(),
                    details: serde_json::Value::Null,
                }),
            )),
        }?;
        json_result = apply_argument(argument_name, argument_value, &json_result)?;
    }
    Ok(ndc_models::RowFieldValue(json_result))
}

fn apply_argument(
    argument_name: &str,
    argument_value: &serde_json::Value,
    previous_result: &serde_json::Value,
) -> Result<serde_json::Value> {
    match argument_name {
        "change_case" => change_case(argument_value, previous_result),
        "limit" => limit_string_length(argument_value, previous_result),
        "offset" => offset_string_length(argument_value, previous_result),
        "hash" => hash(argument_value, previous_result),
        _ => Err((
            StatusCode::BAD_REQUEST,
            Json(ndc_models::ErrorResponse {
                message: format!("invalid argument name: {argument_name}"),
                details: serde_json::Value::Null,
            }),
        )),
    }
}

fn change_case(
    argument_value: &serde_json::Value,
    previous_result: &serde_json::Value,
) -> Result<serde_json::Value> {
    let case = argument_value
        .as_str()
        .ok_or((
            StatusCode::BAD_REQUEST,
            Json(ndc_models::ErrorResponse {
                message: "argument to change_case can only be a string".into(),
                details: serde_json::Value::Null,
            }),
        ))?
        .to_lowercase();

    let previous_result = previous_result.as_str().ok_or((
        StatusCode::BAD_REQUEST,
        Json(ndc_models::ErrorResponse {
            message: "change_case can only be applied to string fields".into(),
            details: serde_json::Value::Null,
        }),
    ))?;

    let result = match case.as_str() {
        "lower" => previous_result.to_lowercase(),
        "upper" => previous_result.to_uppercase(),
        _ => {
            return Err((
                StatusCode::BAD_REQUEST,
                Json(ndc_models::ErrorResponse {
                    message: "change_case argument must be 'lower' or 'upper'".into(),
                    details: serde_json::Value::Null,
                }),
            ))
        }
    };

    Ok(serde_json::Value::String(result))
}

fn limit_string_length(
    argument_value: &serde_json::Value,
    previous_result: &serde_json::Value,
) -> Result<serde_json::Value> {
    let length = argument_value.as_u64().ok_or((
        StatusCode::BAD_REQUEST,
        Json(ndc_models::ErrorResponse {
            message: "argument to limit_string_length can only be a whole number".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    let usize_length = usize::try_from(length).map_err(|_| {
        (
            StatusCode::BAD_REQUEST,
            Json(ndc_models::ErrorResponse {
                message: "limit_string_length argument is out of range".into(),
                details: serde_json::Value::Null,
            }),
        )
    })?;

    let previous_result = previous_result.as_str().ok_or((
        StatusCode::BAD_REQUEST,
        Json(ndc_models::ErrorResponse {
            message: "limit_string_length can only be applied to string fields".into(),
            details: serde_json::Value::Null,
        }),
    ))?;

    let result = if previous_result.len() > usize_length {
        previous_result[..usize_length].to_string()
    } else {
        previous_result.to_string()
    };

    Ok(serde_json::Value::String(result))
}

fn offset_string_length(
    argument_value: &serde_json::Value,
    previous_result: &serde_json::Value,
) -> Result<serde_json::Value> {
    let length = argument_value.as_u64().ok_or((
        StatusCode::BAD_REQUEST,
        Json(ndc_models::ErrorResponse {
            message: "argument to offset_string_length can only be a whole number".into(),
            details: serde_json::Value::Null,
        }),
    ))?;

    let usize_length = usize::try_from(length).map_err(|_| {
        (
            StatusCode::BAD_REQUEST,
            Json(ndc_models::ErrorResponse {
                message: "offset_string_lengths argument is out of range".into(),
                details: serde_json::Value::Null,
            }),
        )
    })?;

    let previous_result = previous_result.as_str().ok_or((
        StatusCode::BAD_REQUEST,
        Json(ndc_models::ErrorResponse {
            message: "offset_string_length can only be applied to string fields".into(),
            details: serde_json::Value::Null,
        }),
    ))?;

    let result = if previous_result.len() > usize_length {
        previous_result[usize_length..].to_string()
    } else {
        String::new()
    };

    Ok(serde_json::Value::String(result))
}

fn hash(
    argument_value: &serde_json::Value,
    previous_result: &serde_json::Value,
) -> Result<serde_json::Value> {
    let algorithm = argument_value
        .as_str()
        .ok_or((
            StatusCode::BAD_REQUEST,
            Json(ndc_models::ErrorResponse {
                message: "argument to hash can only be a string".into(),
                details: serde_json::Value::Null,
            }),
        ))?
        .to_lowercase();
    let previous_result_bytes = serde_json::to_vec(previous_result).map_err(|_| {
        (
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(ndc_models::ErrorResponse {
                message: "failed to serialize previous result for hashing".into(),
                details: serde_json::Value::Null,
            }),
        )
    })?;
    let hash = match algorithm.as_str() {
        "sha224" => {
            let mut hasher = Sha224::new();
            hasher.update(&previous_result_bytes);
            format!("{:x}", hasher.finalize())
        }
        "sha256" => {
            let mut hasher = Sha256::new();
            hasher.update(&previous_result_bytes);
            format!("{:x}", hasher.finalize())
        }
        "sha384" => {
            let mut hasher = Sha384::new();
            hasher.update(&previous_result_bytes);
            format!("{:x}", hasher.finalize())
        }
        "sha512" => {
            let mut hasher = Sha512::new();
            hasher.update(&previous_result_bytes);
            format!("{:x}", hasher.finalize())
        }
        _ => {
            return Err((
                StatusCode::BAD_REQUEST,
                Json(ndc_models::ErrorResponse {
                    message: "hash argument must be 'sha224', 'sha256', 'sha384', or 'sha512'"
                        .into(),
                    details: serde_json::Value::Null,
                }),
            ))
        }
    };
    Ok(serde_json::Value::String(hash))
}
