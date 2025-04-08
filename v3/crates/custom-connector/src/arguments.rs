use std::{borrow::Borrow, collections::BTreeMap, fmt::Display};

use axum::{Json, http::StatusCode};
use ndc_models::Argument;
use sha2::{Digest, Sha224, Sha256, Sha384, Sha512};

use crate::query::Result;

pub fn argument_string() -> BTreeMap<ndc_models::ArgumentName, ndc_models::ArgumentInfo> {
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

pub fn argument_any() -> BTreeMap<ndc_models::ArgumentName, ndc_models::ArgumentInfo> {
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
    arguments: &BTreeMap<ndc_models::ArgumentName, ndc_models::Argument>,
    variables: &BTreeMap<ndc_models::VariableName, serde_json::Value>,
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
    argument_name: &ndc_models::ArgumentName,
    argument_value: &serde_json::Value,
    previous_result: &serde_json::Value,
) -> Result<serde_json::Value> {
    match argument_name.as_str() {
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
            ));
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
            ));
        }
    };
    Ok(serde_json::Value::String(hash))
}

pub(crate) fn parse_object_array_argument<'a, Arg>(
    argument_name: &Arg,
    arguments: &mut BTreeMap<ndc_models::ArgumentName, &'a serde_json::Value>,
) -> Result<Vec<&'a serde_json::Map<String, serde_json::Value>>>
where
    ndc_models::ArgumentName: Borrow<Arg>,
    Arg: Ord + ?Sized + Display,
{
    let result = arguments
        .remove(argument_name)
        .ok_or_else(|| {
            (
                StatusCode::BAD_REQUEST,
                Json(ndc_models::ErrorResponse {
                    message: format!("missing argument {argument_name}"),
                    details: serde_json::Value::Null,
                }),
            )
        })?
        .as_array()
        .ok_or_else(|| {
            (
                StatusCode::BAD_REQUEST,
                Json(ndc_models::ErrorResponse {
                    message: format!("{argument_name} must be an object"),
                    details: serde_json::Value::Null,
                }),
            )
        })?
        .iter()
        .map(|v| {
            v.as_object().ok_or_else(|| {
                (
                    StatusCode::BAD_REQUEST,
                    Json(ndc_models::ErrorResponse {
                        message: format!("{argument_name} must be an array of objects"),
                        details: serde_json::Value::Null,
                    }),
                )
            })
        })
        .collect::<Result<Vec<_>>>()?;
    Ok(result)
}

pub(crate) fn parse_object_argument<'a, Arg>(
    argument_name: &Arg,
    arguments: &mut BTreeMap<ndc_models::ArgumentName, &'a serde_json::Value>,
) -> Result<&'a serde_json::Map<String, serde_json::Value>>
where
    ndc_models::ArgumentName: Borrow<Arg>,
    Arg: Ord + ?Sized + Display,
{
    let result = arguments
        .remove(argument_name)
        .ok_or_else(|| {
            (
                StatusCode::BAD_REQUEST,
                Json(ndc_models::ErrorResponse {
                    message: format!("missing argument {argument_name}"),
                    details: serde_json::Value::Null,
                }),
            )
        })?
        .as_object()
        .ok_or_else(|| {
            (
                StatusCode::BAD_REQUEST,
                Json(ndc_models::ErrorResponse {
                    message: format!("{argument_name} must be an object"),
                    details: serde_json::Value::Null,
                }),
            )
        })?;
    Ok(result)
}

pub(crate) fn parse_nullable_object_argument<'a, Arg>(
    argument_name: &Arg,
    arguments: &mut BTreeMap<ndc_models::ArgumentName, &'a serde_json::Value>,
) -> Result<Option<&'a serde_json::Map<String, serde_json::Value>>>
where
    ndc_models::ArgumentName: Borrow<Arg>,
    Arg: Ord + ?Sized + Display,
{
    let argument_value = arguments.remove(argument_name).ok_or_else(|| {
        (
            StatusCode::BAD_REQUEST,
            Json(ndc_models::ErrorResponse {
                message: format!("missing argument {argument_name}"),
                details: serde_json::Value::Null,
            }),
        )
    })?;
    let result = if argument_value.is_null() {
        None
    } else {
        Some(argument_value.as_object().ok_or_else(|| {
            (
                StatusCode::BAD_REQUEST,
                Json(ndc_models::ErrorResponse {
                    message: format!("{argument_name} must be an object"),
                    details: serde_json::Value::Null,
                }),
            )
        })?)
    };
    Ok(result)
}

pub(crate) fn parse_i32_argument<Arg>(
    argument_name: &Arg,
    arguments: &mut BTreeMap<ndc_models::ArgumentName, &serde_json::Value>,
) -> Result<i32>
where
    ndc_models::ArgumentName: Borrow<Arg>,
    Arg: Ord + ?Sized + Display,
{
    let result = arguments
        .remove(argument_name)
        .ok_or_else(|| {
            (
                StatusCode::BAD_REQUEST,
                Json(ndc_models::ErrorResponse {
                    message: format!("missing argument {argument_name}"),
                    details: serde_json::Value::Null,
                }),
            )
        })?
        .as_i64()
        .ok_or_else(|| {
            (
                StatusCode::INTERNAL_SERVER_ERROR,
                Json(ndc_models::ErrorResponse {
                    message: format!("argument '{argument_name}' is not an integer"),
                    details: serde_json::Value::Null,
                }),
            )
        })?
        .try_into()
        .map_err(|_| {
            (
                StatusCode::BAD_REQUEST,
                Json(ndc_models::ErrorResponse {
                    message: format!("argument '{argument_name}' is out of range"),
                    details: serde_json::Value::Null,
                }),
            )
        })?;
    Ok(result)
}

pub(crate) fn parse_i64_argument<Arg>(
    argument_name: &Arg,
    arguments: &mut BTreeMap<ndc_models::ArgumentName, &serde_json::Value>,
) -> Result<i64>
where
    ndc_models::ArgumentName: Borrow<Arg>,
    Arg: Ord + ?Sized + Display,
{
    let result = arguments
        .remove(argument_name)
        .ok_or_else(|| {
            (
                StatusCode::BAD_REQUEST,
                Json(ndc_models::ErrorResponse {
                    message: format!("missing argument {argument_name}"),
                    details: serde_json::Value::Null,
                }),
            )
        })?
        .as_i64()
        .ok_or_else(|| {
            (
                StatusCode::INTERNAL_SERVER_ERROR,
                Json(ndc_models::ErrorResponse {
                    message: format!("argument '{argument_name}' is not an integer"),
                    details: serde_json::Value::Null,
                }),
            )
        })?;
    Ok(result)
}

pub(crate) fn parse_string_argument<'a, Arg>(
    argument_name: &Arg,
    arguments: &mut BTreeMap<ndc_models::ArgumentName, &'a serde_json::Value>,
) -> Result<&'a str>
where
    ndc_models::ArgumentName: Borrow<Arg>,
    Arg: Ord + ?Sized + Display,
{
    let result = arguments
        .remove(argument_name)
        .ok_or_else(|| {
            (
                StatusCode::BAD_REQUEST,
                Json(ndc_models::ErrorResponse {
                    message: format!("missing argument {argument_name}"),
                    details: serde_json::Value::Null,
                }),
            )
        })?
        .as_str()
        .ok_or_else(|| {
            (
                StatusCode::INTERNAL_SERVER_ERROR,
                Json(ndc_models::ErrorResponse {
                    message: format!("argument '{argument_name}' is not an integer"),
                    details: serde_json::Value::Null,
                }),
            )
        })?;
    Ok(result)
}

pub(crate) fn parse_expression_argument<Arg>(
    argument_name: &Arg,
    arguments: &mut BTreeMap<ndc_models::ArgumentName, &serde_json::Value>,
) -> Result<ndc_models::Expression>
where
    ndc_models::ArgumentName: Borrow<Arg>,
    Arg: Ord + ?Sized + Display,
{
    let value = arguments.remove(argument_name).ok_or_else(|| {
        (
            StatusCode::BAD_REQUEST,
            Json(ndc_models::ErrorResponse {
                message: format!("missing argument {argument_name}"),
                details: serde_json::Value::Null,
            }),
        )
    })?;

    let decoded_bool_exp: ndc_models::Expression =
        serde_json::from_value(value.clone()).map_err(|_| {
            (
                StatusCode::BAD_REQUEST,
                Json(ndc_models::ErrorResponse {
                    message: format!("could not decode argument {argument_name}"),
                    details: serde_json::Value::Null,
                }),
            )
        })?;

    Ok(decoded_bool_exp)
}

pub(crate) fn check_all_arguments_used<TArgumentName, TValue>(
    arguments: &BTreeMap<TArgumentName, TValue>,
) -> Result<()>
where
    TArgumentName: Borrow<str>,
{
    if arguments.keys().next().is_some() {
        let unexpected_arguments = arguments
            .keys()
            .map(|k| Borrow::<str>::borrow(k).to_owned())
            .collect::<Vec<String>>()
            .join(",");
        Err((
            StatusCode::BAD_REQUEST,
            Json(ndc_models::ErrorResponse {
                message: format!("unexpected arguments found: {unexpected_arguments}"),
                details: serde_json::Value::Null,
            }),
        ))
    } else {
        Ok(())
    }
}
