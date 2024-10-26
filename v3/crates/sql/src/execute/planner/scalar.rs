//! A module for parsing DataFusion literals into JSON values.
//!
//! This module provides functionality to convert DataFusion's `ScalarValue`
//! into `serde_json::Value`, supporting a wide range of data types including
//! primitive types, temporal types, structs, and arrays.
use datafusion::{
    arrow::{
        array::{
            Array, ArrayRef, Date32Array, Date64Array, ListArray, StructArray,
            Time32MillisecondArray, Time32SecondArray, Time64MicrosecondArray,
            Time64NanosecondArray, TimestampMicrosecondArray, TimestampMillisecondArray,
            TimestampNanosecondArray, TimestampSecondArray,
        },
        datatypes::{DataType, Field, Fields},
        util::display::{DurationFormat, FormatOptions},
    },
    error::DataFusionError,
    scalar::ScalarValue,
};
use std::{collections::HashSet, sync::Arc};

/// Parses a datafusion literal into a json value, supports most types (including structs and arrays).
pub(crate) fn parse_datafusion_literal(
    expected_type: &DataType,
    literal: &ScalarValue,
) -> datafusion::error::Result<serde_json::Value> {
    match (expected_type, literal) {
        (DataType::Utf8, ScalarValue::Utf8(Some(s))) => {
            Ok(serde_json::Value::String(s.clone()))
        }
        (DataType::Int32, ScalarValue::Int32(Some(i))) => {
            Ok(serde_json::Value::Number((*i).into()))
        }
        (DataType::Int32, ScalarValue::Int64(Some(i))) => match i32::try_from(*i) {
            Ok(i32_value) => Ok(serde_json::Value::Number(i32_value.into())),
            Err(_) => Err(DataFusionError::Plan(format!(
                "Int64 value {i} is out of range for Int32"
            ))),
        },
        (DataType::Int64, ScalarValue::Int64(Some(i))) => {
            Ok(serde_json::Value::String(i.to_string()))
        }
        #[allow(clippy::cast_precision_loss, clippy::cast_possible_truncation)]
        (DataType::Float32, ScalarValue::Int64(Some(i))) => {
            let f = *i as f32;
            if f.is_finite()  {
                Ok(serde_json::Value::Number(
                    serde_json::Number::from_f64(f64::from(f)).ok_or_else(|| {
                        DataFusionError::Plan(
                            "Unable to convert f32 to JSON number".to_string(),
                        )
                    })?,
                ))
            } else {
                Err(DataFusionError::Plan(
                    format!("Int64 value {i} cannot be finitely represented as Float32")
                ))
            }
        }
        (DataType::Float32, ScalarValue::Float32(Some(f))) => {
            Ok(serde_json::Value::Number(
                serde_json::Number::from_f64(f64::from(*f)).ok_or_else(|| {
                    DataFusionError::Plan(
                        "Unable to convert f32 to JSON number".to_string(),
                    )
                })?,
            ))
        }
        #[allow(clippy::cast_possible_truncation)]
        (DataType::Float32, ScalarValue::Float64(Some(f))) => {
            let f32_value = *f as f32;
            if f32_value.is_finite() {
                Ok(serde_json::Value::Number(
                    serde_json::Number::from_f64(f64::from(f32_value)).ok_or_else(|| {
                        DataFusionError::Plan(
                            "Unable to convert f32 to JSON number".to_string(),
                        )
                    })?,
                ))
            } else {
                Err(DataFusionError::Plan(
                    format!("Float64 value {f} cannot be finitely represented as Float32")
                ))
            }
        }
        #[allow(clippy::cast_precision_loss, clippy::cast_possible_truncation)]
        (DataType::Float64, ScalarValue::Int64(Some(i))) => {
            let f = *i as f64;
            if f.is_finite() {
                Ok(serde_json::Value::String(f.to_string()))
            } else {
                Err(DataFusionError::Plan(
                    format!("Int64 value {i} cannot be finitely represented as Float64")
                ))
            }
        }
        (DataType::Float64, ScalarValue::Float64(Some(f))) => {
            Ok(serde_json::Value::String(f.to_string()))
        }
        (DataType::Boolean, ScalarValue::Boolean(Some(b))) => {
            Ok(serde_json::Value::Bool(*b))
        }
        (DataType::Date32, ScalarValue::Date32(Some(days))) => {
            let arr = Date32Array::from(vec![Some(*days)]);
            format_time_related_types(&(Arc::new(arr) as ArrayRef))
        }
        (DataType::Date64, ScalarValue::Date64(Some(milliseconds))) => {
            let arr = Date64Array::from(vec![Some(*milliseconds)]);
            format_time_related_types(&(Arc::new(arr) as ArrayRef))
        }
        (DataType::Timestamp(_, _), ScalarValue::TimestampNanosecond(Some(ts), tz)) => {
            let arr = TimestampNanosecondArray::from(vec![Some(*ts)]).with_timezone_opt(tz.clone());
            format_time_related_types(&(Arc::new(arr) as ArrayRef))
        }
        (DataType::Timestamp(_, _), ScalarValue::TimestampMicrosecond(Some(ts), tz)) => {
            let arr = TimestampMicrosecondArray::from(vec![Some(*ts)]).with_timezone_opt(tz.clone());
            format_time_related_types(&(Arc::new(arr) as ArrayRef))
        }
        (DataType::Timestamp(_, _), ScalarValue::TimestampMillisecond(Some(ts), tz)) => {
            let arr = TimestampMillisecondArray::from(vec![Some(*ts)]).with_timezone_opt(tz.clone());
            format_time_related_types(&(Arc::new(arr) as ArrayRef))
        }
        (DataType::Timestamp(_, _), ScalarValue::TimestampSecond(Some(ts), tz)) => {
            let arr = TimestampSecondArray::from(vec![Some(*ts)]).with_timezone_opt(tz.clone());
            format_time_related_types(&(Arc::new(arr) as ArrayRef))
        }
        (DataType::Time32(_), ScalarValue::Time32Second(Some(seconds))) => {
            let arr = Time32SecondArray::from(vec![Some(*seconds)]);
            format_time_related_types(&(Arc::new(arr) as ArrayRef))
        }
        (DataType::Time32(_), ScalarValue::Time32Millisecond(Some(milliseconds))) => {
            let arr = Time32MillisecondArray::from(vec![Some(*milliseconds)]);
            format_time_related_types(&(Arc::new(arr) as ArrayRef))
        }
        (DataType::Time64(_), ScalarValue::Time64Microsecond(Some(microseconds))) => {
            let arr = Time64MicrosecondArray::from(vec![Some(*microseconds)]);
            format_time_related_types(&(Arc::new(arr) as ArrayRef))
        }
        (DataType::Time64(_), ScalarValue::Time64Nanosecond(Some(nanoseconds))) => {
            let arr = Time64NanosecondArray::from(vec![Some(*nanoseconds)]);
            format_time_related_types(&(Arc::new(arr) as ArrayRef))
        }
        (DataType::Struct(fields), ScalarValue::Struct(struct_array)) => {
            parse_struct_literal(fields, struct_array).map(serde_json::Value::Object)
        }
        (DataType::List(field), ScalarValue::List(list_array)) => {
            parse_array_literal(field, list_array)
        }
        _ => Err(DataFusionError::Plan(format!(
            "Unsupported or mismatched literal type: {literal:?} for expected type: {expected_type:?}",
        ))),
    }
}

/// Parses an array
fn parse_array_literal(
    expected_field: &Field,
    list_array: &Arc<ListArray>,
) -> datafusion::error::Result<serde_json::Value> {
    let values = list_array.values();
    let mut result = Vec::new();
    for i in 0..values.len() {
        if values.is_null(i) {
            result.push(serde_json::Value::Null);
        } else {
            let values = values.slice(i, 1);
            let scalar_value = ScalarValue::try_from_array(&values, 0)?;
            let parsed_value = parse_datafusion_literal(expected_field.data_type(), &scalar_value)?;
            result.push(parsed_value);
        }
    }

    Ok(serde_json::Value::Array(result))
}

/// Parses a struct, both named and unnamed variants
pub(crate) fn parse_struct_literal(
    expected_fields: &Fields,
    // there is no representation for a single struct value, it is encoded as a struct array with
    // one element
    struct_array: &Arc<StructArray>,
) -> datafusion::error::Result<serde_json::Map<String, serde_json::Value>> {
    let mut map = serde_json::Map::new();

    let is_unnamed_struct = struct_array
        .fields()
        .iter()
        .all(|field| field.name().starts_with('c') && field.name()[1..].parse::<usize>().is_ok());

    if is_unnamed_struct {
        if expected_fields.len() != struct_array.num_columns() {
            return Err(DataFusionError::Plan(format!(
                "Mismatch between expected fields ({}) and actual fields ({}) in StructArray",
                expected_fields.len(),
                struct_array.num_columns()
            )));
        }
    } else {
        // Create a map of expected fields
        let expected_field_set: HashSet<_> =
            expected_fields.iter().map(|field| field.name()).collect();

        // Check for extra fields in the struct that aren't expected
        let extra_fields: Vec<_> = struct_array
            .fields()
            .iter()
            .filter(|field| !expected_field_set.contains(field.name()))
            .collect();

        if !extra_fields.is_empty() {
            return Err(DataFusionError::Plan(format!(
                "Unexpected fields found in struct: {:?}, allowed: {:?}",
                extra_fields.iter().map(|f| f.name()).collect::<Vec<_>>(),
                expected_field_set
            )));
        }
    }

    let field_name_to_index: std::collections::HashMap<_, _> = struct_array
        .fields()
        .iter()
        .enumerate()
        .map(|(i, field)| (field.name(), i))
        .collect();

    for (i, expected_field) in expected_fields.iter().enumerate() {
        let field_name = expected_field.name();

        let column_index = if is_unnamed_struct {
            Some(i)
        } else {
            field_name_to_index.get(field_name).copied()
        };

        match column_index {
            Some(index) if index < struct_array.num_columns() => {
                let column = &struct_array.columns()[index];
                let field_type = expected_field.data_type();

                if column.len() == 0 {
                    return Err(DataFusionError::Plan(
                        "Empty column array of struct".to_string(),
                    ));
                }

                // Assuming we're always dealing with the first row of the column array
                let scalar_value = ScalarValue::try_from_array(column, 0)?;

                // Skip nullable fields if they are null
                if scalar_value.is_null() && expected_field.is_nullable() {
                    continue;
                }

                let parsed_value = parse_datafusion_literal(field_type, &scalar_value)?;
                map.insert(field_name.to_string(), parsed_value);
            }
            _ if expected_field.is_nullable() => {
                // Field is nullable and not present in the actual struct, skip it
                continue;
            }
            _ => {
                return Err(DataFusionError::Plan(format!(
                    "Expected non-nullable field '{field_name}' not found in struct",
                )));
            }
        }
    }
    Ok(map)
}

/// Uses datafusion's bulit-in functions to format timestamp related types into IS8601 format
fn format_time_related_types(arr: &ArrayRef) -> datafusion::error::Result<serde_json::Value> {
    let format_options = FormatOptions::new().with_duration_format(DurationFormat::ISO8601);
    let formatter =
        ::datafusion::arrow::util::display::ArrayFormatter::try_new(arr, &format_options)?;
    let result = formatter.value(0).try_to_string()?;
    Ok(serde_json::Value::String(result))
}

#[cfg(test)]
mod tests {
    use super::*;
    use ::datafusion::arrow::{
        array::{Int32Array, StringArray},
        datatypes::{Int32Type, TimeUnit},
    };
    use chrono::{NaiveDate, NaiveDateTime, NaiveTime};

    #[test]
    fn test_parse_utf8() {
        let result = parse_datafusion_literal(
            &DataType::Utf8,
            &ScalarValue::Utf8(Some("hello".to_string())),
        )
        .unwrap();
        assert_eq!(result, serde_json::Value::String("hello".to_string()));
    }

    #[test]
    fn test_parse_int32() {
        let result =
            parse_datafusion_literal(&DataType::Int32, &ScalarValue::Int32(Some(42))).unwrap();
        assert_eq!(result, serde_json::Value::Number(42.into()));
    }

    #[test]
    fn test_parse_int64_as_int32() {
        let result =
            parse_datafusion_literal(&DataType::Int32, &ScalarValue::Int64(Some(42))).unwrap();
        assert_eq!(result, serde_json::Value::Number(42.into()));

        let result =
            parse_datafusion_literal(&DataType::Int32, &ScalarValue::Int64(Some(3_000_000_000)));
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_int64() {
        let result = parse_datafusion_literal(
            &DataType::Int64,
            &ScalarValue::Int64(Some(9223372036854775807)),
        )
        .unwrap();
        assert_eq!(
            result,
            serde_json::Value::String("9223372036854775807".to_string())
        );
    }

    #[test]
    fn test_parse_float64() {
        let result = parse_datafusion_literal(
            &DataType::Float64,
            &ScalarValue::Float64(Some(std::f64::consts::PI)),
        )
        .unwrap();
        assert_eq!(
            result,
            serde_json::Value::String("3.141592653589793".to_string())
        );
    }

    #[test]
    fn test_parse_float64_from_int64() {
        let result =
            parse_datafusion_literal(&DataType::Float64, &ScalarValue::Int64(Some(2))).unwrap();
        assert_eq!(result, serde_json::Value::String("2".to_string()));
    }

    #[test]
    fn test_parse_float32() {
        let result =
            parse_datafusion_literal(&DataType::Float32, &ScalarValue::Float64(Some(2.99)))
                .unwrap();
        assert!(result.is_number());
    }

    #[test]
    fn test_parse_float32_from_int64() {
        let result =
            parse_datafusion_literal(&DataType::Float32, &ScalarValue::Int64(Some(2))).unwrap();
        assert!(result.is_number());
    }

    #[test]
    fn test_parse_boolean() {
        let result =
            parse_datafusion_literal(&DataType::Boolean, &ScalarValue::Boolean(Some(true)))
                .unwrap();
        assert_eq!(result, serde_json::Value::Bool(true));
    }

    #[test]
    fn test_parse_date32() {
        let result = parse_datafusion_literal(
            &DataType::Date32,
            &ScalarValue::Date32(Some(18843)), // 2021-08-04
        )
        .unwrap();
        assert_eq!(result, serde_json::Value::String("2021-08-04".to_string()));
    }

    #[test]
    fn test_parse_date64() {
        let result = parse_datafusion_literal(
            &DataType::Date64,
            &ScalarValue::Date64(Some(1628035200000)), // 2021-08-04
        )
        .unwrap();
        assert_eq!(
            result,
            serde_json::Value::String("2021-08-04T00:00:00".to_string())
        );
    }

    #[test]
    fn test_parse_timestamp() {
        let dt = NaiveDateTime::new(
            NaiveDate::from_ymd_opt(2021, 8, 4).unwrap(),
            NaiveTime::from_hms_opt(12, 0, 0).unwrap(),
        );
        let timestamp = dt.and_utc().timestamp_nanos_opt();

        let result = parse_datafusion_literal(
            &DataType::Timestamp(TimeUnit::Nanosecond, None),
            &ScalarValue::TimestampNanosecond(timestamp, None),
        )
        .unwrap();
        assert_eq!(
            result,
            serde_json::Value::String("2021-08-04T12:00:00".to_string())
        );
    }

    #[test]
    fn test_parse_time32() {
        let result = parse_datafusion_literal(
            &DataType::Time32(TimeUnit::Second),
            &ScalarValue::Time32Second(Some(43200)), // 12:00:00
        )
        .unwrap();
        assert_eq!(result, serde_json::Value::String("12:00:00".to_string()));
    }

    #[test]
    fn test_parse_time64() {
        let result = parse_datafusion_literal(
            &DataType::Time64(TimeUnit::Microsecond),
            &ScalarValue::Time64Microsecond(Some(43200000000)), // 12:00:00
        )
        .unwrap();
        assert_eq!(result, serde_json::Value::String("12:00:00".to_string()));
    }

    #[test]
    fn test_parse_struct() {
        let fields = Fields::from(vec![
            Field::new("name", DataType::Utf8, false),
            Field::new("age", DataType::Int32, false),
        ]);

        // one row arrays for each field
        let arrays: Vec<ArrayRef> = vec![
            Arc::new(StringArray::from(vec!["Alice"])),
            Arc::new(Int32Array::from(vec![30])),
        ];
        let struct_array = Arc::new(StructArray::new(fields.clone(), arrays, None));

        let result = parse_datafusion_literal(
            &DataType::Struct(fields),
            &ScalarValue::Struct(struct_array),
        )
        .unwrap();

        let expected = serde_json::json!({
            "name": "Alice",
            "age": 30
        });
        assert_eq!(result, expected);
    }

    #[test]
    fn test_parse_named_struct() {
        let expected_fields = Fields::from(vec![
            Field::new("name", DataType::Utf8, false),
            Field::new("age", DataType::Int32, false),
            Field::new("city", DataType::Utf8, true),
        ]);

        let actual_fields = Fields::from(vec![
            Field::new("age", DataType::Int32, false),
            Field::new("city", DataType::Utf8, true),
            Field::new("name", DataType::Utf8, false),
        ]);

        let arrays: Vec<ArrayRef> = vec![
            Arc::new(Int32Array::from(vec![30])),
            Arc::new(StringArray::from(vec![Some("New York")])),
            Arc::new(StringArray::from(vec!["Alice"])),
        ];
        let struct_array = Arc::new(StructArray::new(actual_fields, arrays, None));

        let result = parse_datafusion_literal(
            &DataType::Struct(expected_fields),
            &ScalarValue::Struct(struct_array),
        )
        .unwrap();

        let expected = serde_json::json!({
            "name": "Alice",
            "age": 30,
            "city": "New York"
        });
        assert_eq!(result, expected);
    }

    #[test]
    fn test_parse_struct_with_null_fields() {
        let expected_fields = Fields::from(vec![
            Field::new("name", DataType::Utf8, false),
            Field::new("age", DataType::Int32, true),
            Field::new("city", DataType::Utf8, true),
        ]);

        let actual_fields = Fields::from(vec![
            Field::new("name", DataType::Utf8, false),
            Field::new("city", DataType::Utf8, true),
        ]);

        let arrays: Vec<ArrayRef> = vec![
            Arc::new(StringArray::from(vec!["Charlie"])),
            Arc::new(StringArray::from(vec![Some("Paris")])),
        ];
        let struct_array = Arc::new(StructArray::new(actual_fields, arrays, None));

        let result = parse_datafusion_literal(
            &DataType::Struct(expected_fields),
            &ScalarValue::Struct(struct_array),
        )
        .unwrap();

        let expected = serde_json::json!({
            "name": "Charlie",
            "city": "Paris"
        });
        assert_eq!(result, expected);
    }

    #[test]
    fn test_parse_unnamed_struct() {
        let expected_fields = Fields::from(vec![
            Field::new("name", DataType::Utf8, false),
            Field::new("age", DataType::Int32, false),
            Field::new("city", DataType::Utf8, true),
        ]);

        let actual_fields = Fields::from(vec![
            Field::new("c0", DataType::Utf8, false),
            Field::new("c1", DataType::Int32, false),
            Field::new("c2", DataType::Utf8, true),
        ]);

        let arrays: Vec<ArrayRef> = vec![
            Arc::new(StringArray::from(vec!["Bob"])),
            Arc::new(Int32Array::from(vec![25])),
            Arc::new(StringArray::from(vec![Some("London")])),
        ];
        let struct_array = Arc::new(StructArray::new(actual_fields, arrays, None));

        let result = parse_datafusion_literal(
            &DataType::Struct(expected_fields),
            &ScalarValue::Struct(struct_array),
        )
        .unwrap();

        let expected = serde_json::json!({
            "name": "Bob",
            "age": 25,
            "city": "London"
        });
        assert_eq!(result, expected);
    }

    #[test]
    fn test_parse_list() {
        let field = Field::new("item", DataType::Int32, true);
        let list_array = Arc::new(ListArray::from_iter_primitive::<Int32Type, _, _>(vec![
            Some(vec![Some(1), Some(2), Some(3)]),
        ]));

        let result = parse_datafusion_literal(
            &DataType::List(Arc::new(field)),
            &ScalarValue::List(list_array),
        )
        .unwrap();

        let expected = serde_json::json!([1, 2, 3]);
        assert_eq!(result, expected);
    }
}
