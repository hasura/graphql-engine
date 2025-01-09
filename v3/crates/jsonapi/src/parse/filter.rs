use crate::catalog::Model;
use indexmap::IndexMap;
use metadata_resolve::Qualified;
use open_dds::query::{BooleanExpression, ObjectFieldOperand, ObjectFieldTarget, Operand, Value};
use open_dds::{identifier::Identifier, models::ModelName, types::FieldName};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq)]
enum JsonApiFilter {
    And {
        and: Vec<JsonApiFilter>,
    },
    Or {
        or: Vec<JsonApiFilter>,
    },
    Nested {
        field_name: String,
        rest: Box<JsonApiFilter>,
    },
    Comparison {
        field_name: String,
        operator: String,
        comparison_value: serde_json::Value,
    },
}

#[derive(Debug, derive_more::Display, Serialize, Deserialize)]
pub enum FilterError {
    NoBooleanExpressionDefined(Qualified<ModelName>),
}

pub fn build_boolean_expression(
    model: &Model,
    filter: &serde_json::Value,
) -> Result<open_dds::query::BooleanExpression, FilterError> {
    // filters in jsonapi-rust work like
    // `filter={"name":{"$eq":"Horse"},"age":{"$gt":100}}`
    // with multiple expressions that we should && together

    if model.filter_expression_type.is_some() {
        // only include a filter if the model has a `BooleanExpressionType`
        let parsed_filter = parse_filter_value(filter).unwrap();

        Ok(expression_from_jsonapi_filter(&parsed_filter).unwrap())
    } else {
        Err(FilterError::NoBooleanExpressionDefined(model.name.clone()))
    }
}

// not sure if we should be validating boolean expression against the BooleanExpressionType here
// or whether we expect that to happen in planning
fn expression_from_jsonapi_filter(
    filter: &JsonApiFilter,
) -> Result<open_dds::query::BooleanExpression, ()> {
    match filter {
        JsonApiFilter::Comparison {
            field_name,
            comparison_value,
            operator,
        } => {
            let field_name = FieldName::new(Identifier::new(field_name).unwrap());

            let field_operand = Operand::Field(ObjectFieldOperand {
                target: Box::new(ObjectFieldTarget {
                    field_name,
                    arguments: IndexMap::new(),
                }),
                nested: None,
            });

            let operator = open_dds::types::OperatorName::new(operator.into());

            let comparison = BooleanExpression::Comparison {
                operand: field_operand,
                argument: Box::new(Value::Literal(comparison_value.clone())),
                operator: open_dds::query::ComparisonOperator::Custom(operator),
            };

            Ok(comparison)
        }
        JsonApiFilter::Or { or } => {
            let mut expressions = vec![];
            for item in or {
                let expression = expression_from_jsonapi_filter(item)?;
                expressions.push(expression);
            }
            Ok(open_dds::query::BooleanExpression::Or(expressions))
        }
        JsonApiFilter::And { and } => {
            let mut expressions = vec![];
            for item in and {
                let expression = expression_from_jsonapi_filter(item)?;
                expressions.push(expression);
            }
            Ok(open_dds::query::BooleanExpression::And(expressions))
        }
        JsonApiFilter::Nested { .. } => todo!("Nested fields not supported yet"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;
    #[test]
    fn test_parse_filter_value() {
        let tests = vec![
            (
                json!({"name":{"$eq":"Horse"}}),
                JsonApiFilter::Comparison {
                    comparison_value: serde_json::Value::from("Horse"),
                    field_name: "name".to_string(),
                    operator: "eq".to_string(),
                },
            ),
            (
                json!({"name":{"$eq":"Horse"},"age":{"$eq":1}}),
                JsonApiFilter::And {
                    and: vec![
                        JsonApiFilter::Comparison {
                            comparison_value: serde_json::Value::from("Horse"),
                            field_name: "name".to_string(),
                            operator: "eq".to_string(),
                        },
                        JsonApiFilter::Comparison {
                            comparison_value: serde_json::Value::from(1),
                            field_name: "age".to_string(),
                            operator: "eq".to_string(),
                        },
                    ],
                },
            ),
            (
                json!({"user":{"name":{"$eq":1}}}),
                JsonApiFilter::Nested {
                    field_name: "user".to_string(),
                    rest: Box::new(JsonApiFilter::Comparison {
                        comparison_value: serde_json::Value::from(1),
                        field_name: "name".to_string(),
                        operator: "eq".to_string(),
                    }),
                },
            ),
        ];

        for (input, expected) in tests {
            assert_eq!(parse_filter_value(&input), Ok(expected));
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum ParseFilterExpressionError {
    FilterExpressionIsNotObject,
    MultipleComparisonsInOneObject,
    ExpectedNonEmptyObjectForFieldComparison,
    ExpectedArrayForAnd,
    ExpectedArrayForOr,
}

/* Parse our filter shape from JSON value
 *
 * Example filter as query string - /movies?sort=...&filter=<json>
{
  "$or": [
    {
      "name": {
        "$eq": "Braveheart"
      }
    },
    {
      "name": {
        "$eq": "Gladiator"
      }
    },
    {
      "$and": [
        {
          "director": {
            "last_name": {
              "$eq": "Scorcese"
            }
          }
        },
        {
          "director": {
            "age": {
              "$gt": 50
            }
          }
        }
      ]
    }
  ],
  "name": {
    "$eq": "Foobar"
  }
}
*/
fn parse_filter_value(
    value: &serde_json::Value,
) -> Result<JsonApiFilter, ParseFilterExpressionError> {
    match value.as_object() {
        Some(items) => {
            let mut jsonapi_filters = vec![];
            // assume only one item for now
            for (key, value) in items {
                let jsonapi_filter = match key.as_str() {
                    // `$and` and `$or` will need to be configurable in future
                    "$and" => {
                        // fail if items aren't an array
                        let item_array = value
                            .as_array()
                            .ok_or(ParseFilterExpressionError::ExpectedArrayForAnd)?;

                        // parse array of things
                        let filter_array = item_array
                            .iter()
                            .map(parse_filter_value)
                            .collect::<Result<Vec<_>, _>>()?;
                        Ok(JsonApiFilter::And { and: filter_array })
                    }
                    "$or" => {
                        // fail if items aren't an array
                        let item_array = value
                            .as_array()
                            .ok_or(ParseFilterExpressionError::ExpectedArrayForOr)?;

                        // parse array of things
                        let filter_array = item_array
                            .iter()
                            .map(parse_filter_value)
                            .collect::<Result<Vec<_>, _>>()?;
                        Ok(JsonApiFilter::Or { or: filter_array })
                    }
                    field_name => {
                        // if it's not a known operator, assume it's a field name
                        let item_object = value.as_object().ok_or({
                            ParseFilterExpressionError::ExpectedNonEmptyObjectForFieldComparison
                        })?;

                        if item_object.len() > 1 {
                            return Err(ParseFilterExpressionError::MultipleComparisonsInOneObject);
                        }
                        if let Some((operator, comparison_value)) = item_object.iter().next() {
                            match parse_operator(operator.clone()) {
                                OperatorOrNested::Operator(parsed_operator) => {
                                    Ok(JsonApiFilter::Comparison {
                                        field_name: field_name.to_string(),
                                        operator: parsed_operator,
                                        comparison_value: comparison_value.clone(),
                                    })
                                }
                                OperatorOrNested::Nested(_nested_field_name) => {
                                    Ok(JsonApiFilter::Nested {
                                        field_name: field_name.to_string(),
                                        rest: Box::new(parse_filter_value(value)?),
                                    })
                                }
                            }
                        } else {
                            Err(ParseFilterExpressionError::MultipleComparisonsInOneObject)
                        }
                    }
                }?;
                jsonapi_filters.push(jsonapi_filter);
            }

            // if there is only one filter, skip the AND wrapper
            if jsonapi_filters.len() == 1 {
                if let Some(jsonapi_filter) = jsonapi_filters.first() {
                    return Ok(jsonapi_filter.clone());
                }
            }
            Ok(JsonApiFilter::And {
                and: jsonapi_filters,
            })
        }
        None => Err(ParseFilterExpressionError::FilterExpressionIsNotObject),
    }
}

enum OperatorOrNested {
    Operator(String),
    Nested(String),
}

// we're either looking at an operator, ie '$eq`, or the name of another nested field
// only return if it's an operator, and get rid of the `$`
fn parse_operator(operator: String) -> OperatorOrNested {
    match operator.strip_prefix("$") {
        Some(op) => OperatorOrNested::Operator(op.to_string()),
        None => OperatorOrNested::Nested(operator),
    }
}
