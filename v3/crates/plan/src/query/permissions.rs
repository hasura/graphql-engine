use authorization_rules::ArgumentPolicy;
use hasura_authn_core::{SessionVariableName, SessionVariableValue, SessionVariables};
use std::collections::BTreeMap;

use super::arguments::{UnresolvedArgument, map_field_names_to_ndc_field_names};
use crate::ArgumentPresetExecutionError;
use crate::error::{InternalDeveloperError, InternalEngineError, InternalError};
use crate::types::PlanError;
use metadata_resolve::{
    ObjectTypeWithRelationships, Qualified, QualifiedBaseType, QualifiedTypeName,
    QualifiedTypeReference, TypeMapping, UnaryComparisonOperator, ValueExpression,
};
use open_dds::{
    data_connector::{DataConnectorColumnName, DataConnectorOperatorName},
    types::{CustomTypeName, InbuiltType},
};
use plan_types::{
    ComparisonTarget, ComparisonValue, Expression, LocalFieldComparison, UsagesCounts,
};

pub fn process_permissions<'s>(
    data_connector_link: &'s metadata_resolve::DataConnectorLink,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    permissions: &Vec<&'s metadata_resolve::ModelPredicate>,
    session_variables: &SessionVariables,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
    usage_counts: &mut UsagesCounts,
) -> Result<Expression<'s>, PlanError> {
    Ok(Expression::And {
        expressions: permissions
            .iter()
            .map(|permission| {
                process_model_predicate(
                    data_connector_link,
                    type_mappings,
                    permission,
                    session_variables,
                    object_types,
                    usage_counts,
                )
            })
            .collect::<Result<Vec<_>, PlanError>>()?,
    })
}

pub fn process_model_predicate<'s>(
    data_connector_link: &'s metadata_resolve::DataConnectorLink,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    model_predicate: &'s metadata_resolve::ModelPredicate,
    session_variables: &SessionVariables,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
    usage_counts: &mut UsagesCounts,
) -> Result<Expression<'s>, PlanError> {
    match model_predicate {
        metadata_resolve::ModelPredicate::UnaryFieldComparison {
            ndc_column,
            operator,
            column_path,
            ..
        } => Ok(make_permission_unary_boolean_expression(
            ndc_column,
            *operator,
            column_path.iter().collect::<Vec<_>>().as_slice(),
        )),
        metadata_resolve::ModelPredicate::BinaryFieldComparison {
            ndc_column,
            argument_type,
            operator,
            value,
            column_path,
            ..
        } => Ok(make_permission_binary_boolean_expression(
            ndc_column,
            argument_type,
            &operator.data_connector_operator_name,
            value,
            column_path.iter().collect::<Vec<_>>().as_slice(),
            session_variables,
            type_mappings,
            object_types,
        )?),
        metadata_resolve::ModelPredicate::Not(predicate) => {
            let expr = process_model_predicate(
                data_connector_link,
                type_mappings,
                predicate,
                session_variables,
                object_types,
                usage_counts,
            )?;
            Ok(Expression::Not {
                expression: Box::new(expr),
            })
        }
        metadata_resolve::ModelPredicate::And(predicates) => {
            let exprs = predicates
                .iter()
                .map(|predicate| {
                    process_model_predicate(
                        data_connector_link,
                        type_mappings,
                        predicate,
                        session_variables,
                        object_types,
                        usage_counts,
                    )
                })
                .collect::<Result<Vec<_>, PlanError>>()?;
            Ok(Expression::And { expressions: exprs })
        }
        metadata_resolve::ModelPredicate::Or(predicates) => {
            let exprs = predicates
                .iter()
                .map(|predicate| {
                    process_model_predicate(
                        data_connector_link,
                        type_mappings,
                        predicate,
                        session_variables,
                        object_types,
                        usage_counts,
                    )
                })
                .collect::<Result<Vec<_>, PlanError>>()?;
            Ok(Expression::Or { expressions: exprs })
        }
        metadata_resolve::ModelPredicate::Relationship {
            relationship_info,
            predicate,
            column_path,
        } => {
            // Add the target model being used in the usage counts
            crate::model_tracking::count_model(&relationship_info.target_model_name, usage_counts);

            let relationship_predicate = process_model_predicate(
                &relationship_info.target_source.model.data_connector,
                &relationship_info.target_source.model.type_mappings,
                predicate,
                session_variables,
                object_types,
                usage_counts,
            )?;

            // build and return relationshp comparison expression
            Ok(super::filter::build_relationship_comparison_expression(
                type_mappings,
                column_path.clone(),
                data_connector_link,
                &relationship_info.relationship_name,
                &relationship_info.relationship_type,
                &relationship_info.source_type,
                &relationship_info.target_model_name,
                &relationship_info.target_source.model,
                &relationship_info.target_source.capabilities,
                &relationship_info.mappings,
                relationship_predicate,
            )?)
        }
    }
}

fn make_permission_binary_boolean_expression<'s>(
    ndc_column: &DataConnectorColumnName,
    argument_type: &QualifiedTypeReference,
    operator: &DataConnectorOperatorName,
    value_expression: &'s metadata_resolve::ValueExpression,
    column_path: &[&DataConnectorColumnName],
    session_variables: &SessionVariables,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
) -> Result<Expression<'s>, PlanError> {
    let (name, field_path) = crate::filter::with_nesting_path(ndc_column, column_path);

    let ndc_expression_value = make_argument_from_value_expression(
        value_expression,
        argument_type,
        session_variables,
        type_mappings,
        object_types,
    )?;
    Ok(Expression::LocalField(
        LocalFieldComparison::BinaryComparison {
            column: ComparisonTarget::Column { name, field_path },
            operator: operator.clone(),
            value: ComparisonValue::Scalar {
                value: ndc_expression_value,
            },
        },
    ))
}

fn make_permission_unary_boolean_expression<'s>(
    ndc_column: &DataConnectorColumnName,
    operator: UnaryComparisonOperator,
    column_path: &[&DataConnectorColumnName],
) -> Expression<'s> {
    let (name, field_path) = crate::filter::with_nesting_path(ndc_column, column_path);

    Expression::LocalField(LocalFieldComparison::UnaryComparison {
        column: ComparisonTarget::Column { name, field_path },
        operator,
    })
}

pub fn make_argument_from_value_expression(
    val_expr: &metadata_resolve::ValueExpression,
    value_type: &QualifiedTypeReference,
    session_variables: &SessionVariables,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
) -> Result<serde_json::Value, PlanError> {
    match val_expr {
        metadata_resolve::ValueExpression::Literal(val) => Ok(val.clone()),
        metadata_resolve::ValueExpression::SessionVariable(session_var) => {
            let value = session_variables
                .get(&session_var.name)
                .ok_or_else(|| InternalDeveloperError::MissingSessionVariable {
                    session_variable: session_var.name.clone(),
                })
                .map_err(|e| PlanError::InternalError(InternalError::Developer(e)))?;

            typecast_session_variable(
                &session_var.name,
                value,
                session_var.passed_as_json,
                session_var.disallow_unknown_fields,
                value_type,
                type_mappings,
                object_types,
            )
            .map_err(PlanError::InternalError)
        }
    }
}

pub(crate) fn make_argument_from_value_expression_or_predicate<'s>(
    data_connector_link: &'s metadata_resolve::DataConnectorLink,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    val_expr: &'s ArgumentPolicy<'s>,
    session_variables: &SessionVariables,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
    usage_counts: &mut UsagesCounts,
) -> Result<UnresolvedArgument<'s>, PlanError> {
    match val_expr {
        ArgumentPolicy::ValueExpression {
            argument_type,
            value_expression: ValueExpression::Literal(val),
        } => {
            let mut value = val.clone();

            map_field_names_to_ndc_field_names(
                &mut value,
                argument_type,
                type_mappings,
                object_types,
                false, // we have already statically validated values so don't need runtime
                       // checking
            )
            .map_err(ArgumentPresetExecutionError::MapFieldNamesError)?;

            Ok(UnresolvedArgument::Literal { value })
        }
        ArgumentPolicy::ValueExpression {
            argument_type,
            value_expression: ValueExpression::SessionVariable(session_var),
        } => {
            let value = session_variables
                .get(&session_var.name)
                .ok_or_else(|| InternalDeveloperError::MissingSessionVariable {
                    session_variable: session_var.name.clone(),
                })
                .map_err(|e| PlanError::InternalError(InternalError::Developer(e)))?;

            Ok(UnresolvedArgument::Literal {
                value: typecast_session_variable(
                    &session_var.name,
                    value,
                    session_var.passed_as_json,
                    session_var.disallow_unknown_fields,
                    argument_type,
                    type_mappings,
                    object_types,
                )
                .map_err(PlanError::InternalError)?,
            })
        }
        ArgumentPolicy::BooleanExpression { predicates } => {
            let filter_expression = process_permissions(
                data_connector_link,
                type_mappings,
                predicates,
                session_variables,
                object_types,
                usage_counts,
            )?;
            Ok(UnresolvedArgument::BooleanExpression {
                predicate: filter_expression,
            })
        }
    }
}

/// Typecast a stringified session variable into a given type, but as a serde_json::Value
fn typecast_session_variable(
    session_var_name: &SessionVariableName,
    session_var_value_wrapped: &SessionVariableValue,
    passed_as_json: bool,
    disallow_unknown_fields: bool,
    to_type: &QualifiedTypeReference,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
) -> Result<serde_json::Value, InternalError> {
    if passed_as_json {
        typecast_session_variable_v2(
            session_var_name,
            session_var_value_wrapped,
            to_type,
            type_mappings,
            object_types,
            disallow_unknown_fields,
        )
    } else {
        typecast_session_variable_v1(session_var_name, session_var_value_wrapped, to_type)
    }
}

fn typecast_session_variable_v1(
    session_var_name: &SessionVariableName,
    session_var_value_wrapped: &SessionVariableValue,
    to_type: &QualifiedTypeReference,
) -> Result<serde_json::Value, InternalError> {
    // In v1 (ie before json type support in session variables), we expect every session
    // variable to arrive as a string and then we parse that string into whatever type we need
    let session_var_value = &session_var_value_wrapped
        .as_str()
        .ok_or(InternalDeveloperError::VariableJsonNotSupported {
            session_variable: session_var_name.clone(),
        })?
        .to_string();
    match &to_type.underlying_type {
        QualifiedBaseType::Named(type_name) => {
            match type_name {
                QualifiedTypeName::Inbuilt(primitive) => match primitive {
                    InbuiltType::Int => {
                        let value: i32 = session_var_value.parse().map_err(|_| {
                            InternalDeveloperError::VariableTypeCast {
                                session_variable: session_var_name.clone(),
                                expected: "int".into(),
                                found: session_var_value.clone(),
                            }
                        })?;
                        Ok(serde_json::Value::Number(value.into()))
                    }
                    InbuiltType::Float => {
                        let value: f32 = session_var_value.parse().map_err(|_| {
                            InternalDeveloperError::VariableTypeCast {
                                session_variable: session_var_name.clone(),
                                expected: "float".into(),
                                found: session_var_value.clone(),
                            }
                        })?;
                        Ok(serde_json::to_value(value).map_err(|e| {
                            InternalError::Engine(InternalEngineError::InternalGeneric {
                                description: e.to_string(),
                            })
                        })?)
                    }
                    InbuiltType::Boolean => match session_var_value.as_str() {
                        "true" => Ok(serde_json::Value::Bool(true)),
                        "false" => Ok(serde_json::Value::Bool(false)),
                        _ => Err(InternalDeveloperError::VariableTypeCast {
                            session_variable: session_var_name.clone(),
                            expected: "true or false".into(),
                            found: session_var_value.clone(),
                        })?,
                    },
                    InbuiltType::String => {
                        Ok(serde_json::to_value(session_var_value).map_err(|e| {
                            InternalError::Engine(InternalEngineError::InternalGeneric {
                                description: e.to_string(),
                            })
                        })?)
                    }
                    InbuiltType::ID => {
                        Ok(serde_json::to_value(session_var_value).map_err(|e| {
                            InternalError::Engine(InternalEngineError::InternalGeneric {
                                description: e.to_string(),
                            })
                        })?)
                    }
                },
                // TODO: Currently, we don't support `representation` for `NewTypes`, so custom_type is a blackbox with only
                // name present. We need to add support for `representation` and so we can use it to typecast custom new_types
                // based on the definition in their representation.
                QualifiedTypeName::Custom(_type_name) => {
                    let value_result = serde_json::from_str(session_var_value);
                    match value_result {
                        Ok(value) => Ok(value),
                        // If the session variable value doesn't match any JSON value, we treat the variable as a string (e.g.
                        // session variables of string type aren't typically quoted)
                        Err(_) => Ok(serde_json::to_value(session_var_value).map_err(|e| {
                            InternalError::Engine(InternalEngineError::InternalGeneric {
                                description: e.to_string(),
                            })
                        })?),
                    }
                }
            }
        }
        QualifiedBaseType::List(_) => {
            Err(InternalDeveloperError::VariableArrayTypeCastNotSupported {
                session_variable: session_var_name.clone(),
            })?
        }
    }
}

fn typecast_session_variable_v2(
    session_var_name: &SessionVariableName,
    session_var_value: &SessionVariableValue,
    to_type: &QualifiedTypeReference,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
    disallow_unknown_fields: bool,
) -> Result<serde_json::Value, InternalError> {
    match &to_type.underlying_type {
        QualifiedBaseType::Named(type_name) => match type_name {
            QualifiedTypeName::Inbuilt(primitive) => match primitive {
                InbuiltType::Int => {
                    let value: i64 = session_var_value.as_i64().ok_or_else(|| {
                        InternalDeveloperError::VariableTypeCast {
                            session_variable: session_var_name.clone(),
                            expected: "int".into(),
                            found: session_var_value.to_string(),
                        }
                    })?;
                    Ok(serde_json::Value::Number(value.into()))
                }
                InbuiltType::Float => {
                    let value: f64 = session_var_value.as_f64().ok_or_else(|| {
                        InternalDeveloperError::VariableTypeCast {
                            session_variable: session_var_name.clone(),
                            expected: "float".into(),
                            found: session_var_value.to_string(),
                        }
                    })?;
                    serde_json::to_value(value).map_err(|_error| {
                        // f64 may not cleanly go into JSON numbers if the value is a NaN or infinite
                        InternalDeveloperError::VariableTypeCast {
                            session_variable: session_var_name.clone(),
                            expected: "float".into(),
                            found: value.to_string(),
                        }
                        .into()
                    })
                }
                InbuiltType::Boolean => {
                    let value: bool = session_var_value.as_bool().ok_or_else(|| {
                        InternalDeveloperError::VariableTypeCast {
                            session_variable: session_var_name.clone(),
                            expected: "true or false".into(),
                            found: session_var_value.to_string(),
                        }
                    })?;
                    Ok(serde_json::Value::Bool(value))
                }
                InbuiltType::ID | InbuiltType::String => {
                    let value: &str = session_var_value.as_str().ok_or_else(|| {
                        InternalDeveloperError::VariableTypeCast {
                            session_variable: session_var_name.clone(),
                            expected: "string".into(),
                            found: session_var_value.to_string(),
                        }
                    })?;
                    Ok(serde_json::Value::String(value.to_string()))
                }
            },
            QualifiedTypeName::Custom(_custom_type_name) => {
                let mut value = session_var_value.as_value().map_err(|parse_error| {
                    InternalDeveloperError::VariableExpectedJson {
                        session_variable: session_var_name.clone(),
                        parse_error,
                    }
                })?;

                // mutate `value` to fix the field names
                map_field_names_to_ndc_field_names(
                    &mut value,
                    to_type,
                    type_mappings,
                    object_types,
                    disallow_unknown_fields,
                )
                .map_err(InternalDeveloperError::MapFieldNamesError)?;

                Ok(value)
            }
        },
        QualifiedBaseType::List(element_type) => {
            let value = session_var_value.as_value().map_err(|parse_error| {
                InternalDeveloperError::VariableExpectedJson {
                    session_variable: session_var_name.clone(),
                    parse_error,
                }
            })?;
            let elements =
                value
                    .as_array()
                    .ok_or_else(|| InternalDeveloperError::VariableTypeCast {
                        session_variable: session_var_name.clone(),
                        expected: "array".into(),
                        found: value.to_string(),
                    })?;
            let typecasted_elements = elements
                .iter()
                .map(|element| {
                    typecast_session_variable_v2(
                        session_var_name,
                        &SessionVariableValue::Parsed(element.clone()),
                        element_type,
                        type_mappings,
                        object_types,
                        disallow_unknown_fields,
                    )
                })
                .collect::<Result<Vec<_>, _>>()?;

            Ok(serde_json::Value::Array(typecasted_elements))
        }
    }
}
