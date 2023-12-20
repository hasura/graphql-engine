use hasura_authn_core::{SessionVariableValue, SessionVariables};
use lang_graphql::normalized_ast;
use ndc_client as gdc;

use open_dds::{permissions::ValueExpression, types::InbuiltType};

use crate::execute::error::{Error, InternalDeveloperError, InternalEngineError, InternalError};
use crate::metadata::resolved;

use crate::metadata::resolved::subgraph::{
    QualifiedBaseType, QualifiedTypeName, QualifiedTypeReference,
};
use crate::schema::types;
use crate::schema::GDS;

/// Fetch filter expression from the namespace annotation
/// of the field call. If the filter predicate namespace annotation
/// is not found, then an error will be thrown.
pub(crate) fn get_select_filter_predicate<'s>(
    field_call: &normalized_ast::FieldCall<'s, GDS>,
) -> Result<&'s resolved::model::FilterPermission, Error> {
    field_call
        .info
        .namespaced
        .as_ref()
        .and_then(|annotation| match annotation {
            types::NamespaceAnnotation::Filter(predicate) => Some(predicate),
            types::NamespaceAnnotation::NodeFieldTypeMappings(_) => None,
        })
        // If we're hitting this case, it means that the caller of this
        // function expects a filter predicate, but it was not annotated
        // when the V3 engine metadata was built
        .ok_or(Error::InternalError(InternalError::Engine(
            InternalEngineError::ExpectedNamespaceAnnotationNotFound {
                namespace_annotation_type: "Filter".to_string(),
            },
        )))
}

pub(crate) fn process_model_predicate(
    model_predicate: &resolved::model::ModelPredicate,
    session_variables: &SessionVariables,
) -> Result<gdc::models::Expression, Error> {
    match model_predicate {
        resolved::model::ModelPredicate::UnaryFieldComparison {
            field: _,
            ndc_column,
            operator,
        } => Ok(make_permission_unary_boolean_expression(
            ndc_column.clone(),
            operator,
        )?),
        resolved::model::ModelPredicate::BinaryFieldComparison {
            field: _,
            ndc_column,
            argument_type,
            operator,
            value,
        } => Ok(make_permission_binary_boolean_expression(
            ndc_column.clone(),
            argument_type,
            operator,
            value,
            session_variables,
        )?),
        resolved::model::ModelPredicate::Not(predicate) => {
            let expr = process_model_predicate(predicate, session_variables)?;
            Ok(gdc::models::Expression::Not {
                expression: Box::new(expr),
            })
        }
        resolved::model::ModelPredicate::And(predicates) => {
            let exprs = predicates
                .iter()
                .map(|p| process_model_predicate(p, session_variables))
                .collect::<Result<Vec<_>, Error>>()?;
            Ok(gdc::models::Expression::And { expressions: exprs })
        }
        resolved::model::ModelPredicate::Or(predicates) => {
            let exprs = predicates
                .iter()
                .map(|p| process_model_predicate(p, session_variables))
                .collect::<Result<Vec<_>, Error>>()?;
            Ok(gdc::models::Expression::Or { expressions: exprs })
        }
        // TODO: implement this
        // TODO: naveen: When we can use models in predicates, make sure to
        // include those models in the 'models_used' field of the IR's. This is
        // for tracking the models used in query.
        resolved::model::ModelPredicate::Relationship {
            name: _,
            predicate: _,
        } => Err(InternalEngineError::InternalGeneric {
            description: "'relationship' model predicate is not supported yet.".to_string(),
        })?,
    }
}

fn make_permission_binary_boolean_expression(
    ndc_column: String,
    argument_type: &QualifiedTypeReference,
    operator: &ndc_client::models::BinaryComparisonOperator,
    value_expression: &ValueExpression,
    session_variables: &SessionVariables,
) -> Result<gdc::models::Expression, Error> {
    let ndc_expression_value =
        make_value_from_value_expression(value_expression, argument_type, session_variables)?;
    Ok(gdc::models::Expression::BinaryComparisonOperator {
        column: gdc::models::ComparisonTarget::Column {
            name: ndc_column,
            path: vec![],
        },
        operator: operator.clone(),
        value: ndc_expression_value,
    })
}

fn make_permission_unary_boolean_expression(
    ndc_column: String,
    operator: &ndc_client::models::UnaryComparisonOperator,
) -> Result<gdc::models::Expression, Error> {
    Ok(gdc::models::Expression::UnaryComparisonOperator {
        column: gdc::models::ComparisonTarget::Column {
            name: ndc_column,
            path: vec![],
        },
        operator: *operator,
    })
}

fn make_value_from_value_expression(
    val_expr: &ValueExpression,
    field_type: &QualifiedTypeReference,
    session_variables: &SessionVariables,
) -> Result<gdc::models::ComparisonValue, Error> {
    match val_expr {
        ValueExpression::Literal(val) => {
            Ok(gdc::models::ComparisonValue::Scalar { value: val.clone() })
        }
        ValueExpression::SessionVariable(session_var) => {
            let value = session_variables.get(session_var).ok_or_else(|| {
                InternalDeveloperError::MissingSessionVariable {
                    session_variable: session_var.clone(),
                }
            })?;

            Ok(gdc::models::ComparisonValue::Scalar {
                value: typecast_session_variable(value, field_type)?,
            })
        }
    }
}

/// Typecast a stringified session variable into a given type, but as a serde_json::Value
fn typecast_session_variable(
    session_var_value_wrapped: &SessionVariableValue,
    to_type: &QualifiedTypeReference,
) -> Result<serde_json::Value, Error> {
    let session_var_value = &session_var_value_wrapped.0;
    match &to_type.underlying_type {
        QualifiedBaseType::Named(type_name) => {
            match type_name {
                QualifiedTypeName::Inbuilt(primitive) => match primitive {
                    InbuiltType::Int => {
                        let value: i32 = session_var_value.parse().map_err(|_| {
                            InternalDeveloperError::VariableTypeCast {
                                expected: "int".into(),
                                found: session_var_value.clone(),
                            }
                        })?;
                        Ok(serde_json::Value::Number(value.into()))
                    }
                    InbuiltType::Float => {
                        let value: f32 = session_var_value.parse().map_err(|_| {
                            InternalDeveloperError::VariableTypeCast {
                                expected: "float".into(),
                                found: session_var_value.clone(),
                            }
                        })?;
                        Ok(serde_json::to_value(value)?)
                    }
                    InbuiltType::Boolean => match session_var_value.as_str() {
                        "true" => Ok(serde_json::Value::Bool(true)),
                        "false" => Ok(serde_json::Value::Bool(false)),
                        _ => Err(InternalDeveloperError::VariableTypeCast {
                            expected: "true or false".into(),
                            found: session_var_value.clone(),
                        })?,
                    },
                    InbuiltType::String => Ok(serde_json::to_value(session_var_value)?),
                    InbuiltType::ID => Ok(serde_json::to_value(session_var_value)?),
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
                        Err(_) => Ok(serde_json::to_value(session_var_value)?),
                    }
                }
            }
        }
        QualifiedBaseType::List(_) => Err(InternalDeveloperError::VariableArrayTypeCast)?,
    }
}
