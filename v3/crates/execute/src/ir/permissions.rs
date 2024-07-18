use std::collections::BTreeMap;

use hasura_authn_core::{SessionVariableValue, SessionVariables};
use lang_graphql::normalized_ast;

use open_dds::{
    data_connector::{DataConnectorColumnName, DataConnectorOperatorName},
    types::InbuiltType,
};

use crate::ir::error;
use crate::ir::filter::expression as filter_expression;
use crate::model_tracking::{count_model, UsagesCounts};
use metadata_resolve;

use metadata_resolve::{
    QualifiedBaseType, QualifiedTypeName, QualifiedTypeReference, UnaryComparisonOperator,
};
use schema;
use schema::GDS;

use super::relationship::LocalModelRelationshipInfo;
use super::{arguments::Argument, selection_set::NDCRelationshipName};

/// Fetch filter expression from the namespace annotation
/// of the field call. If the filter predicate namespace annotation
/// is not found, then an error will be thrown.
pub(crate) fn get_select_filter_predicate<'s>(
    field_call: &normalized_ast::FieldCall<'s, GDS>,
) -> Result<&'s metadata_resolve::FilterPermission, error::Error> {
    field_call
        .info
        .namespaced
        .as_ref()
        .and_then(|annotation| match annotation {
            schema::NamespaceAnnotation::Model { filter, .. } => Some(filter),
            schema::NamespaceAnnotation::NodeFieldTypeMappings(_)
            | schema::NamespaceAnnotation::EntityTypeMappings(_)
            | schema::NamespaceAnnotation::Command(_)
            | schema::NamespaceAnnotation::InputFieldPresets { .. } => None,
        })
        // If we're hitting this case, it means that the caller of this
        // function expects a filter predicate, but it was not annotated
        // when the V3 engine metadata was built
        .ok_or(error::Error::Internal(error::InternalError::Engine(
            error::InternalEngineError::ExpectedNamespaceAnnotationNotFound {
                namespace_annotation_type: "Filter".to_string(),
            },
        )))
}

/// Fetch argument presets from the namespace annotation
/// of the field call. If there are no annotations, this is fine,
/// but if unexpected ones are found an error will be thrown.
pub(crate) fn get_argument_presets(
    namespaced_info: &'_ Option<schema::NamespaceAnnotation>,
) -> Result<Option<&'_ schema::ArgumentPresets>, error::Error> {
    match namespaced_info.as_ref() {
        None => Ok(None), // no annotation is fine...
        Some(annotation) => match annotation {
            schema::NamespaceAnnotation::Command(argument_presets)
            | schema::NamespaceAnnotation::Model {
                argument_presets, ..
            } => Ok(Some(argument_presets)),
            other_namespace_annotation =>
            // If we're hitting this case, it means that the caller of this
            // function expects an annotation containing argument presets, but it was not annotated
            // when the V3 engine metadata was built
            {
                Err(error::Error::Internal(error::InternalError::Engine(
                    error::InternalEngineError::UnexpectedNamespaceAnnotation {
                        namespace_annotation: other_namespace_annotation.clone(),
                        expected_type: "ArgumentPresets".to_string(),
                    },
                )))
            }
        },
    }
}

pub fn process_model_predicate<'s>(
    model_predicate: &'s metadata_resolve::ModelPredicate,
    session_variables: &SessionVariables,
    usage_counts: &mut UsagesCounts,
) -> Result<filter_expression::Expression<'s>, error::Error> {
    match model_predicate {
        metadata_resolve::ModelPredicate::UnaryFieldComparison {
            field: _,
            field_parent_type: _,
            ndc_column,
            operator,
        } => Ok(make_permission_unary_boolean_expression(
            ndc_column, *operator,
        )),
        metadata_resolve::ModelPredicate::BinaryFieldComparison {
            field: _,
            field_parent_type: _,
            ndc_column,
            argument_type,
            operator,
            value,
        } => Ok(make_permission_binary_boolean_expression(
            ndc_column,
            argument_type,
            operator,
            value,
            session_variables,
        )?),
        metadata_resolve::ModelPredicate::Not(predicate) => {
            let expr = process_model_predicate(predicate, session_variables, usage_counts)?;
            Ok(filter_expression::Expression::Not {
                expression: Box::new(expr),
            })
        }
        metadata_resolve::ModelPredicate::And(predicates) => {
            let exprs = predicates
                .iter()
                .map(|p| process_model_predicate(p, session_variables, usage_counts))
                .collect::<Result<Vec<_>, error::Error>>()?;
            Ok(filter_expression::Expression::And { expressions: exprs })
        }
        metadata_resolve::ModelPredicate::Or(predicates) => {
            let exprs = predicates
                .iter()
                .map(|p| process_model_predicate(p, session_variables, usage_counts))
                .collect::<Result<Vec<_>, error::Error>>()?;
            Ok(filter_expression::Expression::Or { expressions: exprs })
        }
        metadata_resolve::ModelPredicate::Relationship {
            relationship_info,
            predicate,
        } => {
            // Add the target model being used in the usage counts
            count_model(&relationship_info.target_model_name, usage_counts);

            let relationship_name = (NDCRelationshipName::new(
                &relationship_info.source_type,
                &relationship_info.relationship_name,
            ))?;

            let info = LocalModelRelationshipInfo {
                relationship_name: &relationship_info.relationship_name,
                relationship_type: &relationship_info.relationship_type,
                source_type: &relationship_info.source_type,
                source_data_connector: &relationship_info.source_data_connector,
                source_type_mappings: &relationship_info.source_type_mappings,
                target_source: &relationship_info.target_source,
                target_type: &relationship_info.target_type,
                mappings: &relationship_info.mappings,
            };

            let relationship_predicate =
                process_model_predicate(predicate, session_variables, usage_counts)?;

            let local_relationship_filter = filter_expression::Expression::LocalRelationship {
                relationship: relationship_name,
                arguments: BTreeMap::new(),
                predicate: Box::new(relationship_predicate),
                info,
            };

            Ok(local_relationship_filter)
        }
    }
}

fn make_permission_binary_boolean_expression<'s>(
    ndc_column: &DataConnectorColumnName,
    argument_type: &QualifiedTypeReference,
    operator: &DataConnectorOperatorName,
    value_expression: &'s metadata_resolve::ValueExpression,
    session_variables: &SessionVariables,
) -> Result<filter_expression::Expression<'s>, error::Error> {
    let ndc_expression_value =
        make_argument_from_value_expression(value_expression, argument_type, session_variables)?;
    Ok(filter_expression::Expression::LocalField(
        filter_expression::LocalFieldComparison::BinaryComparison {
            column: ndc_models::ComparisonTarget::Column {
                name: ndc_models::FieldName::from(ndc_column.as_str()),
                field_path: None,
            },
            operator: ndc_models::ComparisonOperatorName::from(operator.as_str()),
            value: ndc_models::ComparisonValue::Scalar {
                value: ndc_expression_value,
            },
        },
    ))
}

fn make_permission_unary_boolean_expression<'s>(
    ndc_column: &DataConnectorColumnName,
    operator: UnaryComparisonOperator,
) -> filter_expression::Expression<'s> {
    filter_expression::Expression::LocalField(
        filter_expression::LocalFieldComparison::UnaryComparison {
            column: ndc_models::ComparisonTarget::Column {
                name: ndc_models::FieldName::from(ndc_column.as_str()),
                field_path: None,
            },
            operator: match operator {
                UnaryComparisonOperator::IsNull => ndc_models::UnaryComparisonOperator::IsNull,
            },
        },
    )
}

pub(crate) fn make_argument_from_value_expression(
    val_expr: &metadata_resolve::ValueExpression,
    value_type: &QualifiedTypeReference,
    session_variables: &SessionVariables,
) -> Result<serde_json::Value, error::Error> {
    match val_expr {
        metadata_resolve::ValueExpression::Literal(val) => Ok(val.clone()),
        metadata_resolve::ValueExpression::SessionVariable(session_var) => {
            let value = session_variables.get(session_var).ok_or_else(|| {
                error::InternalDeveloperError::MissingSessionVariable {
                    session_variable: session_var.clone(),
                }
            })?;

            typecast_session_variable(value, value_type)
        }
    }
}

pub(crate) fn make_argument_from_value_expression_or_predicate<'s>(
    val_expr: &'s metadata_resolve::ValueExpressionOrPredicate,
    value_type: &QualifiedTypeReference,
    session_variables: &SessionVariables,
    usage_counts: &mut UsagesCounts,
) -> Result<Argument<'s>, error::Error> {
    match val_expr {
        metadata_resolve::ValueExpressionOrPredicate::Literal(val) => {
            Ok(Argument::Literal { value: val.clone() })
        }
        metadata_resolve::ValueExpressionOrPredicate::SessionVariable(session_var) => {
            let value = session_variables.get(session_var).ok_or_else(|| {
                error::InternalDeveloperError::MissingSessionVariable {
                    session_variable: session_var.clone(),
                }
            })?;

            Ok(Argument::Literal {
                value: typecast_session_variable(value, value_type)?,
            })
        }
        metadata_resolve::ValueExpressionOrPredicate::BooleanExpression(model_predicate) => {
            let filter_expression =
                process_model_predicate(model_predicate, session_variables, usage_counts)?;
            Ok(Argument::BooleanExpression {
                predicate: filter_expression,
            })
        }
    }
}

/// Typecast a stringified session variable into a given type, but as a serde_json::Value
fn typecast_session_variable(
    session_var_value_wrapped: &SessionVariableValue,
    to_type: &QualifiedTypeReference,
) -> Result<serde_json::Value, error::Error> {
    let session_var_value = &session_var_value_wrapped.0;
    match &to_type.underlying_type {
        QualifiedBaseType::Named(type_name) => {
            match type_name {
                QualifiedTypeName::Inbuilt(primitive) => match primitive {
                    InbuiltType::Int => {
                        let value: i32 = session_var_value.parse().map_err(|_| {
                            error::InternalDeveloperError::VariableTypeCast {
                                expected: "int".into(),
                                found: session_var_value.clone(),
                            }
                        })?;
                        Ok(serde_json::Value::Number(value.into()))
                    }
                    InbuiltType::Float => {
                        let value: f32 = session_var_value.parse().map_err(|_| {
                            error::InternalDeveloperError::VariableTypeCast {
                                expected: "float".into(),
                                found: session_var_value.clone(),
                            }
                        })?;
                        Ok(serde_json::to_value(value)?)
                    }
                    InbuiltType::Boolean => match session_var_value.as_str() {
                        "true" => Ok(serde_json::Value::Bool(true)),
                        "false" => Ok(serde_json::Value::Bool(false)),
                        _ => Err(error::InternalDeveloperError::VariableTypeCast {
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
        QualifiedBaseType::List(_) => Err(error::InternalDeveloperError::VariableArrayTypeCast)?,
    }
}
