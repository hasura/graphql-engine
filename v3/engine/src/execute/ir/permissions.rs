use std::collections::BTreeMap;

use hasura_authn_core::{SessionVariableValue, SessionVariables};
use lang_graphql::normalized_ast;
use ndc_client as gdc;

use open_dds::{permissions::ValueExpression, types::InbuiltType};

use crate::execute::error::{Error, InternalDeveloperError, InternalEngineError, InternalError};
use crate::execute::model_tracking::{count_model, UsagesCounts};
use crate::metadata::resolved;

use crate::metadata::resolved::subgraph::{
    QualifiedBaseType, QualifiedTypeName, QualifiedTypeReference,
};
use crate::schema::types;
use crate::schema::GDS;

use super::relationship::LocalModelRelationshipInfo;
use super::selection_set::NDCRelationshipName;

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
            types::NamespaceAnnotation::ArgumentPresets(_) => None,
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

pub(crate) fn process_model_predicate<'s>(
    model_predicate: &'s resolved::model::ModelPredicate,
    session_variables: &SessionVariables,
    mut relationship_paths: Vec<NDCRelationshipName>,
    relationships: &mut BTreeMap<NDCRelationshipName, LocalModelRelationshipInfo<'s>>,
    usage_counts: &mut UsagesCounts,
) -> Result<gdc::models::Expression, Error> {
    match model_predicate {
        resolved::model::ModelPredicate::UnaryFieldComparison {
            field: _,
            ndc_column,
            operator,
        } => Ok(make_permission_unary_boolean_expression(
            ndc_column.clone(),
            operator,
            &relationship_paths,
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
            &relationship_paths,
        )?),
        resolved::model::ModelPredicate::Not(predicate) => {
            let expr = process_model_predicate(
                predicate,
                session_variables,
                relationship_paths,
                relationships,
                usage_counts,
            )?;
            Ok(gdc::models::Expression::Not {
                expression: Box::new(expr),
            })
        }
        resolved::model::ModelPredicate::And(predicates) => {
            let exprs = predicates
                .iter()
                .map(|p| {
                    process_model_predicate(
                        p,
                        session_variables,
                        relationship_paths.clone(),
                        relationships,
                        usage_counts,
                    )
                })
                .collect::<Result<Vec<_>, Error>>()?;
            Ok(gdc::models::Expression::And { expressions: exprs })
        }
        resolved::model::ModelPredicate::Or(predicates) => {
            let exprs = predicates
                .iter()
                .map(|p| {
                    process_model_predicate(
                        p,
                        session_variables,
                        relationship_paths.clone(),
                        relationships,
                        usage_counts,
                    )
                })
                .collect::<Result<Vec<_>, Error>>()?;
            Ok(gdc::models::Expression::Or { expressions: exprs })
        }
        resolved::model::ModelPredicate::Relationship {
            relationship_info,
            predicate,
        } => {
            let relationship_name = (NDCRelationshipName::new(
                &relationship_info.source_type,
                &relationship_info.relationship_name,
            ))?;

            relationship_paths.push(relationship_name.clone());
            relationships.insert(
                relationship_name.clone(),
                LocalModelRelationshipInfo {
                    relationship_name: &relationship_info.relationship_name,
                    relationship_type: &relationship_info.relationship_type,
                    source_type: &relationship_info.source_type,
                    source_data_connector: &relationship_info.source_data_connector,
                    source_type_mappings: &relationship_info.source_type_mappings,
                    target_source: &relationship_info.target_source,
                    target_type: &relationship_info.target_type,
                    mappings: &relationship_info.mappings,
                },
            );

            // Add the target model being used in the usage counts
            count_model(relationship_info.target_model_name.clone(), usage_counts);

            process_model_predicate(
                predicate,
                session_variables,
                relationship_paths,
                relationships,
                usage_counts,
            )
        }
    }
}

fn make_permission_binary_boolean_expression(
    ndc_column: String,
    argument_type: &QualifiedTypeReference,
    operator: &str,
    value_expression: &ValueExpression,
    session_variables: &SessionVariables,
    relationship_paths: &Vec<NDCRelationshipName>,
) -> Result<gdc::models::Expression, Error> {
    let path_elements = super::filter::build_path_elements(relationship_paths);
    let ndc_expression_value =
        make_value_from_value_expression(value_expression, argument_type, session_variables)?;
    Ok(gdc::models::Expression::BinaryComparisonOperator {
        column: gdc::models::ComparisonTarget::Column {
            name: ndc_column,
            path: path_elements,
        },
        operator: operator.to_owned(),
        value: ndc_expression_value,
    })
}

fn make_permission_unary_boolean_expression(
    ndc_column: String,
    operator: &ndc_client::models::UnaryComparisonOperator,
    relationship_paths: &Vec<NDCRelationshipName>,
) -> Result<gdc::models::Expression, Error> {
    let path_elements = super::filter::build_path_elements(relationship_paths);
    Ok(gdc::models::Expression::UnaryComparisonOperator {
        column: gdc::models::ComparisonTarget::Column {
            name: ndc_column,
            path: path_elements,
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
pub(crate) fn typecast_session_variable(
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
