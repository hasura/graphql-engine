use std::collections::BTreeMap;

use hasura_authn_core::{SessionVariableValue, SessionVariables};
use lang_graphql::normalized_ast;
use ndc_models;

use open_dds::types::InbuiltType;

use crate::ir::error;
use crate::model_tracking::{count_model, UsagesCounts};
use metadata_resolve;

use metadata_resolve::{QualifiedBaseType, QualifiedTypeName, QualifiedTypeReference};
use schema;
use schema::GDS;

use super::relationship::LocalModelRelationshipInfo;
use super::selection_set::NDCRelationshipName;

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
            schema::NamespaceAnnotation::NodeFieldTypeMappings(_) => None,
            schema::NamespaceAnnotation::EntityTypeMappings(_) => None,
            schema::NamespaceAnnotation::Command(_) => None,
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
            schema::NamespaceAnnotation::Command(argument_presets) => Ok(Some(argument_presets)),
            schema::NamespaceAnnotation::Model {
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

pub(crate) fn process_model_predicate<'s>(
    model_predicate: &'s metadata_resolve::ModelPredicate,
    session_variables: &SessionVariables,
    mut relationship_paths: Vec<NDCRelationshipName>,
    relationships: &mut BTreeMap<NDCRelationshipName, LocalModelRelationshipInfo<'s>>,
    usage_counts: &mut UsagesCounts,
) -> Result<ndc_models::Expression, error::Error> {
    match model_predicate {
        metadata_resolve::ModelPredicate::UnaryFieldComparison {
            field: _,
            ndc_column,
            operator,
        } => Ok(make_permission_unary_boolean_expression(
            ndc_column.clone(),
            *operator,
            &relationship_paths,
        )?),
        metadata_resolve::ModelPredicate::BinaryFieldComparison {
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
            usage_counts,
        )?),
        metadata_resolve::ModelPredicate::Not(predicate) => {
            let expr = process_model_predicate(
                predicate,
                session_variables,
                relationship_paths,
                relationships,
                usage_counts,
            )?;
            Ok(ndc_models::Expression::Not {
                expression: Box::new(expr),
            })
        }
        metadata_resolve::ModelPredicate::And(predicates) => {
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
                .collect::<Result<Vec<_>, error::Error>>()?;
            Ok(ndc_models::Expression::And { expressions: exprs })
        }
        metadata_resolve::ModelPredicate::Or(predicates) => {
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
                .collect::<Result<Vec<_>, error::Error>>()?;
            Ok(ndc_models::Expression::Or { expressions: exprs })
        }
        metadata_resolve::ModelPredicate::Relationship {
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
            count_model(&relationship_info.target_model_name, usage_counts);

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
    value_expression: &metadata_resolve::ValueExpression,
    session_variables: &SessionVariables,
    relationship_paths: &Vec<NDCRelationshipName>,
    usage_counts: &mut UsagesCounts,
) -> Result<ndc_models::Expression, error::Error> {
    let path_elements = super::filter::build_path_elements(relationship_paths);
    let ndc_expression_value = make_value_from_value_expression(
        value_expression,
        argument_type,
        session_variables,
        usage_counts,
    )?;
    Ok(ndc_models::Expression::BinaryComparisonOperator {
        column: ndc_models::ComparisonTarget::Column {
            name: ndc_column,
            path: path_elements,
        },
        operator: operator.to_owned(),
        value: ndc_models::ComparisonValue::Scalar {
            value: ndc_expression_value,
        },
    })
}

fn make_permission_unary_boolean_expression(
    ndc_column: String,
    operator: ndc_models::UnaryComparisonOperator,
    relationship_paths: &Vec<NDCRelationshipName>,
) -> Result<ndc_models::Expression, error::Error> {
    let path_elements = super::filter::build_path_elements(relationship_paths);
    Ok(ndc_models::Expression::UnaryComparisonOperator {
        column: ndc_models::ComparisonTarget::Column {
            name: ndc_column,
            path: path_elements,
        },
        operator,
    })
}

pub(crate) fn make_value_from_value_expression(
    val_expr: &metadata_resolve::ValueExpression,
    value_type: &QualifiedTypeReference,
    session_variables: &SessionVariables,
    usage_counts: &mut UsagesCounts,
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
        metadata_resolve::ValueExpression::BooleanExpression(model_predicate) => {
            let relationship_paths = Vec::new();
            let mut relationships = BTreeMap::new();

            let ndc_expression = process_model_predicate(
                model_predicate,
                session_variables,
                relationship_paths,
                &mut relationships,
                usage_counts,
            )?;
            Ok(serde_json::to_value(ndc_expression)?)
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
