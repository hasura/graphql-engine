use std::collections::BTreeMap;

use crate::ir::arguments;
use crate::ir::error;
use crate::model_tracking::UsagesCounts;
use hasura_authn_core::SessionVariables;
use lang_graphql::ast::common::Name;
use lang_graphql::normalized_ast::{InputField, Value};
use metadata_resolve::TypeMapping;
use metadata_resolve::{Qualified, QualifiedBaseType, QualifiedTypeName, QualifiedTypeReference};
use nonempty::NonEmpty;
use open_dds::{
    data_connector::DataConnectorColumnName,
    types::{CustomTypeName, DataConnectorArgumentName, InbuiltType},
};
use schema::GDS;
use schema::{
    Annotation, ArgumentNameAndPath, ArgumentPresets, InputAnnotation, ModelInputAnnotation,
};
use serde::Serialize;

use super::filter;
use super::permissions;

#[derive(Debug, Serialize)]
pub enum Argument {
    /// The argument is provided as a literal value
    Literal {
        value: serde_json::Value,
    },
    BooleanExpression {
        predicate: filter::FilterExpression,
    },
}

/// Takes a field path and a serde_json object, and insert a serde_json value
/// into that object, following the field path.
///
/// For example,
/// with JSON object -
///   `{"name": "Queen Mary University of London", "location": {"city": "London"}}`
/// a field path - `["location", "country"]`, and a value - "UK"
/// it will modify the JSON object to -
///   `{"name": "Queen Mary University of London", "location": {"city": "London", "country": "UK"}}`
pub(crate) fn follow_field_path_and_insert_value(
    field_path: &NonEmpty<DataConnectorColumnName>,
    object_slice: &mut serde_json::Map<String, serde_json::Value>,
    value: serde_json::Value,
) -> Result<(), error::Error> {
    let (field_name, rest) = field_path.split_first();
    match NonEmpty::from_slice(rest) {
        // if rest is empty, we have only one-top level field. insert that into the object
        None => {
            object_slice.insert(field_name.to_string(), value);
        }
        // if rest is *not* empty, pick the field from the current object, and
        // recursively process with the rest
        Some(tail) => {
            match object_slice.get_mut(field_name.as_str()) {
                None => {
                    // object should have this field; if it doesn't then all the fields are preset
                    object_slice.insert(
                        field_name.to_string(),
                        serde_json::Value::Object(serde_json::Map::new()),
                    );
                }
                Some(json_value) => {
                    let inner_object = json_value.as_object_mut().ok_or_else(|| {
                        error::InternalEngineError::ArgumentPresetExecution {
                            description: "input value is not a valid JSON object".to_string(),
                        }
                    })?;
                    follow_field_path_and_insert_value(&tail, inner_object, value)?;
                }
            }
        }
    }
    Ok(())
}

/// Takes 'ArgumentPresets' annotations and existing model arguments (which
/// might be partially filled), and fill values in the existing model arguments
/// based on the presets
pub(crate) fn process_model_arguments_presets(
    argument_presets: &ArgumentPresets,
    session_variables: &SessionVariables,
    model_arguments: &mut BTreeMap<DataConnectorArgumentName, arguments::Argument>,
    usage_counts: &mut UsagesCounts,
) -> Result<(), error::Error> {
    let ArgumentPresets { argument_presets } = argument_presets;
    for (argument_name_and_path, (field_type, argument_value)) in argument_presets {
        let ArgumentNameAndPath {
            ndc_argument_name,
            field_path,
        } = argument_name_and_path;

        let argument_name = ndc_argument_name.as_ref().ok_or_else(|| {
            // this can only happen when no argument mapping was not found
            // during annotation generation
            error::InternalEngineError::ArgumentPresetExecution {
                description: "unexpected; ndc argument name not preset".to_string(),
            }
        })?;

        let actual_value = permissions::make_argument_from_value_expression_or_predicate(
            argument_value,
            field_type,
            session_variables,
            usage_counts,
        )?;

        match NonEmpty::from_slice(field_path) {
            // if field path is empty, then the entire argument has to preset
            None => {
                model_arguments.insert(argument_name.clone(), actual_value);
            }
            // if there is some field path, preset the argument partially based on the field path
            Some(field_path) => {
                if let Some(current_arg) = model_arguments.get_mut(&argument_name.clone()) {
                    let current_arg = match current_arg {
                        arguments::Argument::Literal { value } => Ok(value),
                        arguments::Argument::BooleanExpression { predicate: _ } => {
                            Err(error::InternalEngineError::ArgumentPresetExecution {
                                description: "unexpected; can't merge an argument preset into an argument that has a boolean expression value"
                                    .to_owned(),
                            })
                        }
                    }?;
                    let preset_value = match actual_value {
                        Argument::Literal { value } => Ok(value),
                        Argument::BooleanExpression { predicate: _ } => {
                            // See schema::Error::BooleanExpressionInTypePresetArgument
                            Err(error::InternalEngineError::ArgumentPresetExecution {
                                description: "unexpected; type input presets cannot contain a boolean expression preset value"
                                    .to_owned(),
                            })
                        }
                    }?;
                    if let Some(current_arg_object) = current_arg.as_object_mut() {
                        arguments::follow_field_path_and_insert_value(
                            &field_path,
                            current_arg_object,
                            preset_value,
                        )?;
                    }
                }
            }
        }
    }
    Ok(())
}

pub fn build_ndc_command_arguments_as_value(
    command_field: &Name,
    argument: &InputField<GDS>,
    command_type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
) -> Result<(DataConnectorArgumentName, serde_json::Value), error::Error> {
    match argument.info.generic {
        Annotation::Input(InputAnnotation::CommandArgument {
            argument_type,
            ndc_func_proc_argument,
        }) => {
            let ndc_func_proc_argument = ndc_func_proc_argument.clone().ok_or_else(|| {
                error::InternalDeveloperError::NoArgumentSource {
                    field_name: command_field.clone(),
                    argument_name: argument.name.clone(),
                }
            })?;
            let mapped_argument_value = map_argument_value_to_ndc_type(
                argument_type,
                &argument.value,
                command_type_mappings,
            )?;
            return Ok((ndc_func_proc_argument, mapped_argument_value));
        }
        annotation => Err(error::InternalEngineError::UnexpectedAnnotation {
            annotation: annotation.clone(),
        }),
    }?
}

pub fn build_ndc_model_arguments<'a, TInputFieldIter: Iterator<Item = &'a InputField<'a, GDS>>>(
    model_operation_field: &Name,
    arguments: TInputFieldIter,
    model_type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
) -> Result<BTreeMap<DataConnectorArgumentName, arguments::Argument>, error::Error> {
    let mut ndc_arguments = BTreeMap::new();
    for argument in arguments {
        match argument.info.generic {
            Annotation::Input(InputAnnotation::Model(ModelInputAnnotation::ModelArgument {
                argument_type,
                ndc_table_argument,
            })) => {
                let ndc_table_argument = ndc_table_argument.clone().ok_or_else(|| {
                    error::InternalDeveloperError::NoArgumentSource {
                        field_name: model_operation_field.clone(),
                        argument_name: argument.name.clone(),
                    }
                })?;
                let mapped_argument_value = map_argument_value_to_ndc_type(
                    argument_type,
                    &argument.value,
                    model_type_mappings,
                )?;
                ndc_arguments.insert(
                    ndc_table_argument.clone(),
                    arguments::Argument::Literal {
                        value: mapped_argument_value,
                    },
                );
            }
            annotation => Err(error::InternalEngineError::UnexpectedAnnotation {
                annotation: annotation.clone(),
            })?,
        }
    }
    Ok(ndc_arguments)
}

pub(crate) fn map_argument_value_to_ndc_type(
    value_type: &QualifiedTypeReference,
    value: &Value<GDS>,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
) -> Result<serde_json::Value, error::Error> {
    if value.is_null() {
        return Ok(serde_json::Value::Null);
    }

    match &value_type.underlying_type {
        QualifiedBaseType::List(element_type) => {
            let mapped_elements = value
                .as_list()?
                .iter()
                .map(|element_value| {
                    map_argument_value_to_ndc_type(element_type, element_value, type_mappings)
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(serde_json::Value::from(mapped_elements))
        }
        QualifiedBaseType::Named(QualifiedTypeName::Inbuilt(InbuiltType::String)) => {
            Ok(serde_json::Value::from(value.as_string()?))
        }
        QualifiedBaseType::Named(QualifiedTypeName::Inbuilt(InbuiltType::Float)) => {
            Ok(serde_json::Value::from(value.as_float()?))
        }
        QualifiedBaseType::Named(QualifiedTypeName::Inbuilt(InbuiltType::Int)) => {
            Ok(serde_json::Value::from(value.as_int_i64()?))
        }
        QualifiedBaseType::Named(QualifiedTypeName::Inbuilt(InbuiltType::ID)) => {
            Ok(serde_json::Value::from(value.as_id()?))
        }
        QualifiedBaseType::Named(QualifiedTypeName::Inbuilt(InbuiltType::Boolean)) => {
            Ok(serde_json::Value::from(value.as_boolean()?))
        }
        QualifiedBaseType::Named(QualifiedTypeName::Custom(custom_type_name)) => {
            match type_mappings.get(custom_type_name) {
                // If the custom type is a scalar or object but opaque on the NDC side, there won't be a mapping,
                // in which case, pass it as-is.
                None => Ok(value.as_json()),
                Some(TypeMapping::Object { field_mappings, .. }) => {
                    let object_value = value.as_object()?;
                    let mapped_fields =
                        object_value
                            .iter()
                            .map(|(_gql_field_name, field_value)| {
                                let (field_name, field_type) = match field_value.info.generic {
                                    Annotation::Input(InputAnnotation::InputObjectField {
                                        field_name,
                                        field_type,
                                        parent_type: _,
                                    }) => Ok((field_name, field_type)),
                                    annotation => {
                                        Err(error::InternalEngineError::UnexpectedAnnotation {
                                            annotation: annotation.clone(),
                                        })
                                    }
                                }?;

                                let field_mapping =
                                    field_mappings.get(field_name).ok_or_else(|| {
                                        error::InternalEngineError::InternalGeneric {
                                            description: format!(
                                                "unable to find mapping for field {field_name:}"
                                            ),
                                        }
                                    })?;

                                let mapped_field_value = map_argument_value_to_ndc_type(
                                    field_type,
                                    &field_value.value,
                                    type_mappings,
                                )?;
                                Ok((field_mapping.column.to_string(), mapped_field_value))
                            })
                            .collect::<Result<
                                serde_json::Map<String, serde_json::Value>,
                                error::Error,
                            >>()?;

                    Ok(serde_json::Value::Object(mapped_fields))
                }
            }
        }
    }
}
