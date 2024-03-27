use std::collections::{BTreeMap, HashMap};

use lang_graphql::ast::common::Name;
use lang_graphql::normalized_ast::{InputField, Value};
use ndc_client::models as ndc_models;
use open_dds::types::{CustomTypeName, InbuiltType};

use crate::execute::error;
use crate::metadata::resolved::subgraph::{
    Qualified, QualifiedBaseType, QualifiedTypeName, QualifiedTypeReference,
};
use crate::metadata::resolved::types::TypeMapping;
use crate::schema::types::{Annotation, InputAnnotation, ModelInputAnnotation};
use crate::schema::GDS;

pub fn build_ndc_command_arguments_as_value(
    command_field: &Name,
    argument: &InputField<GDS>,
    command_type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
) -> Result<HashMap<String, serde_json::Value>, error::Error> {
    let mut ndc_arguments = HashMap::new();

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
            ndc_arguments.insert(ndc_func_proc_argument, mapped_argument_value);
            Ok(())
        }
        annotation => Err(error::InternalEngineError::UnexpectedAnnotation {
            annotation: annotation.clone(),
        }),
    }?;
    Ok(ndc_arguments)
}

pub fn build_ndc_model_arguments<'a, TInputFieldIter: Iterator<Item = &'a InputField<'a, GDS>>>(
    model_operation_field: &Name,
    arguments: TInputFieldIter,
    model_type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
) -> Result<BTreeMap<String, ndc_models::Argument>, error::Error> {
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
                    ndc_table_argument,
                    ndc_models::Argument::Literal {
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

fn map_argument_value_to_ndc_type(
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
                    let mapped_fields = object_value
                .iter()
                .map(|(_gql_field_name, field_value)| {
                    let (field_name, field_type) = match field_value.info.generic {
                        Annotation::Input(InputAnnotation::InputObjectField {
                            field_name,
                            field_type,
                        }) => Ok((field_name, field_type)),
                        annotation => Err(error::InternalEngineError::UnexpectedAnnotation {
                            annotation: annotation.clone(),
                        }),
                    }?;

                    let field_mapping = field_mappings.get(field_name).ok_or_else(|| {
                        error::InternalEngineError::InternalGeneric {
                            description: format!("unable to find mapping for field {field_name:}"),
                        }
                    })?;

                    let mapped_field_value = map_argument_value_to_ndc_type(
                        field_type,
                        &field_value.value,
                        type_mappings,
                    )?;
                    Ok((field_mapping.column.to_string(), mapped_field_value))
                })
                .collect::<Result<serde_json::Map<String, serde_json::Value>, error::Error>>()?;

                    Ok(serde_json::Value::Object(mapped_fields))
                }
            }
        }
    }
}
