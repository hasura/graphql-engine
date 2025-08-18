use std::collections::BTreeMap;

use crate::error;
use crate::filter;
use crate::flags::GraphqlIrFlags;
use graphql_schema::GDS;
use graphql_schema::{Annotation, InputAnnotation, ModelInputAnnotation};
use indexmap::IndexMap;
use lang_graphql::ast::common::Name;
use lang_graphql::normalized_ast::{InputField, Value};
use metadata_resolve::{
    ArgumentKind, Qualified, QualifiedBaseType, QualifiedTypeName, QualifiedTypeReference,
    TypeMapping,
};
use open_dds::{
    arguments::ArgumentName,
    identifier::Identifier,
    types::{CustomTypeName, InbuiltType},
};
use plan_types::UsagesCounts;

/// The "args" input field.
pub fn resolve_model_arguments_input_opendd<'s>(
    arguments: &IndexMap<Name, InputField<'s, GDS>>,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    flags: &GraphqlIrFlags,
    usage_counts: &mut UsagesCounts,
) -> Result<IndexMap<open_dds::query::ArgumentName, open_dds::query::Value>, error::Error> {
    arguments
        .values()
        .map(|argument| resolve_argument_opendd(argument, type_mappings, flags, usage_counts))
        .collect::<Result<IndexMap<_, _>, _>>()
}

// fetch input values from annotations and turn them into either JSON or an Expression
pub fn resolve_argument_opendd<'s>(
    argument: &InputField<'s, GDS>,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    flags: &GraphqlIrFlags,
    usage_counts: &mut UsagesCounts,
) -> Result<(open_dds::query::ArgumentName, open_dds::query::Value), error::Error> {
    let (argument_name, argument_type, argument_kind) = match argument.info.generic {
        Annotation::Input(
            InputAnnotation::CommandArgument {
                argument_name,
                argument_type,
                argument_kind,
            }
            | InputAnnotation::Model(ModelInputAnnotation::ModelArgument {
                argument_name,
                argument_type,
                argument_kind,
            }),
        ) => Ok((argument_name, argument_type, argument_kind)),

        annotation => Err(error::InternalEngineError::UnexpectedAnnotation {
            annotation: annotation.clone(),
        }),
    }?;

    // simple values are serialized to JSON
    // predicates are converted into boolean expressions
    let mapped_argument_value = match argument_kind {
        ArgumentKind::Other => {
            map_argument_value_to_ndc_type(argument_type, &argument.value, type_mappings, flags)
                .map(open_dds::query::Value::Literal)?
        }

        ArgumentKind::NDCExpression => filter::resolve_filter_expression_open_dd(
            argument.value.as_object()?,
            flags,
            usage_counts,
        )
        .map(open_dds::query::Value::BooleanExpression)?,
    };
    Ok((argument_name.clone(), mapped_argument_value))
}

// fetch input values from annotations and turn them into either JSON or an Expression
pub fn build_argument_as_value<'s>(
    argument: &InputField<'s, GDS>,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    flags: &GraphqlIrFlags,
    usage_counts: &mut UsagesCounts,
) -> Result<(ArgumentName, open_dds::query::Value), error::Error> {
    let (argument_type, argument_kind) = match argument.info.generic {
        Annotation::Input(
            InputAnnotation::CommandArgument {
                argument_type,
                argument_kind,
                ..
            }
            | InputAnnotation::Model(ModelInputAnnotation::ModelArgument {
                argument_type,
                argument_kind,
                ..
            }),
        ) => Ok((argument_type, argument_kind)),

        annotation => Err(error::InternalEngineError::UnexpectedAnnotation {
            annotation: annotation.clone(),
        }),
    }?;

    // simple values are serialized to JSON, predicates
    // are converted into NDC expressions (via our internal Expression type)
    let mapped_argument_value = match argument_kind {
        ArgumentKind::Other => {
            map_argument_value_to_ndc_type(argument_type, &argument.value, type_mappings, flags)
                .map(open_dds::query::Value::Literal)?
        }

        ArgumentKind::NDCExpression => filter::resolve_filter_expression_open_dd(
            argument.value.as_object()?,
            flags,
            usage_counts,
        )
        .map(open_dds::query::Value::BooleanExpression)?,
    };

    let argument_name = ArgumentName::new(Identifier::new(argument.name.as_str()).unwrap());

    Ok((argument_name, mapped_argument_value))
}

pub(crate) fn map_argument_value_to_ndc_type(
    value_type: &QualifiedTypeReference,
    value: &Value<GDS>,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    flags: &GraphqlIrFlags,
) -> Result<serde_json::Value, error::Error> {
    if value.is_null(&flags.validate_non_null_graphql_variables) {
        return Ok(serde_json::Value::Null);
    }

    match &value_type.underlying_type {
        QualifiedBaseType::List(element_type) => {
            let mapped_elements = value
                .as_list()?
                .iter()
                .map(|element_value| {
                    map_argument_value_to_ndc_type(
                        element_type,
                        element_value,
                        type_mappings,
                        flags,
                    )
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
                                        ..
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
                                    type_mappings,flags
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
