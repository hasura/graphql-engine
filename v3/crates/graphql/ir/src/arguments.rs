use std::collections::BTreeMap;

use crate::error;
use crate::filter;
use graphql_schema::GDS;
use graphql_schema::{Annotation, InputAnnotation, ModelInputAnnotation};
use hasura_authn_core::SessionVariables;
use lang_graphql::ast::common::Name;
use lang_graphql::normalized_ast::{InputField, Value};
use metadata_resolve::data_connectors::ArgumentPresetValue;
use metadata_resolve::{
    ArgumentKind, ArgumentNameAndPath, ArgumentPresets, DataConnectorLink, Qualified,
    QualifiedBaseType, QualifiedTypeName, QualifiedTypeReference, TypeMapping,
};
use nonempty::NonEmpty;
use open_dds::{
    data_connector::DataConnectorColumnName,
    types::{CustomTypeName, DataConnectorArgumentName, InbuiltType},
};
use plan_types::{Expression, UsagesCounts};
use reqwest::header::HeaderMap;
use serde::Serialize;

use super::error::InternalDeveloperError;
use super::permissions;

#[derive(Debug, Serialize, Clone, PartialEq)]
pub enum Argument<'s> {
    /// The argument is provided as a literal value
    Literal {
        value: serde_json::Value,
    },
    BooleanExpression {
        predicate: Expression<'s>,
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

/// Takes 'ArgumentPresets' annotations, data connector link argument presets, and
/// existing arguments (which might be partially filled), and fill values in the
/// existing arguments based on the presets
pub fn process_argument_presets<'s, 'a>(
    data_connector_link: &'s metadata_resolve::DataConnectorLink,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    argument_presets: Option<&'a ArgumentPresets>,
    data_connector_link_argument_presets: &BTreeMap<DataConnectorArgumentName, ArgumentPresetValue>,
    session_variables: &SessionVariables,
    request_headers: &HeaderMap,
    mut arguments: BTreeMap<DataConnectorArgumentName, Argument<'s>>,
    usage_counts: &mut UsagesCounts,
) -> Result<BTreeMap<DataConnectorArgumentName, Argument<'s>>, error::Error>
where
    'a: 's,
{
    if let Some(ArgumentPresets { argument_presets }) = argument_presets {
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
                data_connector_link,
                type_mappings,
                argument_value,
                field_type,
                session_variables,
                usage_counts,
            )?;

            match NonEmpty::from_slice(field_path) {
                // if field path is empty, then the entire argument has to preset
                None => {
                    arguments.insert(argument_name.clone(), actual_value);
                }
                // if there is some field path, preset the argument partially based on the field path
                Some(field_path) => {
                    if let Some(current_arg) = arguments.get_mut(&argument_name.clone()) {
                        let current_arg = match current_arg {
                            Argument::Literal { value } => Ok(value),
                            Argument::BooleanExpression { predicate: _ } => {
                                Err(error::InternalEngineError::ArgumentPresetExecution {
                                    description: "unexpected; can't merge an argument preset into an argument that has a boolean expression value"
                                        .to_owned(),
                                })
                            }
                        }?;
                        let preset_value = match actual_value {
                            Argument::Literal { value } => Ok(value),
                            Argument::BooleanExpression { predicate: _ } => {
                                // See graphql_schema::Error::BooleanExpressionInTypePresetArgument
                                Err(error::InternalEngineError::ArgumentPresetExecution {
                                    description: "unexpected; type input presets cannot contain a boolean expression preset value"
                                        .to_owned(),
                                })
                            }
                        }?;
                        if let Some(current_arg_object) = current_arg.as_object_mut() {
                            follow_field_path_and_insert_value(
                                &field_path,
                                current_arg_object,
                                preset_value,
                            )?;
                        }
                    }
                }
            }
        }
    }

    // preset arguments from `DataConnectorLink` argument presets
    for (argument_name, value) in process_connector_link_presets(
        data_connector_link_argument_presets,
        session_variables,
        request_headers,
    )? {
        arguments.insert(argument_name, Argument::Literal { value });
    }

    Ok(arguments)
}

/// Builds arguments for a command that come from a connector link's argument presets
pub fn process_connector_link_presets(
    data_connector_link_argument_presets: &BTreeMap<DataConnectorArgumentName, ArgumentPresetValue>,
    session_variables: &SessionVariables,
    request_headers: &HeaderMap,
) -> Result<BTreeMap<DataConnectorArgumentName, serde_json::Value>, error::Error> {
    let mut arguments = BTreeMap::new();
    // preset arguments from `DataConnectorLink` argument presets
    for (dc_argument_preset_name, dc_argument_preset_value) in data_connector_link_argument_presets
    {
        let mut headers_argument = serde_json::Map::new();

        // add headers from the request to be forwarded
        for header_name in &dc_argument_preset_value.http_headers.forward {
            if let Some(header_value) = request_headers.get(&header_name.0) {
                // we turn the header value into a string, which fails if it contains non-visible
                // ASCII characters: https://docs.rs/reqwest/latest/reqwest/header/struct.HeaderValue.html#method.to_str
                let string_value = header_value
                    .to_str()
                    .map_err(|_| InternalDeveloperError::IllegalCharactersInHeaderValue)?;

                // we make no attempt to parse it and pass it along as a JSON string
                let json_value = serde_json::Value::String(string_value.into());

                headers_argument.insert(header_name.0.to_string(), json_value);
            }
        }

        // add additional headers from `ValueExpression`
        for (header_name, value_expression) in &dc_argument_preset_value.http_headers.additional {
            // TODO: have helper functions to create types
            let string_type = QualifiedTypeReference {
                nullable: false,
                underlying_type: metadata_resolve::QualifiedBaseType::Named(
                    metadata_resolve::QualifiedTypeName::Inbuilt(
                        open_dds::types::InbuiltType::String,
                    ),
                ),
            };
            let value = permissions::make_argument_from_value_expression(
                value_expression,
                &string_type,
                session_variables,
            )?;
            headers_argument.insert(header_name.0.to_string(), value);
        }

        arguments.insert(
            dc_argument_preset_name.clone(),
            serde_json::Value::Object(headers_argument),
        );
    }
    Ok(arguments)
}

// fetch input values from annotations and turn them into either JSON or an Expression
pub fn build_ndc_argument_as_value<'a, 's>(
    command_field: &'a Name,
    argument: &'a InputField<'s, GDS>,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    data_connector_link: &'s DataConnectorLink,
    session_variables: &SessionVariables,
    usage_counts: &mut UsagesCounts,
) -> Result<(DataConnectorArgumentName, Argument<'s>), error::Error> {
    let (argument_type, argument_kind, ndc_argument) = match argument.info.generic {
        Annotation::Input(InputAnnotation::CommandArgument {
            argument_type,
            argument_kind,
            ndc_func_proc_argument,
        }) => Ok((argument_type, argument_kind, ndc_func_proc_argument)),
        Annotation::Input(InputAnnotation::Model(ModelInputAnnotation::ModelArgument {
            argument_type,
            argument_kind,
            ndc_table_argument,
        })) => Ok((argument_type, argument_kind, ndc_table_argument)),

        annotation => Err(error::InternalEngineError::UnexpectedAnnotation {
            annotation: annotation.clone(),
        }),
    }?;

    let ndc_argument =
        ndc_argument
            .clone()
            .ok_or_else(|| error::InternalDeveloperError::NoArgumentSource {
                field_name: command_field.clone(),
                argument_name: argument.name.clone(),
            })?;

    // simple values are serialized to JSON, predicates
    // are converted into NDC expressions (via our internal Expression type)
    let mapped_argument_value = match argument_kind {
        ArgumentKind::Other => {
            map_argument_value_to_ndc_type(argument_type, &argument.value, type_mappings)
                .map(|value| Argument::Literal { value })?
        }

        ArgumentKind::NDCExpression => filter::resolve_filter_expression(
            argument.value.as_object()?,
            data_connector_link,
            type_mappings,
            session_variables,
            usage_counts,
        )
        .map(|predicate| Argument::BooleanExpression { predicate })?,
    };
    Ok((ndc_argument, mapped_argument_value))
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

#[cfg(test)]
mod test {
    use hasura_authn_core::{
        Role, RoleAuthorization, Session, SessionVariableList, SessionVariableName,
        SessionVariableReference, SessionVariableValue,
    };
    use indexmap::IndexMap;
    use metadata_resolve::http::SerializableHeaderName;
    use metadata_resolve::{ArgumentPresetValue, HttpHeadersPreset, ValueExpression};
    use reqwest::header::{HeaderMap, HeaderValue};
    use std::collections::{BTreeMap, HashMap};
    use std::str::FromStr;

    fn make_test_session(
        client_session_variables: HashMap<SessionVariableName, SessionVariableValue>,
    ) -> Session {
        let authenticated_session_variables = HashMap::new();

        let role_authorization = RoleAuthorization {
            role: Role::new("test-role"),
            session_variables: authenticated_session_variables,
            allowed_session_variables_from_request: SessionVariableList::All,
        };

        role_authorization.build_session(client_session_variables)
    }

    #[test]
    fn test_empty_process_connector_link_presets() {
        let data_connector_link_argument_presets = BTreeMap::new();
        let session_variables = make_test_session(HashMap::new()).variables;
        let request_headers = HeaderMap::new();

        let expected = BTreeMap::new();

        assert_eq!(
            super::process_connector_link_presets(
                &data_connector_link_argument_presets,
                &session_variables,
                &request_headers
            )
            .unwrap(),
            expected
        );
    }

    #[test]
    fn headers_are_parsed_and_passed() {
        // is the header `name: Mr Horse` passed through properly?

        // what headers should we pass through and how?
        let mut data_connector_link_argument_presets = BTreeMap::new();
        let http_headers = HttpHeadersPreset {
            forward: vec![SerializableHeaderName::new("name".into()).unwrap()],
            additional: IndexMap::default(),
        };
        data_connector_link_argument_presets
            .insert("headers".into(), ArgumentPresetValue { http_headers });

        // what session variables do we have? (none)
        let session_variables = make_test_session(HashMap::new()).variables;

        // what are our input headers?
        let mut request_headers = HeaderMap::new();
        request_headers.insert("name", HeaderValue::from_static("Mr Horse"));

        // create expected response
        let mut expected = BTreeMap::new();
        let mut expected_object = serde_json::Map::new();
        expected_object.insert("name".into(), serde_json::Value::String("Mr Horse".into()));

        expected.insert("headers".into(), serde_json::Value::Object(expected_object));

        assert_eq!(
            super::process_connector_link_presets(
                &data_connector_link_argument_presets,
                &session_variables,
                &request_headers
            )
            .unwrap(),
            expected
        );
    }

    #[test]
    fn test_string_session_variable_is_passed_through() {
        // is the session variable `x-name: Mr Horse` passed through properly?

        // what should we pass through and how?
        let mut data_connector_link_argument_presets = BTreeMap::new();
        let mut additional = IndexMap::new();
        additional.insert(
            SerializableHeaderName::new("name".into()).unwrap(),
            ValueExpression::SessionVariable(SessionVariableReference {
                name: SessionVariableName::from_str("x-name").unwrap(),
                passed_as_json: false,
            }),
        );
        let http_headers = HttpHeadersPreset {
            forward: vec![],
            additional,
        };
        data_connector_link_argument_presets
            .insert("headers".into(), ArgumentPresetValue { http_headers });

        // what session variables do we have?
        let mut client_session_variables = HashMap::new();
        client_session_variables.insert(
            SessionVariableName::from_str("x-name").unwrap(),
            SessionVariableValue::new("Mr Horse"),
        );

        let session_variables = make_test_session(client_session_variables).variables;

        // what are our input headers?
        let mut request_headers = HeaderMap::new();
        request_headers.insert("name", HeaderValue::from_static("Mr Horse"));

        // create expected response
        let mut expected = BTreeMap::new();
        let mut expected_object = serde_json::Map::new();
        expected_object.insert("name".into(), serde_json::Value::String("Mr Horse".into()));
        expected.insert("headers".into(), serde_json::Value::Object(expected_object));

        assert_eq!(
            super::process_connector_link_presets(
                &data_connector_link_argument_presets,
                &session_variables,
                &request_headers
            )
            .unwrap(),
            expected
        );
    }
}
