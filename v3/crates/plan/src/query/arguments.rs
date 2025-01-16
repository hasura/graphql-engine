use super::boolean_expression;
use super::filter::resolve_filter_expression;
use super::permissions;
use hasura_authn_core::{Role, Session, SessionVariables};
use indexmap::IndexMap;
use metadata_resolve::data_connectors::ArgumentPresetValue;
use metadata_resolve::{
    unwrap_custom_type_name, ArgumentInfo, ArgumentNameAndPath, ArgumentPresets, Metadata,
    Qualified, QualifiedTypeReference, TypeMapping,
};
use nonempty::NonEmpty;
use open_dds::{
    arguments::ArgumentName,
    data_connector::DataConnectorColumnName,
    types::{CustomTypeName, DataConnectorArgumentName},
};
use plan_types::{Argument, Expression, Relationship, UniqueNumber, UsagesCounts};
use reqwest::header::HeaderMap;
use serde::Serialize;
use std::collections::BTreeMap;

use crate::error::{InternalDeveloperError, InternalEngineError, InternalError};
use crate::PlanError;

#[derive(Debug, Serialize, Clone, PartialEq)]
pub enum UnresolvedArgument<'s> {
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
) -> Result<(), InternalError> {
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
                        InternalEngineError::ArgumentPresetExecution {
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
    mut arguments: BTreeMap<DataConnectorArgumentName, UnresolvedArgument<'s>>,
    usage_counts: &mut UsagesCounts,
) -> Result<BTreeMap<DataConnectorArgumentName, UnresolvedArgument<'s>>, InternalError>
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
                InternalEngineError::ArgumentPresetExecution {
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
                            UnresolvedArgument::Literal { value } => Ok(value),
                            UnresolvedArgument::BooleanExpression { predicate: _ } => {
                                Err(InternalEngineError::ArgumentPresetExecution {
                                    description: "unexpected; can't merge an argument preset into an argument that has a boolean expression value"
                                        .to_owned(),
                                })
                            }
                        }?;
                        let preset_value = match actual_value {
                            UnresolvedArgument::Literal { value } => Ok(value),
                            UnresolvedArgument::BooleanExpression { predicate: _ } => {
                                // See graphql_schema::Error::BooleanExpressionInTypePresetArgument
                                Err(InternalEngineError::ArgumentPresetExecution {
                                    description: "unexpected; type input presets cannot contain a boolean expression preset value"
                                        .to_owned(),
                                })
                            }
                        }?;
                        match current_arg {
                            serde_json::Value::Array(current_arg_array) => {
                                // This is not an acceptable way to handle arrays, as it only works at the top level here
                                // and not down the field path. It also does not handle multiply-nested arrays. However,
                                // it's a quick fix for a common case now, and this whole area will be rewritten soon
                                for item in current_arg_array {
                                    if let serde_json::Value::Object(item_object) = item {
                                        follow_field_path_and_insert_value(
                                            &field_path,
                                            item_object,
                                            preset_value.clone(),
                                        )?;
                                    }
                                }
                            }
                            serde_json::Value::Object(current_arg_object) => {
                                follow_field_path_and_insert_value(
                                    &field_path,
                                    current_arg_object,
                                    preset_value,
                                )?;
                            }
                            serde_json::Value::Bool(_)
                            | serde_json::Value::Number(_)
                            | serde_json::Value::String(_)
                            | serde_json::Value::Null => {}
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
        arguments.insert(argument_name, UnresolvedArgument::Literal { value });
    }

    Ok(arguments)
}

/// Builds arguments for a command that come from a connector link's argument presets
pub fn process_connector_link_presets(
    data_connector_link_argument_presets: &BTreeMap<DataConnectorArgumentName, ArgumentPresetValue>,
    session_variables: &SessionVariables,
    request_headers: &HeaderMap,
) -> Result<BTreeMap<DataConnectorArgumentName, serde_json::Value>, InternalError> {
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

pub fn process_arguments(
    input_arguments: &IndexMap<ArgumentName, open_dds::query::Value>,
    argument_presets: &BTreeMap<Role, ArgumentPresets>,
    arguments: &IndexMap<ArgumentName, ArgumentInfo>,
    argument_mappings: &BTreeMap<ArgumentName, DataConnectorArgumentName>,
    data_connector: &metadata_resolve::DataConnectorLink,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    data_connector_link_argument_presets: &BTreeMap<DataConnectorArgumentName, ArgumentPresetValue>,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    metadata: &Metadata,
    usage_counts: &mut plan_types::UsagesCounts,
    relationships: &mut BTreeMap<plan_types::NdcRelationshipName, Relationship>,
    unique_number: &mut UniqueNumber,
) -> Result<BTreeMap<DataConnectorArgumentName, Argument>, PlanError> {
    let mut graphql_ir_arguments = BTreeMap::new();
    for (argument_name, argument_value) in input_arguments {
        let ndc_argument_name = argument_mappings.get(argument_name).ok_or_else(|| {
            PlanError::Internal(format!(
                "couldn't fetch argument mapping for argument {argument_name}"
            ))
        })?;

        let ndc_argument_value = match argument_value {
            open_dds::query::Value::BooleanExpression(bool_exp) => {
                let argument_info = arguments.get(argument_name).unwrap();
                let custom_type_name =
                    unwrap_custom_type_name(&argument_info.argument_type).unwrap();
                let boolean_expression_type = metadata
                    .boolean_expression_types
                    .objects
                    .get(custom_type_name)
                    .unwrap();

                // this implementation is incomplete
                // and should be filled out once we implement user-defined filters here
                let predicate =
                    boolean_expression::open_dd_boolean_expression_to_plan_types_expression(
                        bool_exp,
                        type_mappings,
                        &boolean_expression_type.object_type,
                        &data_connector.name,
                        boolean_expression_type,
                    )?;

                UnresolvedArgument::BooleanExpression { predicate }
            }
            open_dds::query::Value::Literal(value) => UnresolvedArgument::Literal {
                value: value.clone(),
            },
        };
        graphql_ir_arguments.insert(ndc_argument_name.clone(), ndc_argument_value.clone());
    }

    let argument_presets_for_role = argument_presets.get(&session.role).unwrap();

    // add any preset arguments from model permissions
    let arguments_with_presets = process_argument_presets(
        data_connector,
        type_mappings,
        Some(argument_presets_for_role),
        data_connector_link_argument_presets,
        &session.variables,
        request_headers,
        graphql_ir_arguments,
        usage_counts,
    )
    .map_err(|e| PlanError::Internal(e.to_string()))?;

    // now we turn the GraphQL IR `Arguments` type into the `execute` "resolved" argument type
    // by resolving any `Expression` types inside
    let mut resolved_arguments = BTreeMap::new();
    for (argument_name, argument_value) in &arguments_with_presets {
        let resolved_argument_value = match argument_value {
            UnresolvedArgument::BooleanExpression { predicate } => {
                let (resolved_filter_expression, _remote_predicates) =
                    resolve_filter_expression(predicate, relationships, unique_number)?;

                Argument::BooleanExpression {
                    predicate: resolved_filter_expression,
                }
            }
            UnresolvedArgument::Literal { value } => Argument::Literal {
                value: value.clone(),
            },
        };
        resolved_arguments.insert(argument_name.clone(), resolved_argument_value.clone());
    }

    Ok(resolved_arguments)
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
        client_session_variables: BTreeMap<SessionVariableName, SessionVariableValue>,
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
        let session_variables = make_test_session(BTreeMap::new()).variables;
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
        let session_variables = make_test_session(BTreeMap::new()).variables;

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
        let mut client_session_variables = BTreeMap::new();
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
