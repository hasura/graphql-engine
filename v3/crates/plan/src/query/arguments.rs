use super::boolean_expression;
use super::filter::resolve_filter_expression;
use super::permissions;
use hasura_authn_core::{Role, Session, SessionVariables};
use indexmap::IndexMap;
use metadata_resolve::data_connectors::ArgumentPresetValue;
use metadata_resolve::{
    unwrap_custom_type_name, ArgumentInfo, CommandWithPermissions, Metadata, ModelWithPermissions,
    ObjectTypeWithRelationships, Qualified, QualifiedBaseType, QualifiedTypeReference, TypeMapping,
    ValueExpressionOrPredicate,
};
use open_dds::{
    arguments::ArgumentName,
    commands::CommandName,
    models::ModelName,
    types::{CustomTypeName, DataConnectorArgumentName, FieldName},
};
use plan_types::{Argument, Expression, Relationship, UniqueNumber, UsagesCounts};
use reqwest::header::HeaderMap;
use serde::Serialize;
use std::borrow::Cow;
use std::collections::BTreeMap;

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

pub fn process_argument_presets_for_model<'s>(
    arguments: BTreeMap<DataConnectorArgumentName, UnresolvedArgument<'s>>,
    model: &'s ModelWithPermissions,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
    session: &Session,
    request_headers: &HeaderMap,
    usage_counts: &mut UsagesCounts,
) -> Result<BTreeMap<DataConnectorArgumentName, UnresolvedArgument<'s>>, PlanError> {
    let model_source = model.model.source.as_ref().ok_or_else(|| {
        ArgumentPresetExecutionError::ModelSourceNotFound {
            model_name: model.model.name.clone(),
        }
    })?;

    let argument_presets = &model
        .select_permissions
        .get(&session.role)
        .ok_or_else(
            || ArgumentPresetExecutionError::ModelArgumentPresetsNotFound {
                model_name: model.model.name.clone(),
                role: session.role.clone(),
            },
        )?
        .argument_presets;

    process_argument_presets(
        arguments,
        &model.model.arguments,
        &model_source.argument_mappings,
        argument_presets,
        object_types,
        &model_source.type_mappings,
        &model_source.data_connector,
        &model_source.data_connector_link_argument_presets,
        session,
        request_headers,
        usage_counts,
    )
}

pub fn process_argument_presets_for_command<'s>(
    arguments: BTreeMap<DataConnectorArgumentName, UnresolvedArgument<'s>>,
    command: &'s CommandWithPermissions,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
    session: &Session,
    request_headers: &HeaderMap,
    usage_counts: &mut UsagesCounts,
) -> Result<BTreeMap<DataConnectorArgumentName, UnresolvedArgument<'s>>, PlanError> {
    let command_source = command.command.source.as_ref().ok_or_else(|| {
        ArgumentPresetExecutionError::CommandSourceNotFound {
            command_name: command.command.name.clone(),
        }
    })?;

    let argument_presets = &command
        .permissions
        .get(&session.role)
        .ok_or_else(
            || ArgumentPresetExecutionError::CommandArgumentPresetsNotFound {
                command_name: command.command.name.clone(),
                role: session.role.clone(),
            },
        )?
        .argument_presets;

    process_argument_presets(
        arguments,
        &command.command.arguments,
        &command_source.argument_mappings,
        argument_presets,
        object_types,
        &command_source.type_mappings,
        &command_source.data_connector,
        &command_source.data_connector_link_argument_presets,
        session,
        request_headers,
        usage_counts,
    )
}

fn process_argument_presets<'s>(
    mut arguments: BTreeMap<DataConnectorArgumentName, UnresolvedArgument<'s>>,
    argument_infos: &IndexMap<ArgumentName, ArgumentInfo>,
    argument_mappings: &BTreeMap<ArgumentName, DataConnectorArgumentName>,
    argument_presets: &'s BTreeMap<
        ArgumentName,
        (QualifiedTypeReference, ValueExpressionOrPredicate),
    >,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    data_connector_link: &'s metadata_resolve::DataConnectorLink,
    data_connector_link_argument_presets: &BTreeMap<DataConnectorArgumentName, ArgumentPresetValue>,
    session: &Session,
    request_headers: &HeaderMap,
    usage_counts: &mut UsagesCounts,
) -> Result<BTreeMap<DataConnectorArgumentName, UnresolvedArgument<'s>>, PlanError> {
    // Preset arguments from `DataConnectorLink` argument presets
    for (argument_name, value) in process_connector_link_presets(
        data_connector_link_argument_presets,
        &session.variables,
        request_headers,
    )? {
        arguments.insert(argument_name, UnresolvedArgument::Literal { value });
    }

    // Preset arguments from Model/CommandPermission argument presets
    for (argument_name, (field_type, argument_value)) in argument_presets {
        let data_connector_argument_name =
            argument_mappings.get(argument_name).ok_or_else(|| {
                ArgumentPresetExecutionError::ArgumentMappingNotFound {
                    argument_name: argument_name.clone(),
                }
            })?;

        let argument_value = permissions::make_argument_from_value_expression_or_predicate(
            data_connector_link,
            type_mappings,
            argument_value,
            field_type,
            &session.variables,
            usage_counts,
        )?;

        arguments.insert(data_connector_argument_name.clone(), argument_value);
    }

    // Apply input field presets from the TypePermissions involved in the arguments' types
    for (argument_name, argument_info) in argument_infos {
        let data_connector_argument_name =
            argument_mappings.get(argument_name).ok_or_else(|| {
                ArgumentPresetExecutionError::ArgumentMappingNotFound {
                    argument_name: argument_name.clone(),
                }
            })?;

        if let Some(existing_argument_value) = arguments.get_mut(data_connector_argument_name) {
            match existing_argument_value {
                UnresolvedArgument::Literal { value } => {
                    apply_input_field_presets_to_value(
                        value,
                        &argument_info.argument_type,
                        type_mappings,
                        object_types,
                        session,
                    )?;
                }
                UnresolvedArgument::BooleanExpression { .. } => {
                    // We don't apply input field presets to boolean expression arguments
                }
            };
        }
    }

    Ok(arguments)
}

fn apply_input_field_presets_to_value(
    value: &mut serde_json::Value,
    type_reference: &QualifiedTypeReference,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
    session: &Session,
) -> Result<(), PlanError> {
    match &type_reference.underlying_type {
        QualifiedBaseType::List(list_element_type) => {
            let Some(array_elements) = get_value_array_or_null(value, type_reference)? else {
                return Ok(()); // Nothing to do if the value is null
            };

            // Apply presets to each element in the array
            for element_value in array_elements {
                apply_input_field_presets_to_value(
                    element_value,
                    list_element_type,
                    type_mappings,
                    object_types,
                    session,
                )?;
            }
        }
        QualifiedBaseType::Named(qualified_type_name) => {
            // Get the object type information
            let Some((object_type_name, object_type_info)) = qualified_type_name
                .get_custom_type_name()
                .and_then(|type_name| {
                    object_types
                        .get(type_name)
                        .map(|object_type_info| (type_name, object_type_info))
                })
            else {
                return Ok(()); // This is not an object type, so there are no input field presets to apply, skip it
            };

            let object_value =
                if let Some(object_value) = get_value_object_or_null(value, type_reference)? {
                    object_value
                } else {
                    // If the value is null, let's see if it is allowed to be null
                    if type_reference.nullable {
                        return Ok(()); // Null is allowed, so we can keep the null and skip the presets
                    }

                    // The value is not allowed to be null, so we create an empty object to fill with presets
                    // Because we received a null here, the object will be entirely populated with presets
                    *value = serde_json::Value::Object(serde_json::Map::new());
                    value.as_object_mut().unwrap() // This is safe because we just created an object value
                };

            // Get the input permissions for this object type for the current role
            let field_presets = object_type_info
                .type_input_permissions
                .get(&session.role)
                .map_or_else(
                    || Cow::Owned(BTreeMap::new()),
                    |input_permissions| Cow::Borrowed(&input_permissions.field_presets),
                );

            // Get the data connector type mapping for this object type
            let TypeMapping::Object { field_mappings, .. } = type_mappings
                .get(object_type_name)
                .ok_or_else(|| ArgumentPresetExecutionError::TypeMappingNotFound {
                    object_type_name: object_type_name.clone(),
                })?;

            // Apply all input field presets to the object value
            for (field_name, field_preset) in field_presets.as_ref() {
                // Get the data connector field mapping for this field
                let field_mapping = field_mappings.get(field_name).ok_or_else(|| {
                    ArgumentPresetExecutionError::FieldMappingNotFound {
                        field_name: field_name.clone(),
                        object_type_name: object_type_name.clone(),
                    }
                })?;

                // Get the type information about the field
                let field_info = object_type_info
                    .object_type
                    .fields
                    .get(field_name)
                    .ok_or_else(|| ArgumentPresetExecutionError::FieldDefinitionNotFound {
                        field_name: field_name.clone(),
                        object_type_name: object_type_name.clone(),
                    })?;

                let argument_value = permissions::make_argument_from_value_expression(
                    &field_preset.value,
                    &field_info.field_type,
                    &session.variables,
                )?;

                object_value.insert(field_mapping.column.as_str().to_owned(), argument_value);
            }

            // Recur and apply input field presets to the values of all the object fields
            for (field_name, field_info) in &object_type_info.object_type.fields {
                // Get the data connector field mapping for this field
                let field_mapping = field_mappings.get(field_name).ok_or_else(|| {
                    ArgumentPresetExecutionError::DataConnectorFieldMappingNotFound {
                        field_name: field_name.clone(),
                        object_type_name: object_type_name.clone(),
                    }
                })?;

                // Get the object field value; if it is missing, insert a null value
                if let Some(field_value) = object_value.get_mut(field_mapping.column.as_str()) {
                    apply_input_field_presets_to_value(
                        field_value,
                        &field_info.field_type,
                        type_mappings,
                        object_types,
                        session,
                    )?;
                } else {
                    let mut field_value = serde_json::Value::Null;

                    apply_input_field_presets_to_value(
                        &mut field_value,
                        &field_info.field_type,
                        type_mappings,
                        object_types,
                        session,
                    )?;

                    // If the field value is still null, don't insert it into the object
                    // Technically we should be able to do, but bugs in the postgres connector
                    // mean that postgres treats null vs missing differently, even though it shouldn't
                    if !field_value.is_null() {
                        object_value.insert(field_mapping.column.as_str().to_owned(), field_value);
                    }
                }
            }
        }
    }

    Ok(())
}

#[derive(Debug, thiserror::Error)]
pub enum ArgumentPresetExecutionError {
    #[error("expected {expected_type} but got a boolean")]
    GotBoolean {
        expected_type: QualifiedTypeReference,
    },
    #[error("expected {expected_type} but got a number")]
    GotNumber {
        expected_type: QualifiedTypeReference,
    },
    #[error("expected {expected_type} but got a string")]
    GotString {
        expected_type: QualifiedTypeReference,
    },
    #[error("expected {expected_type} but got an object")]
    GotObject {
        expected_type: QualifiedTypeReference,
    },
    #[error("expected {expected_type} but got an array")]
    GotArray {
        expected_type: QualifiedTypeReference,
    },
    #[error("could not convert the provided header value to string as it contains non-visible ASCII characters")]
    IllegalCharactersInHeaderValue,
    #[error("Model source not found for model '{model_name}'")]
    ModelSourceNotFound { model_name: Qualified<ModelName> },
    #[error("Model permissions for model {model_name} not found for role {role}")]
    ModelArgumentPresetsNotFound {
        role: Role,
        model_name: Qualified<ModelName>,
    },

    #[error("command {command_name} does not have a source defined")]
    CommandSourceNotFound {
        command_name: Qualified<CommandName>,
    },
    #[error("command permissions for command {command_name} not found for role {role}")]
    CommandArgumentPresetsNotFound {
        command_name: Qualified<CommandName>,
        role: Role,
    },
    #[error("argument mapping not found for {argument_name}")]
    ArgumentMappingNotFound { argument_name: ArgumentName },
    #[error("type mapping not found for object {object_type_name}")]
    TypeMappingNotFound {
        object_type_name: Qualified<CustomTypeName>,
    },
    #[error("no data connector field mapping found for field '{field_name}' of object type '{object_type_name}'")]
    FieldMappingNotFound {
        object_type_name: Qualified<CustomTypeName>,
        field_name: FieldName,
    },
    #[error(
        "no field definition found for field '{field_name}' of object type '{object_type_name}'"
    )]
    FieldDefinitionNotFound {
        object_type_name: Qualified<CustomTypeName>,
        field_name: FieldName,
    },
    #[error("no data connector field mapping found for field '{field_name}' of object type '{object_type_name}'")]
    DataConnectorFieldMappingNotFound {
        object_type_name: Qualified<CustomTypeName>,
        field_name: FieldName,
    },
}

fn get_value_array_or_null<'a>(
    value: &'a mut serde_json::Value,
    expected_type: &QualifiedTypeReference,
) -> Result<Option<&'a mut Vec<serde_json::Value>>, ArgumentPresetExecutionError> {
    match value {
        serde_json::Value::Array(array) => Ok(Some(array)),
        serde_json::Value::Null => Ok(None),
        serde_json::Value::Bool(_) => Err(ArgumentPresetExecutionError::GotBoolean {
            expected_type: expected_type.clone(),
        }),
        serde_json::Value::Number(_) => Err(ArgumentPresetExecutionError::GotNumber {
            expected_type: expected_type.clone(),
        }),
        serde_json::Value::String(_) => Err(ArgumentPresetExecutionError::GotString {
            expected_type: expected_type.clone(),
        }),
        serde_json::Value::Object(_) => Err(ArgumentPresetExecutionError::GotObject {
            expected_type: expected_type.clone(),
        }),
    }
}

fn get_value_object_or_null<'a>(
    value: &'a mut serde_json::Value,
    expected_type: &QualifiedTypeReference,
) -> Result<Option<&'a mut serde_json::Map<String, serde_json::Value>>, ArgumentPresetExecutionError>
{
    match value {
        serde_json::Value::Object(map) => Ok(Some(map)),
        serde_json::Value::Null => Ok(None),
        serde_json::Value::Bool(_) => Err(ArgumentPresetExecutionError::GotBoolean {
            expected_type: expected_type.clone(),
        }),
        serde_json::Value::Number(_) => Err(ArgumentPresetExecutionError::GotNumber {
            expected_type: expected_type.clone(),
        }),
        serde_json::Value::String(_) => Err(ArgumentPresetExecutionError::GotString {
            expected_type: expected_type.clone(),
        }),
        serde_json::Value::Array(_) => Err(ArgumentPresetExecutionError::GotArray {
            expected_type: expected_type.clone(),
        }),
    }
}

/// Builds arguments for a command that come from a connector link's argument presets
pub fn process_connector_link_presets(
    data_connector_link_argument_presets: &BTreeMap<DataConnectorArgumentName, ArgumentPresetValue>,
    session_variables: &SessionVariables,
    request_headers: &HeaderMap,
) -> Result<BTreeMap<DataConnectorArgumentName, serde_json::Value>, PlanError> {
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
                    .map_err(|_| ArgumentPresetExecutionError::IllegalCharactersInHeaderValue)?;

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

pub fn get_unresolved_arguments<'s>(
    input_arguments: &IndexMap<ArgumentName, open_dds::query::Value>,
    argument_infos: &IndexMap<ArgumentName, ArgumentInfo>,
    argument_mappings: &BTreeMap<ArgumentName, DataConnectorArgumentName>,
    metadata: &Metadata,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    data_connector: &metadata_resolve::DataConnectorLink,
) -> Result<BTreeMap<DataConnectorArgumentName, UnresolvedArgument<'s>>, PlanError> {
    let mut arguments = BTreeMap::new();
    for (argument_name, argument_value) in input_arguments {
        let ndc_argument_name = argument_mappings.get(argument_name).ok_or_else(|| {
            PlanError::Internal(format!(
                "couldn't fetch argument mapping for argument {argument_name}"
            ))
        })?;

        let ndc_argument_value = match argument_value {
            open_dds::query::Value::BooleanExpression(bool_exp) => {
                let argument_info = argument_infos.get(argument_name).unwrap();
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
        arguments.insert(ndc_argument_name.clone(), ndc_argument_value.clone());
    }
    Ok(arguments)
}

pub fn resolve_arguments(
    arguments_with_presets: BTreeMap<DataConnectorArgumentName, UnresolvedArgument<'_>>,
    relationships: &mut BTreeMap<plan_types::NdcRelationshipName, Relationship>,
    unique_number: &mut UniqueNumber,
) -> Result<BTreeMap<DataConnectorArgumentName, Argument>, PlanError> {
    // now we turn the GraphQL IR `Arguments` type into the `execute` "resolved" argument type
    // by resolving any `Expression` types inside
    let mut resolved_arguments = BTreeMap::new();
    for (argument_name, argument_value) in arguments_with_presets {
        let resolved_argument_value = match argument_value {
            UnresolvedArgument::BooleanExpression { predicate } => {
                let (resolved_filter_expression, _remote_predicates) =
                    resolve_filter_expression(&predicate, relationships, unique_number)?;

                Argument::BooleanExpression {
                    predicate: resolved_filter_expression,
                }
            }
            UnresolvedArgument::Literal { value } => Argument::Literal {
                value: value.clone(),
            },
        };
        resolved_arguments.insert(argument_name, resolved_argument_value.clone());
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
