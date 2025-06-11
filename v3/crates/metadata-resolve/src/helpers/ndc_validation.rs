use std::collections::BTreeMap;

use crate::{
    data_connectors::CommandsResponseConfig,
    stages::{commands, data_connectors, models, object_types},
};
use ndc_models;
use open_dds::{
    commands::{CommandName, DataConnectorCommand, FunctionName, ProcedureName},
    data_connector::{
        CollectionName, DataConnectorColumnName, DataConnectorName, DataConnectorScalarType,
    },
    models::ModelName,
    types::{CustomTypeName, DataConnectorArgumentName, FieldName},
};

use crate::types::subgraph::{Qualified, QualifiedTypeName, QualifiedTypeReference};

#[derive(Debug, thiserror::Error)]
pub enum NDCValidationError {
    #[error("collection {collection_name} is not defined in data connector {db_name}")]
    NoSuchCollection {
        db_name: Qualified<DataConnectorName>,
        model_name: Qualified<ModelName>,
        collection_name: CollectionName,
    },
    #[error(
        "argument {argument_name} is not defined for collection {collection_name} in data connector {db_name}"
    )]
    NoSuchArgument {
        db_name: Qualified<DataConnectorName>,
        collection_name: CollectionName,
        argument_name: DataConnectorArgumentName,
    },
    #[error(
        "argument {argument_name} is not defined for function/procedure {func_proc_name} in data connector {db_name}"
    )]
    NoSuchArgumentForCommand {
        db_name: Qualified<DataConnectorName>,
        func_proc_name: String,
        argument_name: DataConnectorArgumentName,
    },
    #[error(
        "column {column_name} is not defined in collection {collection_name} in data connector {db_name}"
    )]
    NoSuchColumn {
        db_name: Qualified<DataConnectorName>,
        model_name: Qualified<ModelName>,
        field_name: FieldName,
        collection_name: CollectionName,
        column_name: DataConnectorColumnName,
    },
    #[error("procedure {procedure_name} is not defined in data connector {db_name}")]
    NoSuchProcedure {
        db_name: Qualified<DataConnectorName>,
        command_name: Qualified<CommandName>,
        procedure_name: ProcedureName,
    },
    #[error("function {function_name} is not defined in data connector {db_name}")]
    NoSuchFunction {
        db_name: Qualified<DataConnectorName>,
        command_name: Qualified<CommandName>,
        function_name: FunctionName,
    },
    #[error(
        "column {column_name} is not defined in function/procedure {func_proc_name} in data connector {db_name}"
    )]
    NoSuchColumnForCommand {
        db_name: Qualified<DataConnectorName>,
        command_name: Qualified<CommandName>,
        field_name: FieldName,
        func_proc_name: String,
        column_name: DataConnectorColumnName,
    },
    #[error(
        "column {column_name} has type {column_type} in collection {collection_name} in data connector {db_name}, not type {field_type}"
    )]
    ColumnTypeDoesNotMatch {
        db_name: DataConnectorName,
        model_name: ModelName,
        field_name: FieldName,
        collection_name: CollectionName,
        column_name: DataConnectorColumnName,
        field_type: String,
        column_type: String,
    },
    #[error(
        "internal error: data connector does not define the scalar type {type}, used by field {field_name} in model {model_name}"
    )]
    TypeCapabilityNotDefined {
        model_name: ModelName,
        field_name: FieldName,
        r#type: String,
    },
    #[error("type {0} is not defined in the agent schema")]
    NoSuchType(String),
    #[error("mapping for type {type_name} of model {model_name} is not defined")]
    UnknownModelTypeMapping {
        model_name: Qualified<ModelName>,
        type_name: Qualified<CustomTypeName>,
    },
    #[error("mapping for type {type_name} of command {command_name} is not defined")]
    UnknownCommandTypeMapping {
        command_name: Qualified<CommandName>,
        type_name: Qualified<CustomTypeName>,
    },
    #[error(
        "Field {field_name} for type {type_name} referenced in model {model_name} is not defined"
    )]
    UnknownTypeField {
        model_name: ModelName,
        type_name: CustomTypeName,
        field_name: FieldName,
    },
    #[error(
        "Result type of function/procedure {function_or_procedure_name:} is {function_or_procedure_output_type:} but output type of command {command_name:} is {command_output_type:}"
    )]
    FuncProcAndCommandScalarOutputTypeMismatch {
        function_or_procedure_name: String,
        function_or_procedure_output_type: String,
        command_name: String,
        command_output_type: String,
    },
    #[error(
        "Custom result type of function {function_or_procedure_name:} does not match custom output type of command: {command_name:}"
    )]
    FuncProcAndCommandCustomOutputTypeMismatch {
        function_or_procedure_name: String,
        command_name: String,
    },
    #[error("data connector does not support queries")]
    QueryCapabilityUnsupported,
    #[error("data connector does not support mutations")]
    MutationCapabilityUnsupported,

    // for `DataConnectorLink.argumentPresets` not all type representations are supported.
    #[error(
        "Unsupported type representation {representation:} in scalar type {scalar_type:}, for argument preset name {argument_name:}. Only 'json' representation is supported."
    )]
    UnsupportedTypeInDataConnectorLinkArgumentPreset {
        representation: String,
        scalar_type: DataConnectorScalarType,
        argument_name: open_dds::types::DataConnectorArgumentName,
    },

    #[error(
        "Argument '{argument_name:}' used in argument mapping for the field '{field_name:}' in object type '{object_type_name:}' is not defined in the data connector '{data_connector_name:}'."
    )]
    NoSuchArgumentInNDCArgumentMapping {
        argument_name: open_dds::arguments::ArgumentName,
        field_name: FieldName,
        object_type_name: Qualified<CustomTypeName>,
        data_connector_name: Qualified<DataConnectorName>,
    },

    #[error(
        "Field {field_name:} not found in object type {object_type:} in data connector {data_connector_name:}."
    )]
    NoSuchFieldInObjectType {
        data_connector_name: Qualified<DataConnectorName>,
        field_name: String,
        object_type: String,
    },

    #[error("Internal error while serializing error message. Error: {err:}")]
    InternalSerializationError { err: serde_json::Error },
}

pub fn validate_ndc(
    model_name: &Qualified<ModelName>,
    model: &models::Model,
    schema: &data_connectors::DataConnectorSchema,
) -> std::result::Result<(), NDCValidationError> {
    let Some(model_source) = &model.source else {
        return Ok(());
    };
    let db = &model_source.data_connector;

    let collection_name = &model_source.collection;

    let collection = schema
        .collections
        .get(collection_name.as_str())
        .ok_or_else(|| NDCValidationError::NoSuchCollection {
            db_name: db.name.clone(),
            model_name: model_name.clone(),
            collection_name: collection_name.clone(),
        })?;

    for mapped_argument_name in model_source.argument_mappings.values() {
        if !collection
            .arguments
            .contains_key(mapped_argument_name.as_str())
        {
            return Err(NDCValidationError::NoSuchArgument {
                db_name: db.name.clone(),
                collection_name: collection_name.clone(),
                argument_name: mapped_argument_name.clone(),
            });
        }
        // TODO: Add type validation for arguments
    }

    let collection_type = schema
        .object_types
        .get(&collection.collection_type)
        .ok_or_else(|| {
            NDCValidationError::NoSuchType(collection.collection_type.as_str().to_owned())
        })?;

    let object_types::TypeMapping::Object { field_mappings, .. } = model_source
        .type_mappings
        .get(&model.data_type)
        .ok_or_else(|| NDCValidationError::UnknownModelTypeMapping {
            model_name: model_name.clone(),
            type_name: model.data_type.clone(),
        })?;
    for (field_name, field_mapping) in field_mappings.iter() {
        let column_name = &field_mapping.column;
        let column = collection_type
            .fields
            .get(column_name.as_str())
            .ok_or_else(|| NDCValidationError::NoSuchColumn {
                db_name: db.name.clone(),
                model_name: model_name.clone(),
                field_name: field_name.clone(),
                collection_name: collection_name.clone(),
                column_name: column_name.clone(),
            })?;
        // Check if the arguments in the mapping are valid
        for (open_dd_argument_name, dc_argument_name) in &field_mapping.argument_mappings {
            if !column.arguments.contains_key(dc_argument_name.as_str()) {
                return Err(NDCValidationError::NoSuchArgumentInNDCArgumentMapping {
                    argument_name: open_dd_argument_name.clone(),
                    field_name: field_name.clone(),
                    object_type_name: model.data_type.clone(),
                    data_connector_name: db.name.clone(),
                });
            }
        }
        // if field_mapping.field_mapping.column_type != column.r#type {
        //     Err(NDCValidationError::ColumnTypeDoesNotMatch {
        //         db_name: db.name.clone(),
        //         model_name: model_name.clone(),
        //         field_name: field_name.clone(),
        //         collection_name: collection_path.clone(),
        //         column_name: column_name.clone(),
        //         field_type: field_mapping.field_mapping.column_type.clone(),
        //         column_type: column.r#type.clone(),
        //     })?
        // }
        // let gdc_type = schema
        //     .scalar_types
        //     .get(column.r#type.as_str())
        //     .ok_or_else(|| NDCValidationError::TypeCapabilityNotDefined {
        //         model_name: model_name.clone(),
        //         field_name: field_name.clone(),
        //         r#type: column.r#type.clone(),
        //     })?;

        // let gds_type = &fields
        //     .get(field_name)
        //     .ok_or_else(|| NDCValidationError::UnknownTypeField {
        //         model_name: model_name.clone(),
        //         type_name: model.data_type.clone(),
        //         field_name: field_name.clone(),
        //     })?
        //     .field_type;
        // if let Some(graphql_type) = gdc_type.graphql_type {
        //     match (graphql_type, gds_type) {
        //         (GraphQlType::Int, GdsType::Inbuilt(InbuiltType::Int)) => Ok(()),
        //         (GraphQlType::Float, GdsType::Inbuilt(InbuiltType::Float)) => Ok(()),
        //         (GraphQlType::String, GdsType::Inbuilt(InbuiltType::String)) => Ok(()),
        //         (GraphQlType::Boolean, GdsType::Inbuilt(InbuiltType::Boolean)) => Ok(()),
        //         _ => Err(NDCValidationError::FieldGraphQLTypeDoesNotMatch {
        //             model_name: model_name.clone(),
        //             field_name: field_name.clone(),
        //             field_type: gds_type.clone(),
        //             graphql_type,
        //         }),
        //     }?
        // }
    }
    Ok(())
}

// Validate the mappings b/w dds object and ndc objects present in command source.
pub fn validate_ndc_command(
    command_name: &Qualified<CommandName>,
    command_source: &commands::CommandSource,
    command_output_type: &QualifiedTypeReference,
    schema: &data_connectors::DataConnectorSchema,
    commands_response_config: Option<&CommandsResponseConfig>,
) -> Result<bool, NDCValidationError> {
    // is the source type or open
    let mut source_type_open_dd_type_same = true;

    let db = &command_source.data_connector;

    let (
        command_source_func_proc_name,
        command_source_ndc_arguments,
        command_source_ndc_result_type,
    ) = match &command_source.source {
        DataConnectorCommand::Procedure(procedure) => {
            let command_source_ndc = schema.procedures.get(procedure).ok_or_else(|| {
                NDCValidationError::NoSuchProcedure {
                    db_name: db.name.clone(),
                    command_name: command_name.clone(),
                    procedure_name: procedure.clone(),
                }
            })?;

            (
                procedure.as_str(),
                command_source_ndc.arguments.clone(),
                &command_source_ndc.result_type,
            )
        }

        DataConnectorCommand::Function(function) => {
            let command_source_ndc = schema.functions.get(function).ok_or_else(|| {
                NDCValidationError::NoSuchFunction {
                    db_name: db.name.clone(),
                    command_name: command_name.clone(),
                    function_name: function.clone(),
                }
            })?;

            (
                function.as_str(),
                command_source_ndc.arguments.clone(),
                &command_source_ndc.result_type,
            )
        }
    };

    // Check if the arguments are correctly mapped
    for ndc_argument_name in command_source.argument_mappings.values() {
        if !command_source_ndc_arguments.contains_key(ndc_argument_name.as_str()) {
            return Err(NDCValidationError::NoSuchArgumentForCommand {
                db_name: db.name.clone(),
                func_proc_name: command_source_func_proc_name.to_owned(),
                argument_name: ndc_argument_name.clone(),
            });
        }
    }

    // Validate if the result type of function/procedure exists in the schema types(scalar + object)
    let command_source_ndc_result_type_name =
        get_underlying_named_type(command_source_ndc_result_type);
    if !(schema
        .scalar_types
        .contains_key(command_source_ndc_result_type_name.as_str())
        || schema
            .object_types
            .contains_key(command_source_ndc_result_type_name.as_str()))
    {
        return Err(NDCValidationError::NoSuchType(
            command_source_ndc_result_type_name.to_string(),
        ));
    }

    // Check if the result_type of function/procedure actually has a scalar type or an object type.
    // If it is an object type, then validate the type mapping.
    match command_output_type.get_underlying_type_name() {
        QualifiedTypeName::Inbuilt(_command_output_type) => {
            // TODO: Validate that the type of command.output_type is
            // same as the &command_source_ndc.result_type
        }
        QualifiedTypeName::Custom(custom_type) => {
            match schema
                .object_types
                .get(command_source_ndc_result_type_name.as_str())
            {
                // Check if the command.output_type is available in schema.object_types
                Some(command_source_ndc_type) => {
                    let actual_command_source_type = resolve_actual_command_source_type(
                        commands_response_config,
                        command_source_ndc_type,
                        schema,
                        command_source_ndc_result_type_name,
                        &db.name,
                    )?;
                    source_type_open_dd_type_same =
                        actual_command_source_type == command_source_ndc_type;
                    // Check if the command.output_type has typeMappings
                    let object_types::TypeMapping::Object { field_mappings, .. } = command_source
                        .type_mappings
                        .get(custom_type)
                        .ok_or_else(|| NDCValidationError::UnknownCommandTypeMapping {
                            command_name: command_name.clone(),
                            type_name: custom_type.clone(),
                        })?;
                    // Check if the field mappings for the output_type is valid
                    for (field_name, field_mapping) in field_mappings.iter() {
                        let column_name = &field_mapping.column;
                        if !actual_command_source_type
                            .fields
                            .contains_key(column_name.as_str())
                        {
                            return Err(NDCValidationError::NoSuchColumnForCommand {
                                db_name: db.name.clone(),
                                command_name: command_name.clone(),
                                field_name: field_name.clone(),
                                func_proc_name: command_source_func_proc_name.to_owned(),
                                column_name: column_name.clone(),
                            });
                        }
                    }
                }
                // If the command.output_type is not available in schema.object_types, then check if it is available in the schema.scalar_types
                // else raise an NDCValidationError error
                None => match schema
                    .scalar_types
                    .get(command_source_ndc_result_type_name.as_str())
                {
                    Some(_command_source_ndc_type) => (),
                    None => Err(NDCValidationError::NoSuchType(
                        command_source_ndc_result_type_name.to_string(),
                    ))?,
                },
            }
        }
    }
    Ok(source_type_open_dd_type_same)
}

/// When `CommandsResponseConfig` is configured, a source/procedure's result
/// type may not match the output type of a corresponding Command. The result
/// type of the NDC function/procedure would be an object type with "headers"
/// and "result" fields. And "result" field's type is what should match the
/// Command's output type.
pub fn resolve_actual_command_source_type<'a>(
    commands_response_config: Option<&CommandsResponseConfig>,
    command_source_ndc_type: &'a ndc_models::ObjectType,
    schema: &'a data_connectors::DataConnectorSchema,
    command_source_ndc_type_name: &ndc_models::TypeName,
    db_name: &Qualified<DataConnectorName>,
) -> Result<&'a ndc_models::ObjectType, NDCValidationError> {
    match commands_response_config {
        None => Ok(command_source_ndc_type),
        Some(response_config) => {
            if command_source_ndc_type
                .fields
                .contains_key(response_config.headers_field.as_str())
                && command_source_ndc_type
                    .fields
                    .contains_key(response_config.result_field.as_str())
            {
                let result_field = command_source_ndc_type
                    .fields
                    .get(response_config.result_field.as_str())
                    .ok_or_else(|| NDCValidationError::NoSuchFieldInObjectType {
                        data_connector_name: db_name.clone(),
                        field_name: response_config.result_field.as_str().to_owned(),
                        object_type: command_source_ndc_type_name.to_string(),
                    })?;

                let type_name = get_underlying_named_type(&result_field.r#type);
                schema
                    .object_types
                    .get(type_name.as_str())
                    .ok_or_else(|| NDCValidationError::NoSuchType(type_name.as_str().to_owned()))
            } else {
                Ok(command_source_ndc_type)
            }
        }
    }
}

/// Validate argument presets of a 'DataConnectorLink' with NDC schema
pub(crate) fn validate_ndc_argument_presets(
    argument_presets: &Vec<data_connectors::ArgumentPreset>,
    schema: &data_connectors::DataConnectorSchema,
) -> Result<(), NDCValidationError> {
    for argument_preset in argument_presets {
        for function_info in schema.functions.values() {
            validate_argument_preset_type(&argument_preset.name, &function_info.arguments, schema)?;
        }

        for procedure_info in schema.procedures.values() {
            validate_argument_preset_type(
                &argument_preset.name,
                &procedure_info.arguments,
                schema,
            )?;
        }
    }
    Ok(())
}

// The type of an argument preset (in argument presets of the data connector), cannot be
// completely arbitrary. As engine would have to map the request headers (and other additional
// headers) to this type. Ideally we would introduce a "map" representation in NDC. So, in JSON
// transport the "map" can be represented as a JSON key-value object and in, say protobuf, it
// can represented as a protobuf map type. But, for now if this scalar type has a representation
// other than "json", we error out. Later if we added a "map" type then we would support both
// "map" and "json".
fn validate_argument_preset_type(
    preset_argument_name: &open_dds::types::DataConnectorArgumentName,
    arguments: &BTreeMap<ndc_models::ArgumentName, ndc_models::ArgumentInfo>,
    schema: &data_connectors::DataConnectorSchema,
) -> Result<(), NDCValidationError> {
    for (arg_name, arg_info) in arguments {
        if arg_name.as_str() == preset_argument_name.as_str() {
            let type_name = get_underlying_named_type(&arg_info.argument_type);
            let scalar_type = schema
                .scalar_types
                .get(type_name.as_str())
                .ok_or_else(|| NDCValidationError::NoSuchType(type_name.as_str().to_owned()))?;

            if scalar_type.representation != ndc_models::TypeRepresentation::JSON {
                return Err(
                    NDCValidationError::UnsupportedTypeInDataConnectorLinkArgumentPreset {
                        representation: serde_json::to_string(&scalar_type.representation)
                            .map_err(|e| NDCValidationError::InternalSerializationError {
                                err: e,
                            })?,
                        scalar_type: DataConnectorScalarType::from(type_name.as_str()),
                        argument_name: preset_argument_name.clone(),
                    },
                );
            }
        }
    }
    Ok(())
}

pub fn get_underlying_named_type(result_type: &ndc_models::Type) -> &ndc_models::TypeName {
    match result_type {
        ndc_models::Type::Named { name } => name,
        ndc_models::Type::Array { element_type } => get_underlying_named_type(element_type),
        ndc_models::Type::Nullable { underlying_type } => {
            get_underlying_named_type(underlying_type)
        }
        ndc_models::Type::Predicate { object_type_name } => object_type_name.as_ref(),
    }
}
