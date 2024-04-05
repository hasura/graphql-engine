use ndc_models;
use open_dds::{
    commands::{CommandName, DataConnectorCommand, FunctionName, ProcedureName},
    data_connector::DataConnectorName,
    models::ModelName,
    types::{CustomTypeName, FieldName},
};
use thiserror::Error;

use super::{
    command::Command,
    model::Model,
    subgraph::{Qualified, QualifiedBaseType, QualifiedTypeName, QualifiedTypeReference},
};

#[derive(Debug, Error)]
pub enum NDCValidationError {
    #[error("collection {collection_name} is not defined in data connector {db_name}")]
    NoSuchCollection {
        db_name: Qualified<DataConnectorName>,
        model_name: Qualified<ModelName>,
        collection_name: String,
    },
    #[error(
        "argument {argument_name} is not defined for collection {collection_name} in data connector {db_name}"
    )]
    NoSuchArgument {
        db_name: Qualified<DataConnectorName>,
        collection_name: String,
        argument_name: String,
    },
    #[error(
        "argument {argument_name} is not defined for function/procedure {func_proc_name} in data connector {db_name}"
    )]
    NoSuchArgumentForCommand {
        db_name: Qualified<DataConnectorName>,
        func_proc_name: String,
        argument_name: String,
    },
    #[error(
        "column {column_name} is not defined in collection {collection_name} in data connector {db_name}"
    )]
    NoSuchColumn {
        db_name: Qualified<DataConnectorName>,
        model_name: Qualified<ModelName>,
        field_name: FieldName,
        collection_name: String,
        column_name: String,
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
    #[error("column {column_name} is not defined in function/procedure {func_proc_name} in data connector {db_name}")]
    NoSuchColumnForCommand {
        db_name: Qualified<DataConnectorName>,
        command_name: Qualified<CommandName>,
        field_name: FieldName,
        func_proc_name: String,
        column_name: String,
    },
    #[error("column {column_name} has type {column_type} in collection {collection_name} in data connector {db_name}, not type {field_type}")]
    ColumnTypeDoesNotMatch {
        db_name: DataConnectorName,
        model_name: ModelName,
        field_name: FieldName,
        collection_name: String,
        column_name: String,
        field_type: String,
        column_type: String,
    },
    #[error("internal error: data connector does not define the scalar type {r#type}, used by field {field_name} in model {model_name}")]
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
    #[error("Result type of function/procedure {function_or_procedure_name:} is {function_or_procedure_output_type:} but output type of command {command_name:} is {command_output_type:}")]
    FuncProcAndCommandScalarOutputTypeMismatch {
        function_or_procedure_name: String,
        function_or_procedure_output_type: String,
        command_name: String,
        command_output_type: String,
    },
    #[error("Custom result type of function {function_or_procedure_name:} does not match custom output type of command: {command_name:}")]
    FuncProcAndCommandCustomOutputTypeMismatch {
        function_or_procedure_name: String,
        command_name: String,
    },
    #[error("data connector does not support queries")]
    QueryCapabilityUnsupported,
    #[error("data connector does not support mutations")]
    MutationCapabilityUnsupported,
    #[error("Using predicate types as field or argument types is currently unsupported")]
    PredicateTypeUnsupported,
}

// Get the underlying type name by resolving Array and Nullable container types
fn get_underlying_type_name(output_type: &QualifiedTypeReference) -> &QualifiedTypeName {
    match &output_type.underlying_type {
        QualifiedBaseType::List(output_type) => get_underlying_type_name(output_type),
        QualifiedBaseType::Named(type_name) => type_name,
    }
}

pub fn validate_ndc(
    model_name: &Qualified<ModelName>,
    model: &Model,
    schema: &ndc_models::SchemaResponse,
) -> std::result::Result<(), NDCValidationError> {
    let model_source = match &model.source {
        Some(model_source) => model_source,
        None => {
            return Ok(());
        }
    };
    let db = &model_source.data_connector;

    let collection_name = &model_source.collection;

    let collection = schema
        .collections
        .iter()
        .find(|collection| collection.name == *collection_name)
        .ok_or_else(|| NDCValidationError::NoSuchCollection {
            db_name: db.name.clone(),
            model_name: model_name.clone(),
            collection_name: collection_name.clone(),
        })?;

    for mapped_argument_name in model_source.argument_mappings.values() {
        if !collection.arguments.contains_key(mapped_argument_name) {
            return Err(NDCValidationError::NoSuchArgument {
                db_name: db.name.clone(),
                collection_name: collection_name.clone(),
                argument_name: mapped_argument_name.clone(),
            });
        }
        // TODO: Add type validation for arguments
    }

    let collection_type = schema.object_types.get(&collection.collection_type).ok_or(
        NDCValidationError::NoSuchType(collection.collection_type.clone()),
    )?;

    let super::types::TypeMapping::Object { field_mappings, .. } = model_source
        .type_mappings
        .get(&model.data_type)
        .ok_or_else(|| NDCValidationError::UnknownModelTypeMapping {
            model_name: model_name.clone(),
            type_name: model.data_type.clone(),
        })?;
    for (field_name, field_mapping) in field_mappings {
        let column_name = &field_mapping.column;
        let _column =
            collection_type
                .fields
                .get(column_name)
                .ok_or(NDCValidationError::NoSuchColumn {
                    db_name: db.name.clone(),
                    model_name: model_name.clone(),
                    field_name: field_name.clone(),
                    collection_name: collection_name.clone(),
                    column_name: column_name.clone(),
                })?;
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
        //     .ok_or(NDCValidationError::TypeCapabilityNotDefined {
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
    command: &Command,
    schema: &ndc_models::SchemaResponse,
) -> std::result::Result<(), NDCValidationError> {
    // Check if the command source exists for the command
    let command_source = match &command.source {
        Some(command_source) => command_source,
        None => {
            return Ok(());
        }
    };

    let db = &command_source.data_connector;

    let (
        command_source_func_proc_name,
        command_source_ndc_arguments,
        command_source_ndc_result_type,
    ) = match &command_source.source {
        DataConnectorCommand::Procedure(procedure) => {
            let command_source_ndc = schema
                .procedures
                .iter()
                .find(|proc| proc.name == *procedure.0)
                .ok_or_else(|| NDCValidationError::NoSuchProcedure {
                    db_name: db.name.clone(),
                    command_name: command_name.clone(),
                    procedure_name: procedure.clone(),
                })?;

            (
                &procedure.0,
                command_source_ndc.arguments.clone(),
                &command_source_ndc.result_type,
            )
        }

        DataConnectorCommand::Function(function) => {
            let command_source_ndc = schema
                .functions
                .iter()
                .find(|func| func.name == *function.0)
                .ok_or_else(|| NDCValidationError::NoSuchFunction {
                    db_name: db.name.clone(),
                    command_name: command_name.clone(),
                    function_name: function.clone(),
                })?;

            (
                &function.0,
                command_source_ndc.arguments.clone(),
                &command_source_ndc.result_type,
            )
        }
    };

    // Check if the arguments are correctly mapped
    for mapped_argument_name in command_source.argument_mappings.values() {
        if !command_source_ndc_arguments.contains_key(mapped_argument_name) {
            return Err(NDCValidationError::NoSuchArgumentForCommand {
                db_name: db.name.clone(),
                func_proc_name: command_source_func_proc_name.clone(),
                argument_name: mapped_argument_name.clone(),
            });
        }
    }

    // Validate if the result type of function/procedure exists in the schema types(scalar + object)
    let command_source_ndc_result_type_name =
        get_underlying_named_type(command_source_ndc_result_type)?;
    if !(schema
        .scalar_types
        .contains_key(command_source_ndc_result_type_name)
        || schema
            .object_types
            .contains_key(command_source_ndc_result_type_name))
    {
        return Err(NDCValidationError::NoSuchType(
            command_source_ndc_result_type_name.to_string(),
        ));
    };

    // Check if the result_type of function/procedure actually has a scalar type or an object type.
    // If it is an object type, then validate the type mapping.
    match get_underlying_type_name(&command.output_type) {
        QualifiedTypeName::Inbuilt(_command_output_type) => {
            // TODO: Validate that the type of command.output_type is
            // same as the &command_source_ndc.result_type
        }
        QualifiedTypeName::Custom(custom_type) => {
            match schema.object_types.get(command_source_ndc_result_type_name) {
                // Check if the command.output_type is available in schema.object_types
                Some(command_source_ndc_type) => {
                    // Check if the command.output_type has typeMappings
                    let super::types::TypeMapping::Object { field_mappings, .. } = command_source
                        .type_mappings
                        .get(custom_type)
                        .ok_or_else(|| NDCValidationError::UnknownCommandTypeMapping {
                            command_name: command_name.clone(),
                            type_name: custom_type.clone(),
                        })?;
                    // Check if the field mappings for the output_type is valid
                    for (field_name, field_mapping) in field_mappings {
                        let column_name = &field_mapping.column;
                        if !command_source_ndc_type.fields.contains_key(column_name) {
                            return Err(NDCValidationError::NoSuchColumnForCommand {
                                db_name: db.name.clone(),
                                command_name: command_name.clone(),
                                field_name: field_name.clone(),
                                func_proc_name: command_source_func_proc_name.clone(),
                                column_name: column_name.clone(),
                            });
                        }
                    }
                }
                // If the command.output_type is not available in schema.object_types, then check if it is available in the schema.scalar_types
                // else raise an NDCValidationError error
                None => match schema.scalar_types.get(command_source_ndc_result_type_name) {
                    Some(_command_source_ndc_type) => (),
                    None => Err(NDCValidationError::NoSuchType(
                        command_source_ndc_result_type_name.to_string(),
                    ))?,
                },
            };
        }
    }
    Ok(())
}

pub fn get_underlying_named_type(
    result_type: &ndc_models::Type,
) -> Result<&str, NDCValidationError> {
    match result_type {
        ndc_models::Type::Named { name } => Ok(name),
        ndc_models::Type::Array { element_type } => get_underlying_named_type(element_type),
        ndc_models::Type::Nullable { underlying_type } => {
            get_underlying_named_type(underlying_type)
        }
        ndc_models::Type::Predicate {
            object_type_name: _,
        } => Err(NDCValidationError::PredicateTypeUnsupported),
    }
}
