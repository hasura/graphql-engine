use open_dds::{
    arguments::ArgumentName,
    data_connector::{DataConnectorName, DataConnectorObjectType},
    types::{CustomTypeName, FieldName},
};

use crate::NDCValidationError;
use crate::types::subgraph::Qualified;
use crate::{
    stages::{apollo, graphql_config},
    types::error::ContextualError,
};

#[derive(Debug, thiserror::Error)]
pub enum ObjectTypesError {
    #[error("object type {type_name} could not be found")]
    ObjectTypeNotFound {
        type_name: Qualified<CustomTypeName>,
    },
    #[error(
        "the following argument for field {field_name:} in type {type_name:} is defined more than once: {argument_name:}"
    )]
    DuplicateArgumentDefinition {
        field_name: FieldName,
        argument_name: ArgumentName,
        type_name: Qualified<CustomTypeName>,
    },
    #[error("the following type is defined more than once: {name:}")]
    DuplicateTypeDefinition { name: Qualified<CustomTypeName> },
    #[error("{error:} in object type {type_name:}")]
    DataConnectorTypeMappingValidationError {
        type_name: Qualified<CustomTypeName>,
        error: TypeMappingValidationError,
    },

    #[error(
        "Multiple mappings have been defined from object {data_connector_object_type:} of data connector {data_connector:}"
    )]
    DuplicateDataConnectorObjectTypeMapping {
        data_connector: Qualified<DataConnectorName>,
        data_connector_object_type: String,
    },
    #[error("the following field in type {type_name:} is defined more than once: {field_name:}")]
    DuplicateFieldDefinition {
        type_name: Qualified<CustomTypeName>,
        field_name: FieldName,
    },
    #[error(
        "A field named `id` cannot be present in the object type {type_name} when global_id fields are non-empty."
    )]
    IdFieldConflictingGlobalId {
        type_name: Qualified<CustomTypeName>,
    },
    #[error("Unknown field {field_name:} in global_id defined for the type {type_name:}")]
    UnknownFieldInGlobalId {
        field_name: FieldName,
        type_name: Qualified<CustomTypeName>,
    },
    #[error("{0}")]
    GraphqlError(#[from] graphql_config::GraphqlConfigError),
    #[error("{0}")]
    ApolloError(#[from] apollo::ApolloError),
}

impl ContextualError for ObjectTypesError {
    fn create_error_context(&self) -> Option<error_context::Context> {
        None
    }
}

#[derive(Debug, thiserror::Error)]
pub enum TypeMappingValidationError {
    #[error(
        "data connector {data_connector:} referenced in type mappings of type {type_name:} is not found"
    )]
    UnknownDataConnector {
        data_connector: Qualified<DataConnectorName>,
        type_name: Qualified<CustomTypeName>,
    },
    #[error("the type {type_name:} referenced in type mappings has not been defined")]
    UnknownSourceType {
        type_name: Qualified<CustomTypeName>,
    },
    #[error(
        "the following fields in field mappings of type {type_name:} are unknown: {}",
        field_names.join(", ")
    )]
    UnknownSourceFields {
        type_name: Qualified<CustomTypeName>,
        field_names: Vec<FieldName>,
    },
    #[error("unknown target column name {column_name:} for field {field_name:}")]
    UnknownTargetColumn {
        column_name: String,
        field_name: FieldName,
    },
    #[error(
        "the mapping for field {field_name:} of type {type_name:} has been defined more than once"
    )]
    DuplicateFieldMapping {
        type_name: Qualified<CustomTypeName>,
        field_name: FieldName,
    },
    #[error(
        "the type {unknown_field_type_name:} referenced by the field {field_name:} in type {type_name:} has not been defined"
    )]
    UnknownFieldType {
        type_name: Qualified<CustomTypeName>,
        field_name: FieldName,
        unknown_field_type_name: Qualified<CustomTypeName>,
    },
    #[error(
        "could not find mappings for {object_type_name:} to the {data_connector_object_type:} on data connector {data_connector_name:}"
    )]
    DataConnectorTypeMappingNotFound {
        object_type_name: Qualified<CustomTypeName>,
        data_connector_name: Qualified<DataConnectorName>,
        data_connector_object_type: DataConnectorObjectType,
    },
    #[error(
        "the type {unknown_ndc_type:} is not defined as an object type in the connector's schema. This type is being mapped to by the type {type_name:}"
    )]
    UnknownNdcType {
        type_name: Qualified<CustomTypeName>,
        unknown_ndc_type: DataConnectorObjectType,
    },
    #[error("expected to find a predicate type for argument {argument_name:} but did not")]
    PredicateTypeNotFound { argument_name: ArgumentName },
    #[error(
        "the type {unknown_ndc_field_type_name:} is not defined as an object type in the connector's schema. This type is referenced by the field {ndc_field_name:} in the connector's schema type {ndc_type_name:}, which is mapped to the field {field_name:} in the type {type_name:}"
    )]
    UnknownNdcFieldObjectType {
        type_name: Qualified<CustomTypeName>,
        field_name: FieldName,
        ndc_type_name: String,
        ndc_field_name: String,
        unknown_ndc_field_type_name: String,
    },
    #[error("ndc validation error: {0}")]
    NDCValidationError(#[from] NDCValidationError),
}
