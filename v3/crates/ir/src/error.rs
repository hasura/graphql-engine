use gql::ast::common as ast;
use lang_graphql as gql;
use open_dds::{
    aggregates::AggregationFunctionName,
    arguments::ArgumentName,
    data_connector::{DataConnectorColumnName, DataConnectorName},
    relationships::RelationshipName,
    session_variables::SessionVariable,
    types::{CustomTypeName, FieldName},
};
use serde_json as json;
use thiserror::Error;
use tracing_util::{ErrorVisibility, TraceableError};
use transitive::Transitive;

use metadata_resolve::{Qualified, QualifiedTypeName};
use schema::{Annotation, NamespaceAnnotation};

#[allow(clippy::duplicated_attributes)] // suppress spurious warnings from Clippy
#[derive(Error, Debug, Transitive)]
#[transitive(from(json::Error, InternalError))]
#[transitive(from(gql::normalized_ast::Error, InternalError))]
#[transitive(from(InternalEngineError, InternalError))]
#[transitive(from(InternalDeveloperError, InternalError))]
pub enum Error {
    #[error("The global ID {encoded_value:} couldn't be decoded due to {decoding_error:}")]
    FailureDecodingGlobalId {
        encoded_value: String,
        decoding_error: String,
    },

    #[error("Unexpected value: expecting {expected_kind:}, but found: {found:}")]
    UnexpectedValue {
        expected_kind: &'static str,
        found: json::Value,
    },

    #[error("'{name:}' is not a valid GraphQL name.")]
    TypeFieldInvalidGraphQlName { name: String },

    #[error("field '{field_name:} not found in entity representation")]
    FieldNotFoundInEntityRepresentation { field_name: FieldName },

    #[error("order_by expects a list of input objects with exactly one key-value pair per input object. Please split the input object with multiple key-value pairs into a list of single key-value pair objects.")]
    OrderByObjectShouldExactlyHaveOneKeyValuePair,

    #[error("missing non-nullable argument {argument_name:} for field {field_name:}")]
    MissingNonNullableArgument {
        argument_name: String,
        field_name: String,
    },

    #[error("internal: {0}")]
    Internal(#[from] InternalError),
}

impl Error {
    /// Map the internal unexpected value error from gql::normalized_ast::Error to external error.
    /// It is useful to expose any unexpected value error in API response.
    pub fn map_unexpected_value_to_external_error(error: gql::normalized_ast::Error) -> Self {
        match error {
            gql::normalized_ast::Error::UnexpectedValue {
                expected_kind,
                found,
            } => Self::UnexpectedValue {
                expected_kind,
                found,
            },
            err => Self::from(err),
        }
    }
}

impl TraceableError for Error {
    fn visibility(&self) -> ErrorVisibility {
        match self {
            Self::Internal(internal) => match internal {
                InternalError::Developer(_) => ErrorVisibility::User,
                InternalError::Engine(_) => ErrorVisibility::Internal,
            },
            _ => ErrorVisibility::User,
        }
    }
}

#[allow(clippy::duplicated_attributes)] // suppress spurious warnings from Clippy
#[derive(Error, Debug, Transitive)]
#[transitive(from(json::Error, InternalEngineError))]
#[transitive(from(gql::normalized_ast::Error, InternalEngineError))]
pub enum InternalError {
    #[error("{0}")]
    Developer(#[from] InternalDeveloperError),
    #[error("{0}")]
    Engine(#[from] InternalEngineError),
}

#[derive(Debug, thiserror::Error)]
pub enum InternalDeveloperError {
    #[error("No source data connector specified for field {field_name} of type {type_name}")]
    NoSourceDataConnector {
        type_name: ast::TypeName,
        field_name: ast::Name,
    },

    #[error("No function/procedure specified for command field {field_name} of type {type_name}")]
    NoFunctionOrProcedure {
        type_name: ast::TypeName,
        field_name: ast::Name,
    },

    #[error("No argument source specified for argument {argument_name} of field {field_name}")]
    NoArgumentSource {
        field_name: ast::Name,
        argument_name: ast::Name,
    },

    #[error("Required session variable not found in the request: {session_variable}")]
    MissingSessionVariable { session_variable: SessionVariable },

    #[error("Unable to typecast session variable. Expected: {expected:}, but found: {found:}")]
    VariableTypeCast { expected: String, found: String },

    #[error("Typecasting to array is not supported.")]
    VariableArrayTypeCast,

    #[error("Mapping for the {mapping_kind} typename {type_name:} not found")]
    TypenameMappingNotFound {
        type_name: ast::TypeName,
        mapping_kind: &'static str,
    },

    #[error("Type mapping not found for the type name {type_name:}")]
    TypeMappingNotFound {
        type_name: Qualified<CustomTypeName>,
    },

    #[error("Field mapping not found for the field {field_name:} of type {type_name:}")]
    FieldMappingNotFound {
        type_name: Qualified<CustomTypeName>,
        field_name: FieldName,
    },

    #[error("Type mapping not found for the type name {type_name:} while executing the relationship {relationship_name:}")]
    TypeMappingNotFoundForRelationship {
        type_name: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
    },

    #[error("Field mapping not found for the field {field_name:} of type {type_name:} while executing the relationship {relationship_name:}")]
    FieldMappingNotFoundForRelationship {
        type_name: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
        field_name: FieldName,
    },

    #[error("Argument mapping not found for the argument {argument_name:} while executing the relationship {relationship_name:}")]
    ArgumentMappingNotFoundForRelationship {
        relationship_name: RelationshipName,
        argument_name: ArgumentName,
    },

    #[error("The value expression could not be converted to header value. Error: ")]
    UnableToConvertValueExpressionToHeaderValue,

    #[error("The aggregation function {aggregation_function} operating over the {aggregate_operand_type} type is missing a data connector mapping for {data_connector_name}")]
    DataConnectorAggregationFunctionNotFound {
        aggregate_operand_type: QualifiedTypeName,
        aggregation_function: AggregationFunctionName,
        data_connector_name: Qualified<DataConnectorName>,
    },

    #[error("A field ({field_name}) with an AggregatableField annotation was found on a scalar-typed ({aggregate_operand_type}) operand's selection set")]
    AggregatableFieldFoundOnScalarTypedOperand {
        field_name: FieldName,
        aggregate_operand_type: QualifiedTypeName,
    },

    #[error("The aggregation function {aggregation_function} was used on the model object type and not on a model field. Aggregation functions operate on columns, not rows")]
    ColumnAggregationFunctionUsedOnModelObjectType {
        aggregate_operand_type: QualifiedTypeName,
        aggregation_function: AggregationFunctionName,
    },
}

#[derive(Debug, thiserror::Error)]
pub enum InternalEngineError {
    #[error("serialization error: {0}")]
    SerializationError(#[from] json::Error),

    #[error("error from normalized AST: {0}")]
    IRConversionError(#[from] gql::normalized_ast::Error),

    #[error("internal error: {0}")]
    OperatorMappingError(#[from] OperatorMappingError),

    #[error("unexpected annotation: {annotation}")]
    UnexpectedAnnotation { annotation: Annotation },

    #[error("unexpected namespace annotation: {namespace_annotation:} found, expected type {expected_type:}")]
    UnexpectedNamespaceAnnotation {
        namespace_annotation: NamespaceAnnotation,
        expected_type: String,
    },

    #[error("subscription shouldn't have been validated")]
    SubscriptionsNotSupported,

    #[error("expected namespace annotation type {namespace_annotation_type} but not found")]
    // Running into this error means that the GDS field was not annotated with the correct
    // namespace annotation while building the metadata.
    ExpectedNamespaceAnnotationNotFound { namespace_annotation_type: String },

    #[error("internal error during execution of argument presets: {description}")]
    ArgumentPresetExecution { description: String },

    #[error("internal error: {description}")]
    InternalGeneric { description: String },
}

#[derive(Debug, thiserror::Error)]
pub enum OperatorMappingError {
    #[error("could not find operator mapping for column {column_name:} in data connector {data_connector_name}")]
    MissingEntryForDataConnector {
        column_name: DataConnectorColumnName,
        data_connector_name: Qualified<DataConnectorName>,
    },
}
