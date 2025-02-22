use gql::ast::common as ast;
use lang_graphql as gql;
use open_dds::{
    aggregates::AggregationFunctionName,
    arguments::ArgumentName,
    data_connector::{DataConnectorColumnName, DataConnectorName},
    relationships::RelationshipName,
    types::{CustomTypeName, FieldName},
};
use serde_json as json;
use thiserror::Error;
use tracing_util::{ErrorVisibility, TraceableError};
use transitive::Transitive;

use graphql_schema::Annotation;
use metadata_resolve::{Qualified, QualifiedTypeName};

impl From<plan::InternalError> for Error {
    fn from(plan_internal_error: plan::InternalError) -> Error {
        match plan_internal_error {
            plan::InternalError::Engine(engine_error) => Error::Internal(InternalError::Engine(
                InternalEngineError::PlanInternalEngineError(engine_error),
            )),
            plan::InternalError::Developer(developer_error) => {
                Error::Internal(InternalError::Developer(
                    InternalDeveloperError::PlanInternalDeveloperError(developer_error),
                ))
            }
        }
    }
}

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

    #[error("'{alias:} is not a valid alias")]
    InvalidAlias { alias: String },

    #[error("{value} is not a valid limit value")]
    InvalidLimitValue { value: u32 },

    #[error("{value} is not a valid offset value")]
    InvalidOffsetValue { value: u32 },

    #[error("field '{field_name:} not found in entity representation")]
    FieldNotFoundInEntityRepresentation { field_name: FieldName },

    #[error("order_by expects a list of input objects with exactly one key-value pair per input object. Please split the input object with multiple key-value pairs into a list of single key-value pair objects.")]
    OrderByObjectShouldExactlyHaveOneKeyValuePair,

    #[error("missing non-nullable argument {argument_name:} for field {field_name:}")]
    MissingNonNullableArgument {
        argument_name: String,
        field_name: String,
    },

    #[error("Only one subscription root field is allowed")]
    NoneOrMoreSubscriptionRootFields,

    #[error("internal error: type mapping not found for type {type_name:}")]
    InternalTypeMappingNotFound {
        type_name: Qualified<CustomTypeName>,
    },

    #[error("internal error: type mapping or field mapping not found for type {type_name:} and field {field_name:}")]
    InternalMappingNotFound {
        type_name: Qualified<CustomTypeName>,
        field_name: FieldName,
    },

    #[error("internal error: missing target model source for the relationship {relationship_name:} on type {type_name:}")]
    InternalMissingTargetModelSourceForRelationship {
        relationship_name: RelationshipName,
        type_name: Qualified<CustomTypeName>,
    },

    #[error("{0}")]
    PlanError(#[from] plan::PlanError),

    #[error("{0}")]
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
            Self::Internal(internal) => internal.visibility(),
            Self::PlanError(error) => error.visibility(),
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

impl TraceableError for InternalError {
    fn visibility(&self) -> ErrorVisibility {
        match self {
            Self::Developer(_) => ErrorVisibility::User,
            Self::Engine(_) => ErrorVisibility::Internal,
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum InternalDeveloperError {
    #[error("Target model {model_name} not found for relationship {relationship_name}")]
    TargetModelNotFoundForRelationship {
        model_name: Qualified<open_dds::models::ModelName>,
        relationship_name: open_dds::relationships::RelationshipName,
    },

    #[error("Target command {command_name} not found for relationship {relationship_name}")]
    TargetCommandNotFoundForRelationship {
        command_name: Qualified<open_dds::commands::CommandName>,
        relationship_name: open_dds::relationships::RelationshipName,
    },

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

    #[error("Mapping for the {mapping_kind} typename {type_name:} not found")]
    TypenameMappingNotFound {
        type_name: ast::TypeName,
        mapping_kind: &'static str,
    },

    #[error("Field mapping not found for the field {field_name:} of type {type_name:}")]
    FieldMappingNotFound {
        type_name: Qualified<CustomTypeName>,
        field_name: FieldName,
    },

    #[error("{0}")]
    RelationshipFieldMappingError(#[from] plan::RelationshipFieldMappingError),

    #[error("Argument mapping not found for the argument {argument_name:} while executing the relationship {relationship_name:}")]
    ArgumentMappingNotFoundForRelationship {
        relationship_name: RelationshipName,
        argument_name: ArgumentName,
    },

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

    #[error("{0}")]
    PlanInternalDeveloperError(plan::InternalDeveloperError),
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

    #[error("expected namespace annotation type {namespace_annotation_type} but not found")]
    // Running into this error means that the GDS field was not annotated with the correct
    // namespace annotation while building the metadata.
    ExpectedNamespaceAnnotationNotFound { namespace_annotation_type: String },

    #[error("Subscription is restricted by select permission")]
    SubscriptionNotAllowed,

    #[error("{0}")]
    PlanInternalEngineError(plan::InternalEngineError),

    #[error("{0}")]
    OrderableRelationshipError(#[from] metadata_resolve::OrderableRelationshipError),

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
