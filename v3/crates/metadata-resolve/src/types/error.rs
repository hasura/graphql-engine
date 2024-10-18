use crate::helpers::typecheck::TypecheckError;
use crate::stages::{
    aggregate_boolean_expressions, aggregates::AggregateExpressionError, apollo, argument_presets,
    boolean_expressions, commands, data_connector_scalar_types, data_connectors, graphql_config,
    models, object_types, order_by_expressions, relationships, relay, scalar_boolean_expressions,
    scalar_types, type_permissions,
};
use crate::types::subgraph::{Qualified, QualifiedTypeReference};
use lang_graphql::ast::common as ast;
use open_dds::data_connector::DataConnectorColumnName;
use open_dds::flags;
use open_dds::order_by_expression::OrderByExpressionName;
use open_dds::{
    arguments::ArgumentName,
    commands::CommandName,
    data_connector::{DataConnectorName, DataConnectorObjectType},
    models::ModelName,
    relationships::RelationshipName,
    types::{CustomTypeName, FieldName, OperatorName},
};
use std::fmt::Display;

use crate::helpers::{
    ndc_validation::NDCValidationError, type_mappings::TypeMappingCollectionError, typecheck,
};

// Eventually, we'll just delete the `Raw` variant and this will become a regular struct when all
// errors have all the relevant path information.
#[derive(Debug, thiserror::Error)]
pub enum WithContext<T> {
    Raw(#[from] T),
    Contextualised {
        error: T,
        context: error_context::Context,
    },
}

impl<T> WithContext<T> {
    pub fn context(&self) -> Option<error_context::Context> {
        match self {
            WithContext::Contextualised { context, .. } => Some(context.clone()),
            WithContext::Raw(_) => None,
        }
    }

    pub fn coerce<S: From<T>>(self) -> WithContext<S> {
        match self {
            WithContext::Raw(err) => WithContext::Raw(S::from(err)),
            WithContext::Contextualised { error, context } => WithContext::Contextualised {
                error: S::from(error),
                context,
            },
        }
    }
}

impl<T: Display> Display for WithContext<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WithContext::Contextualised { error, .. } | WithContext::Raw(error) => error.fmt(f),
        }
    }
}

// TODO: This enum really needs structuring
#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("unknown field {field_name:} in filterable fields defined for model {model_name:}")]
    UnknownFieldInFilterableFields {
        model_name: ModelName,
        field_name: FieldName,
    },
    #[error("a preset argument {argument_name:} has been set for the model {model_name:} but no such argument exists for this model")]
    ModelArgumentPresetMismatch {
        model_name: Qualified<ModelName>,
        argument_name: ArgumentName,
    },
    #[error("duplicate preset argument {argument_name:} for command {command_name:}")]
    DuplicateCommandArgumentPreset {
        command_name: Qualified<CommandName>,
        argument_name: ArgumentName,
    },
    #[error("duplicate preset argument {argument_name:} for model {model_name:}")]
    DuplicateModelArgumentPreset {
        model_name: Qualified<ModelName>,
        argument_name: ArgumentName,
    },

    // ----------------
    #[error("the mapping for type {type_name:} in model {model_name:} is defined more than once")]
    DuplicateTypeMappingDefinitionInModelSource {
        model_name: Qualified<ModelName>,
        type_name: CustomTypeName,
    },
    #[error("the mapping for type {type_name:} is defined against multiple data connector objects: {ndc_object_types:?}")]
    MultipleNDCObjectForOpenDDObjectType {
        type_name: Qualified<CustomTypeName>,
        ndc_object_types: Vec<String>,
    },
    #[error(
        "unknown argument {argument_name:} referenced in argument mappings for model {model_name:}"
    )]
    UnknownModelSourceArgument {
        model_name: Qualified<ModelName>,
        argument_name: ArgumentName,
    },
    #[error(
        "the mapping for argument {argument_name:} of model {model_name:} has been defined more than once"
    )]
    DuplicateModelArgumentMapping {
        model_name: Qualified<ModelName>,
        argument_name: ArgumentName,
    },
    #[error("model arguments graphql input configuration has been specified for model {model_name:} that does not have arguments")]
    UnnecessaryModelArgumentsGraphQlInputConfiguration { model_name: Qualified<ModelName> },
    #[error("an unnecessary filter input type name graphql configuration has been specified for model {model_name:} that does not use aggregates")]
    UnnecessaryFilterInputTypeNameGraphqlConfiguration { model_name: Qualified<ModelName> },
    #[error("filter input type name graphql configuration must be specified for model {model_name:} because aggregates are used with it")]
    MissingFilterInputTypeNameGraphqlConfiguration { model_name: Qualified<ModelName> },
    #[error("unknown field {field_name:} in unique identifier defined for model {model_name:}")]
    UnknownFieldInUniqueIdentifier {
        model_name: Qualified<ModelName>,
        field_name: FieldName,
    },
    #[error("duplicate field {field_name:} in unique identifier defined for model {model_name:}")]
    DuplicateFieldInUniqueIdentifier {
        model_name: Qualified<ModelName>,
        field_name: FieldName,
    },
    #[error("a source must be defined for model {model:} in order to use filter expressions")]
    CannotUseFilterExpressionsWithoutSource { model: Qualified<ModelName> },
    #[error("graphql config must be defined for a filter expression to be used in a {model:}")]
    CannotUseFilterExpressionsWithoutGraphQlConfig {
        model: Qualified<ModelName>,
        filter_expression_type: Qualified<CustomTypeName>,
    },
    #[error("Model {model:} has source data connector {model_data_connector:} but its filter expression type {filter_expression_type:} is backed by data connector {filter_expression_data_connector:}")]
    DifferentDataConnectorInFilterExpression {
        model: Qualified<ModelName>,
        model_data_connector: Qualified<DataConnectorName>,
        filter_expression_type: Qualified<CustomTypeName>,
        filter_expression_data_connector: Qualified<DataConnectorName>,
    },
    #[error("Model {model:} has source data connector object type {model_data_connector_object_type:} but its filter expression type {filter_expression_type:} is backed by data connector {filter_expression_data_connector_object_type:}")]
    DifferentDataConnectorObjectTypeInFilterExpression {
        model: Qualified<ModelName>,
        model_data_connector_object_type: DataConnectorObjectType,
        filter_expression_type: Qualified<CustomTypeName>,
        filter_expression_data_connector_object_type: DataConnectorObjectType,
    },
    #[error("Type error in argument {argument_name:}: {type_error:}")]
    ArgumentTypeError {
        argument_name: ArgumentName,
        type_error: TypeError,
    },
    #[error("unknown model used in model select permissions definition: {model_name:}")]
    UnknownModelInModelSelectPermissions { model_name: Qualified<ModelName> },
    #[error("multiple select permissions defined for model: {model_name:}")]
    DuplicateModelSelectPermission { model_name: Qualified<ModelName> },
    #[error("model source is required for model '{model_name:}' to resolve predicate")]
    ModelSourceRequiredForPredicate { model_name: Qualified<ModelName> },
    #[error(
        "both model source for model '{source_model_name:}' and target source for model '{target_model_name}' are required  to resolve select permission predicate with relationships"
    )]
    ModelAndTargetSourceRequiredForRelationshipPredicate {
        source_model_name: Qualified<ModelName>,
        target_model_name: Qualified<ModelName>,
    },
    #[error(
        "no relationship predicate is defined for relationship '{relationship_name:}' in model '{model_name:}'"
    )]
    NoPredicateDefinedForRelationshipPredicate {
        model_name: Qualified<ModelName>,
        relationship_name: RelationshipName,
    },
    #[error("unknown field '{field_name:}' used in select permissions of model '{model_name:}'")]
    UnknownFieldInSelectPermissionsDefinition {
        field_name: FieldName,
        model_name: Qualified<ModelName>,
    },
    #[error("field '{field_name:}' used in select permissions of model '{model_name:}' should be mapped to non-array scalar field")]
    UnsupportedFieldInSelectPermissionsPredicate {
        field_name: FieldName,
        model_name: Qualified<ModelName>,
    },
    #[error("Nested predicate used in select permissions of model '{model_name:}'")]
    NestedPredicateInSelectPermissionPredicate { model_name: Qualified<ModelName> },

    #[error("relationship '{relationship_name:}' used in select permissions of model '{model_name:}' does not exist on type {type_name:}")]
    UnknownRelationshipInSelectPermissionsPredicate {
        relationship_name: RelationshipName,
        model_name: Qualified<ModelName>,
        type_name: Qualified<CustomTypeName>,
    },
    #[error("The model '{target_model_name:}' corresponding to the  relationship '{relationship_name:}' used in select permissions of model '{model_name:}' is not defined")]
    UnknownModelUsedInRelationshipSelectPermissionsPredicate {
        model_name: Qualified<ModelName>,
        target_model_name: Qualified<ModelName>,
        relationship_name: RelationshipName,
    },
    #[error(
        "Invalid operator used in model '{model_name:}' select permission: '{operator_name:}'"
    )]
    InvalidOperatorInModelSelectPermission {
        model_name: Qualified<ModelName>,
        operator_name: OperatorName,
    },
    #[error("unknown command used in command permissions definition: {command_name:}")]
    UnknownCommandInCommandPermissions {
        command_name: Qualified<CommandName>,
    },
    #[error("multiple permissions defined for command: {command_name:}")]
    DuplicateCommandPermission {
        command_name: Qualified<CommandName>,
    },

    #[error("{message:}")]
    UnsupportedFeature { message: String },
    #[error("the referenced secret {secret_name:} was not found in the environment")]
    SecretNotFound { secret_name: String },
    #[error("{0}")]
    DeserializationError(#[from] serde_json::Error),
    #[error(
        "duplicate relationship field {field_name} from {relationship_name} associated with source type {type_name}"
    )]
    DuplicateRelationshipFieldInSourceType {
        field_name: ast::Name,
        type_name: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
    },
    #[error("unknown target model {model_name:} used in relationship {relationship_name:} on type {type_name:}")]
    UnknownTargetModelUsedInRelationship {
        type_name: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
        model_name: Qualified<ModelName>,
    },
    #[error("Model fields cannot be used in command based relationship: {relationship_name:} on type {type_name:}")]
    ModelFieldCannotBeUsedInCommandRelationship {
        relationship_name: RelationshipName,
        type_name: Qualified<CustomTypeName>,
    },
    #[error("unknown target command {command_name:} used in relationship {relationship_name:} on type {type_name:}")]
    UnknownTargetCommandUsedInRelationship {
        type_name: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
        command_name: Qualified<CommandName>,
    },
    #[error("{reason:}")]
    NotSupported { reason: String },
    #[error("The field path provided in the {location:} of the relationship {relationship_name} on type {type_name} is empty")]
    EmptyFieldPath {
        location: String,
        relationship_name: RelationshipName,
        type_name: Qualified<CustomTypeName>,
    },
    #[error("the data type {data_type:} has not been defined")]
    UnknownType {
        data_type: Qualified<CustomTypeName>,
    },
    #[error("the object type {data_type:} has not been defined")]
    UnknownObjectType {
        data_type: Qualified<CustomTypeName>,
    },
    #[error("the scalar type {data_type:} has not been defined")]
    UnknownScalarType {
        data_type: Qualified<CustomTypeName>,
    },
    #[error(
        "Type error in preset argument {argument_name:} for command {command_name:}: {type_error:}"
    )]
    CommandArgumentPresetTypeError {
        command_name: Qualified<CommandName>,
        argument_name: ArgumentName,
        type_error: typecheck::TypecheckError,
    },
    #[error(
        "Type error in preset argument {argument_name:} for model {model_name:}: {type_error:}"
    )]
    ModelArgumentPresetTypeError {
        model_name: Qualified<ModelName>,
        argument_name: ArgumentName,
        type_error: typecheck::TypecheckError,
    },
    #[error("{graphql_config_error:}")]
    GraphqlConfigError {
        #[from]
        graphql_config_error: graphql_config::GraphqlConfigError,
    },
    #[error("{relationship_error:}")]
    ObjectRelationshipError {
        relationship_error: RelationshipError,
    },
    #[error("Error in order by expression {order_by_expression_name}: {error}")]
    OrderByExpressionError {
        order_by_expression_name: Qualified<OrderByExpressionName>,
        error: order_by_expressions::OrderByExpressionError,
    },
    #[error("{0}")]
    BooleanExpressionError(#[from] boolean_expressions::BooleanExpressionError),
    #[error("{0}")]
    ScalarBooleanExpressionTypeError(
        #[from] scalar_boolean_expressions::ScalarBooleanExpressionTypeError,
    ),
    #[error("{0}")]
    AggregateBooleanExpressionError(
        #[from] aggregate_boolean_expressions::NamedAggregateBooleanExpressionError,
    ),
    #[error("{type_predicate_error:}")]
    TypePredicateError {
        type_predicate_error: TypePredicateError,
    },
    #[error("{0}")]
    DataConnectorError(#[from] data_connectors::NamedDataConnectorError),
    #[error("NDC validation error: {0}")]
    NDCValidationError(#[from] NDCValidationError),
    #[error("{0}")]
    ScalarTypesError(#[from] scalar_types::ScalarTypesError),
    #[error("{type_error:}")]
    TypeError { type_error: TypeError },
    #[error("{0}")]
    AggregateExpressionError(AggregateExpressionError),
    #[error("{0}")]
    TypePermissionError(type_permissions::TypePermissionError),
    #[error("{0}")]
    ObjectTypesError(#[from] object_types::ObjectTypesError),
    #[error("{0}")]
    ApolloError(#[from] apollo::ApolloError),
    #[error("{0}")]
    RelayError(#[from] relay::RelayError),
    #[error("{0}")]
    ModelsError(#[from] models::ModelsError),
    #[error("{0}")]
    CommandsError(#[from] commands::CommandsError),
    #[error("{0}")]
    RelationshipError(#[from] relationships::RelationshipError),
    #[error("{0}")]
    DataConnectorScalarTypesError(
        #[from] data_connector_scalar_types::DataConnectorScalarTypesError,
    ),
    #[error("{0}")]
    ArgumentPresetError(#[from] argument_presets::ArgumentPresetError),
    #[error(
        "The following issues were raised but disallowed by compatibility configuration:\n\n{warnings_as_errors}"
    )]
    CompatibilityError {
        warnings_as_errors: SeparatedBy<crate::Warning>,
    },
}

pub trait ShouldBeAnError {
    fn should_be_an_error(&self, flags: &flags::Flags) -> bool;
}

// A small utility type which exists for the sole purpose of displaying a vector with a certain
// separator.
#[derive(Debug)]
pub struct SeparatedBy<T> {
    pub lines_of: Vec<T>,
    pub separator: String,
}

impl<T: Display> Display for SeparatedBy<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (index, elem) in self.lines_of.iter().enumerate() {
            elem.fmt(f)?;
            if index < self.lines_of.len() - 1 {
                self.separator.fmt(f)?;
            }
        }

        Ok(())
    }
}

#[derive(Debug, thiserror::Error)]
pub enum RelationshipError {
    #[error("source field {field_name} in field mapping for relationship {relationship_name} on type {source_type} is unknown.")]
    UnknownSourceFieldInRelationshipMapping {
        source_type: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
        field_name: FieldName,
    },
    #[error("target field {field_name} in field mapping for relationship {relationship_name} on type {source_type} to model {model_name} is unknown.")]
    UnknownTargetFieldInRelationshipMapping {
        source_type: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
        model_name: Qualified<ModelName>,
        field_name: FieldName,
    },
    #[error("target argument {argument_name} in argument mapping for relationship {relationship_name} on type {source_type} to command {command_name} is unknown.")]
    UnknownTargetArgumentInRelationshipMapping {
        source_type: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
        command_name: Qualified<CommandName>,
        argument_name: ArgumentName,
    },
    #[error("Mapping for source field {field_name} already exists in the relationship {relationship_name} on type {type_name}")]
    MappingExistsInRelationship {
        type_name: Qualified<CustomTypeName>,
        field_name: FieldName,
        relationship_name: RelationshipName,
    },
    #[error("Mapping for target argument {argument_name} of command {command_name} already exists in the relationship {relationship_name} on type {type_name}")]
    ArgumentMappingExistsInRelationship {
        argument_name: ArgumentName,
        command_name: Qualified<CommandName>,
        relationship_name: RelationshipName,
        type_name: Qualified<CustomTypeName>,
    },
    #[error("No mapping for target command argument {argument_name} in the relationship {relationship_name} on type {type_name}")]
    MissingArgumentMappingInRelationship {
        type_name: Qualified<CustomTypeName>,
        argument_name: ArgumentName,
        relationship_name: RelationshipName,
    },
    #[error("The target data connector {data_connector_name} for relationship {relationship_name} on type {type_name} does not support the variables capability")]
    RelationshipTargetDoesNotSupportForEach {
        type_name: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
        data_connector_name: Qualified<DataConnectorName>,
    },
    #[error("The target data connector {data_connector_name} for relationship {relationship_name} on type {type_name} has not defined any capabilities")]
    NoRelationshipCapabilitiesDefined {
        type_name: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
        data_connector_name: Qualified<DataConnectorName>,
    },
    #[error("The relationship {relationship_name} on type {type_name} defines an aggregate, but aggregates can only be used with array relationships, not object relationships")]
    AggregateIsOnlyAllowedOnArrayRelationships {
        type_name: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
    },
    #[error("The aggregate defined on the relationship {relationship_name} on type {type_name} has an error: {error}")]
    ModelAggregateExpressionError {
        type_name: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
        error: models::ModelsError, // ideally, this would return the more accurate
                                    // `ModelAggregateExpressionError` instead
    },
}

impl From<RelationshipError> for Error {
    fn from(val: RelationshipError) -> Self {
        Error::ObjectRelationshipError {
            relationship_error: val,
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum TypePredicateError {
    #[error("unknown field '{field_name:}' used in predicate for type '{type_name:}'")]
    UnknownFieldInTypePredicate {
        field_name: FieldName,
        type_name: Qualified<CustomTypeName>,
    },
    #[error("boolean expression '{boolean_expression_name:}' not found")]
    BooleanExpressionNotFound {
        boolean_expression_name: Qualified<CustomTypeName>,
    },
    #[error(
        "the source data connector {data_connector:} for type {type_name:} has not been defined"
    )]
    UnknownTypeDataConnector {
        type_name: Qualified<CustomTypeName>,
        data_connector: Qualified<DataConnectorName>,
    },
    #[error("field '{field_name:}' used in predicate for type '{type_name:}' should be mapped to non-array scalar field")]
    UnsupportedFieldInTypePredicate {
        field_name: FieldName,
        type_name: Qualified<CustomTypeName>,
    },
    #[error("Invalid operator used in type '{type_name:}' predicate: '{operator_name:}'")]
    InvalidOperatorInTypePredicate {
        type_name: Qualified<CustomTypeName>,
        operator_name: OperatorName,
    },
    #[error("Nested predicate used in type '{type_name:}'")]
    NestedPredicateInTypePredicate {
        type_name: Qualified<CustomTypeName>,
    },
    #[error("relationship '{relationship_name:}' is used in predicate but does not exist in comparableRelationships in boolean expression for type '{type_name:}'")]
    UnknownRelationshipInTypePredicate {
        relationship_name: RelationshipName,
        type_name: Qualified<CustomTypeName>,
    },
    #[error("The model '{target_model_name:}' corresponding to the  relationship '{relationship_name:}' used in predicate for type '{type_name:}' is not defined")]
    UnknownModelUsedInRelationshipTypePredicate {
        type_name: Qualified<CustomTypeName>,
        target_model_name: Qualified<ModelName>,
        relationship_name: RelationshipName,
    },
    #[error(
        "target source for model '{target_model_name:}' is required to resolve predicate with relationships for {source_type_name:}"
    )]
    TargetSourceRequiredForRelationshipPredicate {
        source_type_name: Qualified<CustomTypeName>,
        target_model_name: Qualified<ModelName>,
    },
    #[error(
        "no relationship predicate is defined for relationship '{relationship_name:}' in type '{type_name:}'"
    )]
    NoPredicateDefinedForRelationshipPredicate {
        type_name: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
    },
    #[error("{error:} in type {type_name:}")]
    TypeMappingCollectionError {
        type_name: Qualified<CustomTypeName>,
        error: TypeMappingCollectionError,
    },
    #[error("object type {type_name:} not found")]
    ObjectTypeNotFound {
        type_name: Qualified<CustomTypeName>,
    },
    #[error("operator mappings not found for data connector {data_connector_name:}")]
    OperatorMappingsNotFound {
        data_connector_name: Qualified<DataConnectorName>,
    },
    #[error(
        "no type mapping found for type {type_name:} in data connector {data_connector_name:}"
    )]
    UnknownTypeMapping {
        type_name: Qualified<CustomTypeName>,
        data_connector_name: Qualified<DataConnectorName>,
    },
    #[error("no field mapping found for field {field_name:} in type {type_name:} in data connector {data_connector_name:}")]
    UnknownFieldMapping {
        type_name: Qualified<CustomTypeName>,
        field_name: FieldName,
        data_connector_name: Qualified<DataConnectorName>,
    },
    #[error("Operator {operator_name:} not found for field {field_name:}")]
    OperatorNotFoundForField {
        field_name: FieldName,
        operator_name: OperatorName,
    },
    #[error(
        "{location:} - missing equality operator for source column {ndc_column:} in data connector {data_connector_name:} \
         which is mapped to field {field_name:} in type {type_name:}"
    )]
    MissingEqualOperator {
        location: String,
        type_name: Qualified<CustomTypeName>,
        field_name: FieldName,
        ndc_column: DataConnectorColumnName,
        data_connector_name: Qualified<DataConnectorName>,
    },
    #[error(
        "Relationships across subgraphs are not supported in filter predicates. \
         Relationship: {relationship_name:} is defined between source data connector: {source_data_connector:} \
         and target data connector: {target_data_connector:}"
    )]
    RelationshipAcrossSubgraphs {
        relationship_name: RelationshipName,
        source_data_connector: Qualified<DataConnectorName>,
        target_data_connector: Qualified<DataConnectorName>,
    },
}

impl From<TypePredicateError> for Error {
    fn from(val: TypePredicateError) -> Self {
        Error::TypePredicateError {
            type_predicate_error: val,
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum TypeError {
    #[error("expected to find a custom named type in {qualified_type_reference:} but none found")]
    NoNamedTypeFound {
        qualified_type_reference: QualifiedTypeReference,
    },
    #[error("type mismatch: {error:}")]
    TypeCheckError { error: TypecheckError },
}

impl From<AggregateExpressionError> for Error {
    fn from(val: AggregateExpressionError) -> Self {
        Error::AggregateExpressionError(val)
    }
}

impl From<TypecheckError> for Error {
    fn from(type_error: TypecheckError) -> Self {
        Error::TypeError {
            type_error: TypeError::TypeCheckError { error: type_error },
        }
    }
}
