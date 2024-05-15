use open_dds::data_connector::DataConnectorObjectType;
use thiserror::Error;

use crate::helpers::argument::ArgumentMappingError;
use crate::types::subgraph::{Qualified, QualifiedTypeReference};
use lang_graphql::ast::common as ast;
use open_dds::{
    arguments::ArgumentName,
    commands::{CommandName, FunctionName, ProcedureName},
    data_connector::DataConnectorName,
    models::ModelName,
    relationships::RelationshipName,
    types::{CustomTypeName, FieldName, OperatorName, TypeReference},
};

use crate::helpers::{
    ndc_validation::NDCValidationError, type_mappings::TypeMappingCollectionError, typecheck,
};

// TODO: This enum really needs structuring
#[derive(Error, Debug)]
pub enum Error {
    #[error("the following data connector is defined more than once: {name:}")]
    DuplicateDataConnectorDefinition { name: Qualified<DataConnectorName> },
    #[error("the following type is defined more than once: {name:}")]
    DuplicateTypeDefinition { name: Qualified<CustomTypeName> },
    #[error("the following field in type {type_name:} is defined more than once: {field_name:}")]
    DuplicateFieldDefinition {
        type_name: Qualified<CustomTypeName>,
        field_name: FieldName,
    },
    #[error("{error:} in object type {type_name:}")]
    DataConnectorTypeMappingValidationError {
        type_name: Qualified<CustomTypeName>,
        error: TypeMappingValidationError,
    },
    #[error("Multiple mappings have been defined from object {data_connector_object_type:} of data connector {data_connector:}")]
    DuplicateDataConnectorObjectTypeMapping {
        data_connector: Qualified<DataConnectorName>,
        data_connector_object_type: String,
    },
    #[error("the following model is defined more than once: {name:}")]
    DuplicateModelDefinition { name: Qualified<ModelName> },
    #[error("'globalIdFields' for type {object_type:} found, but no model found with 'globalIdSource: true' for type {object_type:}")]
    GlobalIdSourceNotDefined {
        object_type: Qualified<CustomTypeName>,
    },
    #[error("'apolloFederation.keys' for type {object_type:} found, but no model found with 'apolloFederation.entitySource: true' for type {object_type:}")]
    ApolloFederationEntitySourceNotDefined {
        object_type: Qualified<CustomTypeName>,
    },
    #[error("the data type {data_type:} for model {model_name:} has not been defined")]
    UnknownModelDataType {
        model_name: Qualified<ModelName>,
        data_type: Qualified<CustomTypeName>,
    },
    #[error(
        "the following argument in model {model_name:} is defined more than once: {argument_name:}"
    )]
    DuplicateModelArgumentDefinition {
        model_name: Qualified<ModelName>,
        argument_name: ArgumentName,
    },
    #[error("unknown field {field_name:} in filterable fields defined for model {model_name:}")]
    UnknownFieldInFilterableFields {
        model_name: ModelName,
        field_name: FieldName,
    },
    #[error("unknown field {field_name:} in orderable fields defined for model {model_name:}")]
    UnknownFieldInOrderableFields {
        model_name: ModelName,
        field_name: FieldName,
    },
    #[error("source for the following model is defined more than once: {model_name:}")]
    DuplicateModelSourceDefinition { model_name: Qualified<ModelName> },
    #[error("{error:} in model {model_name:}")]
    ModelTypeMappingCollectionError {
        model_name: Qualified<ModelName>,
        error: TypeMappingCollectionError,
    },
    // ------- Errors for commands ---------
    #[error("the following command is defined more than once: {name:}")]
    DuplicateCommandDefinition { name: Qualified<CommandName> },
    #[error("the following argument {argument_name:} with argument type {argument_type:} in command {command_name:} ) has not been defined")]
    UnknownCommandArgumentType {
        command_name: Qualified<CommandName>,
        argument_name: ArgumentName,
        argument_type: TypeReference,
    },
    #[error(
        "the following argument in command {command_name:} is defined more than once: {argument_name:}"
    )]
    DuplicateCommandArgumentDefinition {
        command_name: Qualified<CommandName>,
        argument_name: ArgumentName,
    },
    #[error("source for the following command is defined more than once: {command_name:}")]
    DuplicateCommandSourceDefinition {
        command_name: Qualified<CommandName>,
    },
    #[error("the source data connector {data_connector:} for command {command_name:} has not been defined")]
    UnknownCommandDataConnector {
        command_name: Qualified<CommandName>,
        data_connector: Qualified<DataConnectorName>,
    },
    #[error(
        "the mapping for type {type_name:} in command {command_name:} is defined more than once"
    )]
    DuplicateTypeMappingDefinitionInCommandSource {
        command_name: Qualified<CommandName>,
        type_name: CustomTypeName,
    },
    #[error(
        "unknown argument {argument_name:} referenced in argument mappings for command {command_name:}"
    )]
    UnknownCommandSourceArgument {
        command_name: Qualified<CommandName>,
        argument_name: ArgumentName,
    },
    #[error(
        "the mapping for argument {argument_name:} of command {command_name:} has been defined more than once"
    )]
    DuplicateCommandArgumentMapping {
        command_name: Qualified<CommandName>,
        argument_name: ArgumentName,
    },
    #[error("a preset argument {argument_name:} has been set for the command {command_name:} but no such argument exists for this command")]
    CommandArgumentPresetMismatch {
        command_name: Qualified<CommandName>,
        argument_name: ArgumentName,
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

    #[error("the procedure {procedure:} in the data connector {data_connector:} for command {command_name:} has not been defined")]
    UnknownCommandProcedure {
        command_name: Qualified<CommandName>,
        data_connector: Qualified<DataConnectorName>,
        procedure: ProcedureName,
    },
    #[error("the function {function:} in the data connector {data_connector:} for command {command_name:} has not been defined")]
    UnknownCommandFunction {
        command_name: Qualified<CommandName>,
        data_connector: Qualified<DataConnectorName>,
        function: FunctionName,
    },
    #[error("{error:} in command {command_name:}")]
    CommandTypeMappingCollectionError {
        command_name: Qualified<CommandName>,
        error: TypeMappingCollectionError,
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
        "the source data connector {data_connector:} for model {model_name:} has not been defined"
    )]
    UnknownModelDataConnector {
        model_name: Qualified<ModelName>,
        data_connector: Qualified<DataConnectorName>,
    },
    #[error("the collection {collection:} in the data connector {data_connector:} for model {model_name:} has not been defined")]
    UnknownModelCollection {
        model_name: Qualified<ModelName>,
        data_connector: Qualified<DataConnectorName>,
        collection: String,
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
    #[error("multiple graphql types found with the same name: {graphql_type_name:}")]
    ConflictingGraphQlType { graphql_type_name: ast::TypeName },
    #[error("unknown field {field_name:} in unique identifier defined for model {model_name:}")]
    UnknownFieldInUniqueIdentifier {
        model_name: Qualified<ModelName>,
        field_name: FieldName,
    },
    #[error("unknown field {field_name:} in apollo federation keys defined for the object type {object_type:}")]
    UnknownFieldInApolloFederationKey {
        field_name: FieldName,
        object_type: Qualified<CustomTypeName>,
    },
    #[error(
        "empty keys in apollo federation configuration defined for the object type {object_type:}"
    )]
    EmptyKeysInApolloFederationConfigForObject {
        object_type: Qualified<CustomTypeName>,
    },
    #[error("empty fields in apollo federation keys defined for the object type {object_type:}")]
    EmptyFieldsInApolloFederationConfigForObject {
        object_type: Qualified<CustomTypeName>,
    },
    #[error("multiple models are marked as entity source for the object type {type_name:}")]
    MultipleEntitySourcesForType {
        type_name: Qualified<CustomTypeName>,
    },
    #[error("duplicate field {field_name:} in unique identifier defined for model {model_name:}")]
    DuplicateFieldInUniqueIdentifier {
        model_name: Qualified<ModelName>,
        field_name: FieldName,
    },
    #[error("no equality operator has been defined in the data connector for field {field_name:} of model {model_name:} used in {comparison_location}")]
    NoEqualOperatorForComparedField {
        comparison_location: String,
        model_name: Qualified<ModelName>,
        field_name: FieldName,
    },
    #[error("multiple equality operators have been defined in the data connector for field {field_name:} of model {model_name:} used in {comparison_location}")]
    MultipleEqualOperatorsForComparedField {
        comparison_location: String,
        model_name: Qualified<ModelName>,
        field_name: FieldName,
    },
    #[error("no field mapping was found for field {field_name:} of model {model_name:} used in {comparison_location}")]
    NoFieldMappingForComparedField {
        comparison_location: String,
        model_name: Qualified<ModelName>,
        field_name: FieldName,
    },
    #[error("comparison for non-scalar field {field_name:} of model {model_name:} used in {comparison_location} is unsupported")]
    UncomparableNonScalarFieldType {
        comparison_location: String,
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
    // Permission errors
    // Type Output Permissions
    #[error("unsupported type in output type permissions definition: {type_name:}; only object types are supported")]
    UnsupportedTypeInOutputPermissions { type_name: CustomTypeName },
    #[error("multiple output type permissions have been defined for type: {type_name:}")]
    DuplicateOutputTypePermissions { type_name: CustomTypeName },
    #[error("unknown type used in output permissions: {type_name:}")]
    UnknownTypeInOutputPermissionsDefinition {
        type_name: Qualified<CustomTypeName>,
    },
    #[error("unknown field '{field_name:}' used in output permissions of type '{type_name:}'")]
    UnknownFieldInOutputPermissionsDefinition {
        field_name: FieldName,
        type_name: CustomTypeName,
    },
    // Type Input Permissions
    #[error("unsupported type in input type permissions definition: {type_name:}; only object types are supported")]
    UnsupportedTypeInInputPermissions { type_name: CustomTypeName },
    #[error("unknown field '{field_name:}' used in output permissions of type '{type_name:}'")]
    UnknownFieldInInputPermissionsDefinition {
        field_name: FieldName,
        type_name: CustomTypeName,
    },
    #[error("multiple input type permissions have been defined for type: {type_name:}")]
    DuplicateInputTypePermissions { type_name: CustomTypeName },
    #[error(
        "Type error in field preset of {field_name:}, for input type permissions definition of type {type_name:}: {type_error:}"
    )]
    FieldPresetTypeError {
        field_name: FieldName,
        type_name: CustomTypeName,
        type_error: typecheck::TypecheckError,
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
    #[error(
        "model source is required for model '{model_name:}' to resolve select permission predicate"
    )]
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
    #[error("NDC validation error: {0}")]
    NDCValidationError(#[from] NDCValidationError),
    #[error(
        "duplicate relationship {relationship_name:} associated with source type {type_name:}"
    )]
    DuplicateRelationshipInSourceType {
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
    #[error("Source type {type_name:} referenced in the definition of relationship(s) {relationship_name:} is not defined ")]
    RelationshipDefinedOnUnknownType {
        relationship_name: RelationshipName,
        type_name: Qualified<CustomTypeName>,
    },
    #[error("{reason:}")]
    NotSupported { reason: String },
    #[error("The field path provided in the {location:} of the relationship {relationship_name} on type {type_name} is empty")]
    EmptyFieldPath {
        location: String,
        relationship_name: RelationshipName,
        type_name: Qualified<CustomTypeName>,
    },
    #[error("unknown data connector {data_connector:} referenced in scalar type representation of {scalar_type:}")]
    ScalarTypeFromUnknownDataConnector {
        data_connector: Qualified<DataConnectorName>,
        scalar_type: String,
    },
    #[error("cannot find scalar type {scalar_type:} in data connector {data_connector:}")]
    UnknownScalarTypeInDataConnector {
        data_connector: Qualified<DataConnectorName>,
        scalar_type: String,
    },
    #[error("unknown type represented for scalar type {scalar_type:}: {type_name:}")]
    ScalarTypeUnknownRepresentation {
        scalar_type: String,
        type_name: Qualified<CustomTypeName>,
    },
    #[error("multiple type representations defined for scalar {scalar_type:} from data connector {data_connector:}")]
    DuplicateDataConnectorScalarRepresentation {
        data_connector: Qualified<DataConnectorName>,
        scalar_type: String,
    },
    #[error(
        "scalar type representation required for type {scalar_type:} in data connector {data_connector:}"
    )]
    DataConnectorScalarRepresentationRequired {
        data_connector: Qualified<DataConnectorName>,
        scalar_type: String,
    },
    #[error("type mapping required for type {type_name:} in model source {model_name:} backed by data connector {data_connector:}")]
    TypeMappingRequired {
        model_name: Qualified<ModelName>,
        type_name: Qualified<CustomTypeName>,
        data_connector: Qualified<DataConnectorName>,
    },
    #[error("Unknown field {field_name:} in global_id defined for the type {type_name:}")]
    UnknownFieldInGlobalId {
        field_name: FieldName,
        type_name: Qualified<CustomTypeName>,
    },
    #[error("Model {model_name:} is marked as a global ID source but there are no global id fields present in the related object type {type_name:}")]
    NoGlobalFieldsPresentInGlobalIdSource {
        type_name: Qualified<CustomTypeName>,
        model_name: ModelName,
    },
    #[error("Model {model_name:} is marked as an Apollo Federation entity source but there are no keys fields present in the related object type {type_name:}")]
    NoKeysFieldsPresentInEntitySource {
        type_name: Qualified<CustomTypeName>,
        model_name: ModelName,
    },
    #[error("A field named `id` cannot be present in the object type {type_name} when global_id fields are non-empty.")]
    IdFieldConflictingGlobalId {
        type_name: Qualified<CustomTypeName>,
    },
    #[error("Found multiple models  {model_1:}, {model_2:} that implement the same object type {object_type:} to be global ID sources.")]
    DuplicateModelGlobalIdSource {
        model_1: Qualified<ModelName>,
        model_2: Qualified<ModelName>,
        object_type: Qualified<CustomTypeName>,
    },
    #[error("\"{name:}\" is not a valid GraphQL name.")]
    InvalidGraphQlName { name: String },
    #[error("Invalid header name {header_name} specified for data connector: {data_connector}.")]
    InvalidHeaderName {
        data_connector: Qualified<DataConnectorName>,
        header_name: String,
    },
    #[error(
        "Invalid value specified for header {header_name} for data connector: {data_connector}."
    )]
    InvalidHeaderValue {
        data_connector: Qualified<DataConnectorName>,
        header_name: String,
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

    #[error("model {model_name:} with arguments is unsupported as a global ID source")]
    ModelWithArgumentsAsGlobalIdSource { model_name: Qualified<ModelName> },
    #[error(
        "model {model_name:} with arguments is unsupported as an Apollo Federation entity source"
    )]
    ModelWithArgumentsAsApolloFederationEntitySource { model_name: Qualified<ModelName> },
    #[error("An error occurred while mapping arguments in the model {model_name:} to the collection {collection_name:} in the data connector {data_connector_name:}: {error:}")]
    ModelCollectionArgumentMappingError {
        data_connector_name: Qualified<DataConnectorName>,
        model_name: Qualified<ModelName>,
        collection_name: String,
        error: ArgumentMappingError,
    },
    #[error("An error occurred while mapping arguments in the command {command_name:} to the function {function_name:} in the data connector {data_connector_name:}: {error:}")]
    CommandFunctionArgumentMappingError {
        data_connector_name: Qualified<DataConnectorName>,
        command_name: Qualified<CommandName>,
        function_name: FunctionName,
        error: ArgumentMappingError,
    },
    #[error("An error occurred while mapping arguments in the command {command_name:} to the procedure {procedure_name:} in the data connector {data_connector_name:}: {error:}")]
    CommandProcedureArgumentMappingError {
        data_connector_name: Qualified<DataConnectorName>,
        command_name: Qualified<CommandName>,
        procedure_name: ProcedureName,
        error: ArgumentMappingError,
    },
    #[error("The url for the data connector {data_connector_name:} is invalid: {error:}")]
    InvalidDataConnectorUrl {
        data_connector_name: Qualified<DataConnectorName>,
        error: url::ParseError,
    },
    #[error("Predicate types in data connectors are unsupported")]
    PredicateTypesUnsupported,
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
        graphql_config_error: GraphqlConfigError,
    },
    #[error("{relationship_error:}")]
    RelationshipError {
        relationship_error: RelationshipError,
    },
    #[error("{boolean_expression_error:}")]
    BooleanExpressionError {
        boolean_expression_error: BooleanExpressionError,
    },
    #[error("{type_predicate_error:}")]
    TypePredicateError {
        type_predicate_error: TypePredicateError,
    },
    #[error("{type_error:}")]
    TypeError { type_error: TypeError },
}

impl From<BooleanExpressionError> for Error {
    fn from(val: BooleanExpressionError) -> Self {
        Error::BooleanExpressionError {
            boolean_expression_error: val,
        }
    }
}

#[derive(Debug, Error)]
pub enum BooleanExpressionError {
    #[error("unknown type used in object boolean expression: {type_name:}")]
    UnknownTypeInObjectBooleanExpressionType {
        type_name: Qualified<CustomTypeName>,
    },
    #[error("unsupported type used in object boolean expression: {type_name:}; only object types are supported")]
    UnsupportedTypeInObjectBooleanExpressionType {
        type_name: Qualified<CustomTypeName>,
    },
    #[error("unknown data connector {data_connector:} referenced in object boolean expression type {object_boolean_expression_type:}")]
    UnknownDataConnectorInObjectBooleanExpressionType {
        data_connector: Qualified<DataConnectorName>,
        object_boolean_expression_type: Qualified<CustomTypeName>,
    },
    #[error("unknown data connector object type {data_connector_object_type:} (in data connector {data_connector:}) referenced in object boolean expression type {object_boolean_expression_type:}")]
    UnknownDataConnectorTypeInObjectBooleanExpressionType {
        data_connector: Qualified<DataConnectorName>,
        data_connector_object_type: DataConnectorObjectType,
        object_boolean_expression_type: Qualified<CustomTypeName>,
    },
    #[error("unknown field '{field_name:}' used in object boolean expression type {object_boolean_expression_type:}")]
    UnknownFieldInObjectBooleanExpressionType {
        field_name: FieldName,
        object_boolean_expression_type: Qualified<CustomTypeName>,
    },
    #[error("the object type '{object_type:}' used in boolean expression type {object_boolean_expression_type:} does not have a mapping to object {data_connector_object_type:} of data connector {data_connector:}")]
    NoDataConnectorTypeMappingForObjectTypeInBooleanExpression {
        object_type: Qualified<CustomTypeName>,
        object_boolean_expression_type: Qualified<CustomTypeName>,
        data_connector_object_type: DataConnectorObjectType,
        data_connector: Qualified<DataConnectorName>,
    },
    #[error("{error:} in boolean expression type {object_boolean_expression_type:}")]
    BooleanExpressionTypeMappingCollectionError {
        object_boolean_expression_type: Qualified<CustomTypeName>,
        error: TypeMappingCollectionError,
    },
    #[error("the following object boolean expression type is defined more than once: {name:}")]
    DuplicateObjectBooleanExpressionTypeDefinition { name: Qualified<CustomTypeName> },
    #[error("unknown object boolean expression type {name:} is used in model {model:}")]
    UnknownBooleanExpressionTypeInModel {
        name: Qualified<CustomTypeName>,
        model: Qualified<ModelName>,
    },
    #[error("the boolean expression type {name:} used in model {model:} corresponds to object type {boolean_expression_object_type:} whereas the model's object type is {model_object_type:}")]
    BooleanExpressionTypeForInvalidObjectTypeInModel {
        name: Qualified<CustomTypeName>,
        boolean_expression_object_type: Qualified<CustomTypeName>,
        model: Qualified<ModelName>,
        model_object_type: Qualified<CustomTypeName>,
    },
}

#[derive(Debug, Error)]
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
}

#[derive(Debug, Error)]
pub enum TypePredicateError {
    #[error("unknown field '{field_name:}' used in predicate for type '{type_name:}'")]
    UnknownFieldInTypePredicate {
        field_name: FieldName,
        type_name: Qualified<CustomTypeName>,
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
    #[error("relationship '{relationship_name:}' used in predicate for type '{type_name:}' does not exist")]
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
}

impl From<TypePredicateError> for Error {
    fn from(val: TypePredicateError) -> Self {
        Error::TypePredicateError {
            type_predicate_error: val,
        }
    }
}

#[derive(Debug, Error)]
pub enum GraphqlConfigError {
    #[error("graphql configuration is not defined in supergraph")]
    MissingGraphqlConfig,
    #[error("graphql configuration should be defined only once in supergraph")]
    MultipleGraphqlConfigDefinition,
    #[error("the fieldName for limitInput need to be defined in GraphqlConfig, when models have selectMany graphql API")]
    MissingLimitFieldInGraphqlConfig,
    #[error("the fieldName for offsetInput need to be defined in GraphqlConfig, when models have selectMany graphql API")]
    MissingOffsetFieldInGraphqlConfig,
    #[error("the filterInput need to be defined in GraphqlConfig, when models have filterExpressionType")]
    MissingFilterInputFieldInGraphqlConfig,
    #[error("the orderByInput need to be defined in GraphqlConfig, when models have orderByExpressionType")]
    MissingOrderByInputFieldInGraphqlConfig,
    #[error("the orderByInput.enumTypeNames need to be defined in GraphqlConfig, when models have orderByExpressionType")]
    MissingOrderByEnumTypeNamesInGraphqlConfig,
    #[error("only one enumTypeNames can be defined in GraphqlConfig, whose direction values are both 'asc' and 'desc'.")]
    MultipleOrderByEnumTypeNamesInGraphqlConfig,
    #[error(
            "invalid directions: {directions} defined in orderByInput of GraphqlConfig , currenlty there is no support for partial directions. Please specify a type which has both 'asc' and 'desc' directions"
        )]
    InvalidOrderByDirection { directions: String },
    #[error("the fieldName for argumentsInput need to be defined in GraphqlConfig, when models have argumentsInputType")]
    MissingArgumentsInputFieldInGraphqlConfig,
}

#[derive(Error, Debug)]
pub enum TypeError {
    #[error("expected to find a custom named type in {qualified_type_reference:} but none found")]
    NoNamedTypeFound {
        qualified_type_reference: QualifiedTypeReference,
    },
}

#[derive(Error, Debug)]
pub enum TypeMappingValidationError {
    #[error("data connector {data_connector:} referenced in type mappings of type {type_name:} is not found")]
    UnknownDataConnector {
        data_connector: Qualified<DataConnectorName>,
        type_name: Qualified<CustomTypeName>,
    },
    #[error("the type {type_name:} referenced in type mappings has not been defined")]
    UnknownSourceType {
        type_name: Qualified<CustomTypeName>,
    },
    #[error("the mapping for type {type_name:} is incompatible with the type representation")]
    IncompatibleTypeMappingDefinition {
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
    #[error("unknown target column name {column_name:} for field {field_name:} ")]
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
    #[error("could not find mappings for {object_type_name:} to the {data_connector_object_type:} on data connector {data_connector_name:}")]
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
    NDCValidationError(NDCValidationError),
}
