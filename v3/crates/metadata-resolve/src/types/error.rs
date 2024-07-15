use crate::stages::{
    aggregates::AggregateExpressionError, apollo, boolean_expressions, data_connectors,
    graphql_config, object_types, scalar_boolean_expressions, type_permissions,
};
use crate::types::subgraph::{Qualified, QualifiedTypeName, QualifiedTypeReference};
use open_dds::{
    aggregates::AggregateExpressionName,
    arguments::ArgumentName,
    commands::{CommandName, FunctionName, ProcedureName},
    data_connector::{
        CollectionName, DataConnectorName, DataConnectorObjectType, DataConnectorScalarType,
    },
    models::ModelName,
    relationships::RelationshipName,
    types::{CustomTypeName, FieldName, OperatorName, TypeName, TypeReference},
};

use crate::helpers::{
    argument::ArgumentMappingError, ndc_validation::NDCValidationError,
    type_mappings::TypeMappingCollectionError, typecheck,
};

// TODO: This enum really needs structuring
#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("the following model is defined more than once: {name:}")]
    DuplicateModelDefinition { name: Qualified<ModelName> },
    #[error("'globalIdFields' for type {object_type:} found, but no model found with 'globalIdSource: true' for type {object_type:}")]
    GlobalIdSourceNotDefined {
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
    #[error("command source is required for command '{command_name:}' to resolve predicate")]
    CommandSourceRequiredForPredicate {
        command_name: Qualified<CommandName>,
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
        collection: CollectionName,
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
    #[error("unknown type represented for scalar type {scalar_type:}: {type_name:}")]
    ScalarTypeUnknownRepresentation {
        scalar_type: DataConnectorScalarType,
        type_name: Qualified<CustomTypeName>,
    },
    #[error("multiple type representations defined for scalar {scalar_type:} from data connector {data_connector:}")]
    DuplicateDataConnectorScalarRepresentation {
        data_connector: Qualified<DataConnectorName>,
        scalar_type: DataConnectorScalarType,
    },
    #[error("conflicting type representations found for data connector {data_connector:}: {old_representation:} and {new_representation:}")]
    DataConnectorScalarRepresentationMismatch {
        data_connector: Qualified<DataConnectorName>,
        old_representation: TypeName,
        new_representation: TypeName,
    },
    #[error(
        "scalar type representation required for type {scalar_type:} in data connector {data_connector:}"
    )]
    DataConnectorScalarRepresentationRequired {
        data_connector: Qualified<DataConnectorName>,
        scalar_type: DataConnectorScalarType,
    },
    #[error("type mapping required for type {type_name:} in model source {model_name:} backed by data connector {data_connector:}")]
    TypeMappingRequired {
        model_name: Qualified<ModelName>,
        type_name: Qualified<CustomTypeName>,
        data_connector: Qualified<DataConnectorName>,
    },
    #[error("Model {model_name:} is marked as a global ID source but there are no global id fields present in the related object type {type_name:}")]
    NoGlobalFieldsPresentInGlobalIdSource {
        type_name: Qualified<CustomTypeName>,
        model_name: ModelName,
    },
    #[error("Found multiple models  {model_1:}, {model_2:} that implement the same object type {object_type:} to be global ID sources.")]
    DuplicateModelGlobalIdSource {
        model_1: Qualified<ModelName>,
        model_2: Qualified<ModelName>,
        object_type: Qualified<CustomTypeName>,
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
    #[error("An error occurred while mapping arguments in the model {model_name:} to the collection {collection_name:} in the data connector {data_connector_name:}: {error:}")]
    ModelCollectionArgumentMappingError {
        data_connector_name: Qualified<DataConnectorName>,
        model_name: Qualified<ModelName>,
        collection_name: CollectionName,
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
        #[from]
        graphql_config_error: graphql_config::GraphqlConfigError,
    },
    #[error("{relationship_error:}")]
    RelationshipError {
        relationship_error: RelationshipError,
    },
    #[error("{0}")]
    BooleanExpressionError(#[from] boolean_expressions::BooleanExpressionError),
    #[error("{0}")]
    ScalarBooleanExpressionTypeError(
        #[from] scalar_boolean_expressions::ScalarBooleanExpressionTypeError,
    ),
    #[error("{type_predicate_error:}")]
    TypePredicateError {
        type_predicate_error: TypePredicateError,
    },
    #[error("{type_error:}")]
    TypeError { type_error: TypeError },
    #[error("{0}")]
    AggregateExpressionError(AggregateExpressionError),
    #[error("{0}")]
    ModelAggregateExpressionError(ModelAggregateExpressionError),
    #[error("{0}")]
    DataConnectorError(#[from] data_connectors::DataConnectorError),
    #[error("{0}")]
    TypePermissionError(type_permissions::TypePermissionError),
    #[error("{0}")]
    ObjectTypesError(#[from] object_types::ObjectTypesError),
    #[error("{0}")]
    ApolloError(#[from] apollo::ApolloError),
}

#[derive(Debug, thiserror::Error)]
pub enum ModelAggregateExpressionError {
    #[error("a source must be defined for model {model:} in order to use aggregate expressions")]
    CannotUseAggregateExpressionsWithoutSource { model: Qualified<ModelName> },
    #[error("the aggregate expression {aggregate_expression} used with model {model_name} has not been defined")]
    UnknownModelAggregateExpression {
        model_name: Qualified<ModelName>,
        aggregate_expression: Qualified<AggregateExpressionName>,
    },
    #[error("the aggregate expression {aggregate_expression} is used with the model {model_name} but its operand type {aggregate_operand_type} does not match the model's type {model_type}")]
    ModelAggregateExpressionOperandTypeMismatch {
        model_name: Qualified<ModelName>,
        aggregate_expression: Qualified<AggregateExpressionName>,
        model_type: QualifiedTypeName,
        aggregate_operand_type: QualifiedTypeName,
    },
    #[error("the aggregate expression {aggregate_expression} is used with the model {model_name} which has the countDistinct aggregation enabled, but countDistinct is not valid when aggregating a model as every object is already logically distinct")]
    ModelAggregateExpressionCountDistinctNotAllowed {
        model_name: Qualified<ModelName>,
        aggregate_expression: Qualified<AggregateExpressionName>,
    },
    #[error("the aggregate expression {aggregate_expression} is used with the model {model_name} but the NDC type of the field {field_name} for data connector {data_connector_name} was not a optionally nullable named type")]
    ModelAggregateExpressionUnexpectedDataConnectorType {
        model_name: Qualified<ModelName>,
        aggregate_expression: Qualified<AggregateExpressionName>,
        data_connector_name: Qualified<DataConnectorName>,
        field_name: FieldName,
    },
    #[error("the aggregate expression {aggregate_expression} is used with the model {model_name} but for the data connector {data_connector_name} and scalar type {data_connector_operand_type}, mappings are not provided for all aggregation functions in the aggregate expression")]
    ModelAggregateExpressionDataConnectorMappingMissing {
        model_name: Qualified<ModelName>,
        aggregate_expression: Qualified<AggregateExpressionName>,
        data_connector_name: Qualified<DataConnectorName>,
        data_connector_operand_type: DataConnectorScalarType,
    },
    #[error("error in aggregate expression {aggregate_expression} used with the model {model_name}: {object_type_error}")]
    ModelAggregateObjectTypeError {
        model_name: Qualified<ModelName>,
        aggregate_expression: Qualified<AggregateExpressionName>,
        object_type_error: object_types::ObjectTypesError,
    },
    #[error("{0}")]
    OtherError(Box<Error>),
}

impl From<ModelAggregateExpressionError> for Error {
    fn from(val: ModelAggregateExpressionError) -> Self {
        Error::ModelAggregateExpressionError(val)
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
        error: ModelAggregateExpressionError,
    },
}

impl From<RelationshipError> for Error {
    fn from(val: RelationshipError) -> Self {
        Error::RelationshipError {
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
    #[error("operator mappings not found for data connector {data_connector_name:}")]
    OperatorMappingsNotFound {
        data_connector_name: Qualified<DataConnectorName>,
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
}

impl From<AggregateExpressionError> for Error {
    fn from(val: AggregateExpressionError) -> Self {
        Error::AggregateExpressionError(val)
    }
}
