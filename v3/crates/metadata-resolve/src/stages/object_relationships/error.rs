use crate::stages::{commands, graphql_config, models};
use crate::types::subgraph::{Qualified, QualifiedTypeReference};
use lang_graphql::ast::common as ast;
use open_dds::{
    arguments::ArgumentName,
    commands::CommandName,
    data_connector::DataConnectorName,
    models::ModelName,
    relationships::RelationshipName,
    types::{CustomTypeName, FieldName},
};

#[derive(Debug, thiserror::Error)]
pub enum RelationshipError {
    #[error(
        "duplicate relationship field {field_name} from {relationship_name} associated with source type {type_name}"
    )]
    DuplicateRelationshipFieldInSourceType {
        field_name: ast::Name,
        type_name: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
    },

    #[error(
        "unknown target model {model_name:} used in relationship {relationship_name:} on type {type_name:}"
    )]
    UnknownTargetModelUsedInRelationship {
        type_name: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
        model_name: Qualified<ModelName>,
    },

    #[error(
        "unknown target command {command_name:} used in relationship {relationship_name:} on type {type_name:}"
    )]
    UnknownTargetCommandUsedInRelationship {
        type_name: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
        command_name: Qualified<CommandName>,
    },

    #[error(
        "source field {field_name} in field mapping for relationship {relationship_name} on type {source_type} is unknown."
    )]
    UnknownSourceFieldInRelationshipMapping {
        source_type: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
        field_name: FieldName,
    },
    #[error(
        "target field {field_name} in field mapping for relationship {relationship_name} on type {source_type} to model {model_name} is unknown."
    )]
    UnknownTargetFieldInRelationshipMapping {
        source_type: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
        model_name: Qualified<ModelName>,
        field_name: FieldName,
    },
    #[error(
        "target argument {argument_name} in argument mapping for relationship {relationship_name} on type {source_type} to model {model_name} is unknown."
    )]
    UnknownTargetModelArgumentInRelationshipMapping {
        source_type: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
        model_name: Qualified<ModelName>,
        argument_name: ArgumentName,
    },
    #[error(
        "target argument {argument_name} in argument mapping for relationship {relationship_name} on type {source_type} to command {command_name} is unknown."
    )]
    UnknownTargetCommandArgumentInRelationshipMapping {
        source_type: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
        command_name: Qualified<CommandName>,
        argument_name: ArgumentName,
    },
    #[error(
        "Mapping for source field {field_name} already exists in the relationship {relationship_name} on type {type_name}"
    )]
    MappingExistsInRelationship {
        type_name: Qualified<CustomTypeName>,
        field_name: FieldName,
        relationship_name: RelationshipName,
    },
    #[error(
        "The target argument {argument_name} of model {model_name} has been mapped more than once in the relationship {relationship_name} on type {type_name}"
    )]
    ModelArgumentMappingExistsInRelationship {
        argument_name: ArgumentName,
        model_name: Qualified<ModelName>,
        relationship_name: RelationshipName,
        type_name: Qualified<CustomTypeName>,
    },
    #[error(
        "The target argument {argument_name} of command {command_name} has been mapped more than once in the relationship {relationship_name} on type {type_name}"
    )]
    CommandArgumentMappingExistsInRelationship {
        argument_name: ArgumentName,
        command_name: Qualified<CommandName>,
        relationship_name: RelationshipName,
        type_name: Qualified<CustomTypeName>,
    },
    #[error(
        "No mapping for target command argument {argument_name} in the relationship {relationship_name} on type {type_name}"
    )]
    MissingArgumentMappingInRelationship {
        type_name: Qualified<CustomTypeName>,
        argument_name: ArgumentName,
        relationship_name: RelationshipName,
    },
    #[error(
        "The target data connector {data_connector_name} for relationship {relationship_name} on type {type_name} does not support the variables capability"
    )]
    RelationshipTargetDoesNotSupportForEach {
        type_name: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
        data_connector_name: Qualified<DataConnectorName>,
    },
    #[error(
        "The target data connector {data_connector_name} for relationship {relationship_name} on type {type_name} has not defined any capabilities"
    )]
    NoRelationshipCapabilitiesDefined {
        type_name: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
        data_connector_name: Qualified<DataConnectorName>,
    },
    #[error(
        "The relationship {relationship_name} on type {type_name} defines an aggregate, but aggregates can only be used with array relationships, not object relationships"
    )]
    AggregateIsOnlyAllowedOnArrayRelationships {
        type_name: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
    },
    #[error(
        "The aggregate defined on the relationship {relationship_name} on type {type_name} has an error: {error}"
    )]
    ModelAggregateExpressionError {
        type_name: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
        error: models::ModelsError, // ideally, this would return the more accurate
                                    // `ModelAggregateExpressionError` instead
    },
    #[error(
        "The source field '{source_field_name}' of type '{source_field_type}' in the relationship '{relationship_name}' on type '{source_type}' cannot be mapped to the target argument '{target_argument_name}' of type '{target_argument_type}' on the target model '{target_model_name}' because their types are incompatible"
    )]
    ModelArgumentTargetMappingTypeMismatch {
        source_type: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
        source_field_name: FieldName,
        source_field_type: QualifiedTypeReference,
        target_model_name: Qualified<ModelName>,
        target_argument_name: ArgumentName,
        target_argument_type: QualifiedTypeReference,
    },
    #[error("Relationship mappings from value expressions are not supported yet.")]
    ValueExpressionMappingsNotSupportedYet,
    #[error(
        "The field path provided in the {location:} of the relationship {relationship_name} on type {type_name} is empty"
    )]
    EmptyFieldPath {
        location: String,
        relationship_name: RelationshipName,
        type_name: Qualified<CustomTypeName>,
    },
    #[error(
        "Model fields cannot be used in command based relationship: {relationship_name:} on type {type_name:}"
    )]
    ModelFieldCannotBeUsedInCommandRelationship {
        relationship_name: RelationshipName,
        type_name: Qualified<CustomTypeName>,
    },
    #[error("Relationships with nested field paths are not supported yet.")]
    NestedFieldPathsNotSupportedYet,
    #[error("{0}")]
    CommandError(#[from] commands::CommandsError),
    #[error("{0}")]
    ModelError(#[from] models::ModelsError),
    #[error("{0}")]
    GraphqlError(#[from] graphql_config::GraphqlConfigError),
}
