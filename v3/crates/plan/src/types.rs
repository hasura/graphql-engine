use crate::error::InternalError;
use crate::query::{ArgumentPresetExecutionError, RelationshipFieldMappingError};
use authorization_rules::ConditionCache;
use metadata_resolve::Qualified;
use open_dds::data_connector::DataConnectorOperatorName;
use open_dds::{
    arguments::ArgumentName,
    commands::CommandName,
    data_connector::{DataConnectorColumnName, DataConnectorName},
    models::ModelName,
    relationships::RelationshipName,
    types::{CustomTypeName, FieldName},
};
use plan_types::UniqueNumber;
use tracing_util::{ErrorVisibility, TraceableError};

#[derive(Debug, thiserror::Error)]
pub enum PlanError {
    #[error("{0}")]
    Permission(#[from] PermissionError),
    #[error("{0}")]
    Relationship(#[from] RelationshipError),
    #[error("{0}")]
    OrderBy(#[from] OrderByError),
    #[error("{0}")]
    BooleanExpression(#[from] BooleanExpressionError),
    #[error("{0}")]
    ArgumentPresetExecutionError(#[from] ArgumentPresetExecutionError),
    #[error("{0}")]
    InternalError(InternalError),
    #[error("{0}")]
    Internal(String), // equivalent to DataFusionError::Internal
}

impl TraceableError for PlanError {
    fn visibility(&self) -> ErrorVisibility {
        match self {
            Self::InternalError(error) => error.visibility(),
            Self::ArgumentPresetExecutionError(error) => error.visibility(),
            Self::Permission(permission_error) => permission_error.visibility(),
            Self::Relationship(relationship_error) => relationship_error.visibility(),
            Self::OrderBy(order_by_error) => order_by_error.visibility(),
            Self::BooleanExpression(boolean_expression_error) => {
                boolean_expression_error.visibility()
            }
            Self::Internal(_) => ErrorVisibility::Internal,
        }
    }
}

#[derive(Debug, thiserror::Error)]
// errors thrown during permissions evaluation, but not necessary errors due to permisssions
pub enum PermissionError {
    #[error("command {command_name:} could not be found")]
    CommandNotFound {
        command_name: Qualified<CommandName>,
    },
    #[error("no permission to select from command {command_name:}")]
    CommandNotAccessible {
        command_name: Qualified<CommandName>,
    },
    #[error("model {model_name:} could not be found")]
    ModelNotFound { model_name: Qualified<ModelName> },
    #[error("model {model_name:} has no source")]
    ModelHasNoSource { model_name: Qualified<ModelName> },

    #[error("no permission to select from model {model_name:}")]
    ModelNotAccessible { model_name: Qualified<ModelName> },

    #[error("object type {object_type_name:} could not be found")]
    ObjectTypeNotFound {
        object_type_name: Qualified<CustomTypeName>,
    },
    #[error("no permission to select from type {object_type_name:}")]
    ObjectTypeNotAccessible {
        object_type_name: Qualified<CustomTypeName>,
    },
    #[error("no permission to select from field {field_name:} in type {object_type_name:}")]
    ObjectFieldNotFound {
        object_type_name: Qualified<CustomTypeName>,
        field_name: FieldName,
    },
    #[error("Object boolean expression type {boolean_expression_type_name} could not be found")]
    ObjectBooleanExpressionTypeNotFound {
        boolean_expression_type_name: Qualified<CustomTypeName>,
    },
    #[error("Relationship {relationship_name} not found for object type {object_type_name}")]
    RelationshipNotFound {
        object_type_name: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
    },
    #[error(
        "Internal error: Relationship capabilities are missing for {relationship_name} on type {object_type_name}"
    )]
    InternalMissingRelationshipCapabilities {
        object_type_name: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
    },
    #[error(
        "Field {field_name} not found in object boolean expression type {boolean_expression_type_name}"
    )]
    FieldNotFoundInBooleanExpressionType {
        field_name: FieldName,
        boolean_expression_type_name: Qualified<CustomTypeName>,
    },
    #[error(
        "Relationship {relationship_name} not found in object boolean expression type {boolean_expression_type_name}"
    )]
    RelationshipNotFoundInBooleanExpressionType {
        relationship_name: RelationshipName,
        boolean_expression_type_name: Qualified<CustomTypeName>,
    },
    #[error("Error evaluating condition: {0}")]
    ConditionEvaluationError(#[from] authorization_rules::ConditionError),

    #[error("Nested scalar filtering is not supported by data connector {data_connector_name}")]
    NestedScalarFilteringNotSupported {
        data_connector_name: Qualified<DataConnectorName>,
    },

    #[error("{0}")]
    Other(String),
}

impl TraceableError for PermissionError {
    fn visibility(&self) -> ErrorVisibility {
        match self {
            Self::ObjectTypeNotFound { .. }
            | Self::CommandNotFound { .. }
            | Self::ModelNotFound { .. }
            | Self::ModelHasNoSource { .. }
            | Self::RelationshipNotFound { .. }
            | Self::InternalMissingRelationshipCapabilities { .. }
            | Self::FieldNotFoundInBooleanExpressionType { .. }
            | Self::RelationshipNotFoundInBooleanExpressionType { .. }
            | Self::ObjectBooleanExpressionTypeNotFound { .. }
            | Self::ConditionEvaluationError(_)
            | Self::NestedScalarFilteringNotSupported { .. } => ErrorVisibility::Internal,
            Self::ObjectFieldNotFound { .. }
            | Self::ObjectTypeNotAccessible { .. }
            | Self::CommandNotAccessible { .. }
            | Self::ModelNotAccessible { .. }
            | Self::Other(_) => ErrorVisibility::User,
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum RelationshipError {
    #[error(
        "Mapping for source column {source_column} already exists in the relationship {relationship_name}"
    )]
    MappingExistsInRelationship {
        source_column: FieldName,
        relationship_name: RelationshipName,
    },
    #[error(
        "Mapping for argument {argument_name} already exists in the relationship {relationship_name}"
    )]
    ArgumentMappingExistsInRelationship {
        argument_name: ArgumentName,
        relationship_name: RelationshipName,
    },
    #[error(
        "Missing argument mapping to model {model_name} data connector source for argument {argument_name} used in relationship {relationship_name} on type {source_type}"
    )]
    MissingArgumentMappingInModelRelationship {
        source_type: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
        model_name: Qualified<ModelName>,
        argument_name: ArgumentName,
    },
    #[error(
        "Missing argument mapping to command {command_name} data connector source for argument {argument_name} used in relationship {relationship_name} on type {source_type}"
    )]
    MissingArgumentMappingInCommandRelationship {
        source_type: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
        command_name: Qualified<CommandName>,
        argument_name: ArgumentName,
    },
    #[error(
        "Missing source field '{source_field}' in the mapping in relationship '{relationship_name}'"
    )]
    MissingSourceField {
        relationship_name: RelationshipName,
        source_field: FieldName,
    },
    #[error(
        "Missing NDC column name in relationship {relationship_name} in the mapping between source field {source_field} and target field {target_field}"
    )]
    MissingTargetColumn {
        relationship_name: RelationshipName,
        source_field: FieldName,
        target_field: FieldName,
    },
    #[error(
        "Cannot use relationship '{relationship_name}' in filter predicate. NDC column {source_column} (used by source field '{source_field}') needs to implement an EQUAL comparison operator"
    )]
    SourceColumnMissingEqualComparisonOperator {
        relationship_name: RelationshipName,
        source_field: FieldName,
        source_column: DataConnectorColumnName,
    },
    #[error(
        "Remote predicates are not supported for relationships with an argument mapping target. Relationship {relationship_name} on type {source_type} has source field {source_field} mapped to target argument {target_argument}"
    )]
    RemotePredicatesNotSupportedWithArgumentMappingTarget {
        relationship_name: RelationshipName,
        source_type: Qualified<CustomTypeName>,
        source_field: FieldName,
        target_argument: ArgumentName,
    },

    #[error("Procedure relationships are not supported: {relationship_name}")]
    ProcedureRelationshipsNotSupported { relationship_name: RelationshipName },
    #[error("{0}")]
    RelationshipFieldMappingError(#[from] RelationshipFieldMappingError),
    #[error("{0}")]
    Other(String),
}

impl TraceableError for RelationshipError {
    fn visibility(&self) -> ErrorVisibility {
        ErrorVisibility::Internal
    }
}

#[derive(Debug, thiserror::Error)]
pub enum OrderByError {
    #[error("Aggregate relationship {0} is not supported in order_by")]
    RelationshipAggregateNotSupported(RelationshipName),
    #[error("{0}")]
    RemoteRelationshipNotSupported(String),
    #[error("Nested order by is not supported: {0}")]
    NestedOrderByNotSupported(String),
    #[error("Can't find field mapping for {field_name} in type: {object_type_name}")]
    FieldMappingNotFound {
        field_name: FieldName,
        object_type_name: Qualified<CustomTypeName>,
    },
    #[error("An internal error occurred in order_by: {0}")]
    Internal(String),
}

impl OrderByError {
    pub fn into_plan_error(self) -> PlanError {
        PlanError::OrderBy(self)
    }
}

impl TraceableError for OrderByError {
    fn visibility(&self) -> ErrorVisibility {
        match self {
            Self::RelationshipAggregateNotSupported(_)
            | Self::NestedOrderByNotSupported(_)
            | Self::RemoteRelationshipNotSupported(_) => ErrorVisibility::User,
            Self::Internal(_) | Self::FieldMappingNotFound { .. } => ErrorVisibility::Internal,
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum BooleanExpressionError {
    #[error(
        "Comparison operator {comparison_operator} not defined for data connector {data_connector_name} in scalar boolean expression type {boolean_expression_type_name}"
    )]
    ComparisonOperatorNotFound {
        comparison_operator: DataConnectorOperatorName,
        boolean_expression_type_name: metadata_resolve::BooleanExpressionTypeIdentifier,
        data_connector_name: Qualified<DataConnectorName>,
    },
    #[error(
        "Built-in operators require a boolean expression type. Could not find one for object type {object_type_name}"
    )]
    BuiltInOperatorsRequireABooleanExpressionType {
        object_type_name: Qualified<CustomTypeName>,
    },
}

impl TraceableError for BooleanExpressionError {
    fn visibility(&self) -> ErrorVisibility {
        match self {
            Self::ComparisonOperatorNotFound { .. }
            | Self::BuiltInOperatorsRequireABooleanExpressionType { .. } => ErrorVisibility::User,
        }
    }
}

// Any state that needs to be threaded through the plan
// Nothing here should last more than a single request
pub struct PlanState {
    pub unique_number: UniqueNumber,
    pub condition_cache: ConditionCache,
}

impl PlanState {
    pub fn new() -> Self {
        Self {
            unique_number: UniqueNumber::new(),
            condition_cache: ConditionCache::new(),
        }
    }
}

impl Default for PlanState {
    fn default() -> Self {
        Self::new()
    }
}
