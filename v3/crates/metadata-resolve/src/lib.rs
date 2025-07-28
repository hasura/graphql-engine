//! Resolve is the process of taking the input metadata, validating it,
//! and building structures that contain all of the relevant information
//! necessary for the engine to function.
//!
//! `stages::resolve` is the function responsible for resolving the metadata,src/lie
//! and it returns a resolved and validated `stages::Metadata` object.

// No modules outside this should know about its internal structure.

mod helpers;
mod ndc_migration;
mod stages;
mod types;

pub use helpers::http;
pub use helpers::ndc_validation::NDCValidationError;
pub use helpers::to_fancy_errors;
pub use helpers::types::{
    NdcColumnForComparison, TypeRepresentation, get_type_representation, mk_name,
    object_type_exists, unwrap_custom_type_name,
};
pub use stages::aggregates::{
    AggregatableFieldInfo, AggregateExpression, AggregateExpressionGraphqlConfig, AggregateOperand,
    AggregationFunctionInfo, DataConnectorAggregationFunctionInfo,
};
pub use stages::arguments::ArgumentInfo;
pub use stages::boolean_expressions::{
    BooleanExpressionComparableRelationship, BooleanExpressionError,
    BooleanExpressionGraphqlConfig, BooleanExpressionGraphqlFieldConfig,
    BooleanExpressionTypeIdentifier, ComparableRelationshipExecutionStrategy,
    ComparisonExpressionInfo, IncludeLogicalOperators, ObjectBooleanExpressionGraphqlConfig,
    ObjectComparisonExpressionInfo, ObjectComparisonKind, OperatorMapping,
    ResolvedObjectBooleanExpressionType, ScalarBooleanExpressionGraphqlConfig,
    ScalarComparisonKind, get_comparable_relationship_execution_strategy,
};
pub use stages::data_connectors::{
    ArgumentPresetValue, DataConnectorLink, DataConnectorRelationalQueryCapabilities,
    HttpHeadersPreset, NdcVersion,
};
pub use stages::graphql_config::{GlobalGraphqlConfig, MultipleOrderByInputObjectFields};
pub use stages::model_permissions::{
    FilterPermission, ModelAuthorizationRule, ModelPredicate, ModelTargetSource,
    ModelWithPermissions, PredicateRelationshipInfo, RelationalDeletePermission,
    RelationalInsertPermission, RelationalOperation, RelationalUpdatePermission, SelectPermission,
    UnaryComparisonOperator,
};
pub use stages::models::{ModelSource, ModelsError};
pub use stages::models_graphql::{
    Model, ModelGraphqlError, ModelOrderByExpression, SelectAggregateGraphQlDefinition,
    SelectManyGraphQlDefinition, SelectUniqueGraphQlDefinition, SubscriptionGraphQlDefinition,
    UniqueIdentifierField,
};
pub use stages::object_relationships::{
    AggregateRelationship, CommandRelationshipTarget, FieldNestedness, ModelRelationshipTarget,
    ObjectTypeWithRelationships, RelationshipCapabilities, RelationshipCommandMapping,
    RelationshipExecutionCategory, RelationshipField, RelationshipModelMapping,
    RelationshipModelMappingFieldTarget, RelationshipModelMappingTarget, RelationshipTarget,
    field_selection_relationship_execution_category,
};
pub use stages::object_types::{
    AggregateFunctions, ComparisonOperators, ExtractionFunctions, FieldArgumentInfo,
    FieldDefinition, FieldMapping, ObjectTypeRepresentation, ResolvedObjectApolloFederationConfig,
    TypeMapping,
};
pub use stages::order_by_expressions::{
    ObjectOrderByExpression, OrderByExpressionGraphqlConfig, OrderByExpressionIdentifier,
    OrderByExpressions, OrderableField, OrderableFieldNestedness, OrderableObjectField,
    OrderableRelationship, OrderableRelationshipError, OrderableScalarField,
    validate_orderable_relationship,
};
pub use stages::plugins::{
    LifecyclePluginConfigs, ResolvedLifecyclePreNdcRequestPluginHook,
    ResolvedLifecyclePreNdcResponsePluginHook,
    types::{
        ResolvedLifecyclePreResponseAsyncPluginHook, ResolvedLifecyclePreResponsePluginHooks,
        ResolvedLifecyclePreResponseSyncPluginHook,
    },
};
pub use stages::scalar_boolean_expressions::{
    LogicalOperators, LogicalOperatorsGraphqlConfig, ResolvedScalarBooleanExpressionType,
};
pub use stages::scalar_type_representations::ScalarTypeRepresentation;
pub use stages::type_permissions::{
    FieldAuthorizationRule, FieldPresetInfo, TypeInputAuthorizationRule, TypeInputPermission,
};
pub use stages::{Metadata, resolve};
pub use stages::{
    command_permissions::{AllowOrDeny, Command, CommandAuthorizationRule, CommandWithPermissions},
    commands::CommandSource,
    data_connectors,
};
pub use types::condition::{BinaryOperation, Condition, ConditionHash, Conditions, UnaryOperation};
pub use types::configuration;
pub use types::error::{Error, WithContext};
pub use types::flags::{self, RuntimeFlags};
pub use types::permission::{ValueExpression, ValueExpressionOrPredicate};
pub use types::subgraph::{
    ArgumentKind, Qualified, QualifiedBaseType, QualifiedTypeName, QualifiedTypeReference,
    UnTaggedQualifiedTypeName, deserialize_non_string_key_btreemap, deserialize_qualified_btreemap,
    serialize_non_string_key_btreemap, serialize_qualified_btreemap,
};
pub use types::warning::Warning;
