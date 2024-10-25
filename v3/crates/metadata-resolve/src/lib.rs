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
pub use helpers::type_mappings::{get_field_mapping_of_field_name, RelationshipFieldMappingError};
pub use helpers::types::{
    get_type_representation, mk_name, object_type_exists, unwrap_custom_type_name,
    NdcColumnForComparison, TypeRepresentation,
};
pub use stages::aggregates::{
    AggregatableFieldInfo, AggregateExpression, AggregateExpressionGraphqlConfig, AggregateOperand,
    AggregationFunctionInfo, DataConnectorAggregationFunctionInfo,
};
pub use stages::argument_presets::{
    ArgumentNameAndPath, ArgumentPresets, CommandWithArgumentPresets, ModelWithArgumentPresets,
};
pub use stages::boolean_expressions::{
    BooleanExpressionComparableRelationship, BooleanExpressionError,
    BooleanExpressionGraphqlConfig, BooleanExpressionGraphqlFieldConfig, ComparisonExpressionInfo,
    IncludeLogicalOperators, ObjectComparisonExpressionInfo, ObjectComparisonKind,
    ResolvedObjectBooleanExpressionType,
};
pub use stages::data_connectors::{ArgumentPresetValue, DataConnectorLink, HttpHeadersPreset};
pub use stages::graphql_config::GlobalGraphqlConfig;
pub use stages::model_permissions::{
    FilterPermission, ModelPredicate, ModelTargetSource, ModelWithPermissions, SelectPermission,
    UnaryComparisonOperator,
};
pub use stages::models::{Model, ModelSource, ModelsError};
pub use stages::models_graphql::{
    ModelExpressionType, ModelOrderByExpression, SelectAggregateGraphQlDefinition,
    SelectManyGraphQlDefinition, SelectUniqueGraphQlDefinition, SubscriptionGraphQlDefinition,
    UniqueIdentifierField,
};
pub use stages::object_boolean_expressions::{
    ObjectBooleanExpressionDataConnector, ObjectBooleanExpressionType,
};
pub use stages::object_relationships::{
    relationship_execution_category, CommandRelationshipTarget, ModelAggregateRelationshipTarget,
    ModelRelationshipTarget, ObjectTypeWithRelationships, RelationshipCapabilities,
    RelationshipCommandMapping, RelationshipExecutionCategory, RelationshipField,
    RelationshipModelMapping, RelationshipTarget,
};
pub use stages::object_types::{
    FieldArgumentInfo, FieldMapping, ObjectTypeRepresentation,
    ResolvedObjectApolloFederationConfig, TypeMapping,
};
pub use stages::order_by_expressions::{
    ObjectOrderByExpression, OrderByExpressionGraphqlConfig, OrderByExpressionIdentifier,
    OrderByExpressions, OrderableField, OrderableObjectField, OrderableRelationship,
    OrderableRelationships, OrderableScalarField,
};
pub use stages::plugins::LifecyclePluginConfigs;
pub use stages::scalar_boolean_expressions::ResolvedScalarBooleanExpressionType;
pub use stages::scalar_type_representations::{ScalarTypeRepresentation, ValueRepresentation};
pub use stages::type_permissions::{FieldPresetInfo, TypeInputPermission};
pub use stages::{
    command_permissions::CommandWithPermissions,
    commands::{Command, CommandSource},
    data_connectors,
};
pub use stages::{resolve, Metadata};
pub use types::configuration;
pub use types::error::{Error, WithContext};
pub use types::permission::{ValueExpression, ValueExpressionOrPredicate};
pub use types::subgraph::{
    deserialize_non_string_key_btreemap, deserialize_qualified_btreemap,
    serialize_non_string_key_btreemap, serialize_qualified_btreemap, ArgumentInfo, ArgumentKind,
    Qualified, QualifiedBaseType, QualifiedTypeName, QualifiedTypeReference,
    UnTaggedQualifiedTypeName,
};
pub use types::warning::Warning;
