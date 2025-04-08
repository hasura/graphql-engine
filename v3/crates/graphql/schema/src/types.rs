use lang_graphql::{
    ast::common::{self as ast, TypeName},
    mk_name,
};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::{
    collections::{BTreeMap, HashMap},
    fmt::Display,
    sync::Arc,
};

use open_dds::{
    aggregates,
    arguments::ArgumentName,
    commands,
    data_connector::{DataConnectorName, DataConnectorOperatorName},
    models,
    types::{self, DataConnectorArgumentName, Deprecated},
};

use metadata_resolve::{
    self, FieldPresetInfo, LogicalOperators, NdcColumnForComparison, OperatorMapping,
    OrderByExpressionIdentifier, Qualified, QualifiedTypeReference,
    deserialize_non_string_key_btreemap, serialize_non_string_key_btreemap,
};

use json_ext::HashMapWithJsonKey;
use strum_macros::Display;

use self::output_type::relationship::{
    FilterRelationshipAnnotation, OrderByRelationshipAnnotation,
};

pub mod inbuilt_type;
pub mod input_type;
pub mod output_type;
pub mod scalar_type;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct GlobalID {
    pub version: u16,
    pub typename: TypeName,
    /// Indexmap containing the name of the field and the
    /// value corresponding to the field that uniquely
    /// identify the object, these fields are declared in the
    /// `globalIdFields` with the object type.
    // We use BTreeMap here to keep the ordering of the
    // `FieldName`s consistent whenever it's serialized when the global ID
    // is constructed from more than one field, if
    // we don't do this, we may get a different Base 64 encoded
    // value every time, the `id` field is queried.
    pub id: BTreeMap<types::FieldName, Value>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct NodeFieldTypeNameMapping {
    pub model_name: Qualified<models::ModelName>,
    pub type_name: Qualified<types::CustomTypeName>,
    // `model_source` is are optional because we allow building schema without specifying a data source
    // In such a case, `global_id_fields_ndc_mapping` will also be empty
    pub model_source: Option<Arc<metadata_resolve::ModelSource>>,
    pub global_id_fields_ndc_mapping: BTreeMap<types::FieldName, NdcColumnForComparison>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct EntityFieldTypeNameMapping {
    pub type_name: Qualified<types::CustomTypeName>,
    pub model_name: Qualified<models::ModelName>,
    // `model_source` is are optional because we allow building schema without specifying a data source
    // In such a case, `global_id_fields_ndc_mapping` will also be empty
    pub model_source: Option<Arc<metadata_resolve::ModelSource>>,
    pub key_fields_ndc_mapping: BTreeMap<types::FieldName, NdcColumnForComparison>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum RootFieldKind {
    SelectOne,
    SelectMany,
    SelectAggregate,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum ObjectFieldKind {
    Scalar,
    ScalarArray,
    Object,
    ObjectArray,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum ModelOrderByDirection {
    Asc,
    Desc,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Display)]
/// Annotations of the GraphQL root field.
pub enum RootFieldAnnotation {
    Introspection,
    RelayNode {
        /// Contains the information required to execute the selection
        /// set of the node root field.
        /// The appropriate `NodeTypeNameMapping` is obtained by decoding
        /// the `id` argument and parsing it as a `GlobalID`. Then the,
        /// `typename` present in the `GlobalID` is used to look up
        /// in the `typename_mappings` which will contain the information
        /// about how the selection set should be executed.
        typename_mappings: HashMap<ast::TypeName, NodeFieldTypeNameMapping>,
    },
    Model {
        kind: RootFieldKind,
        name: Qualified<models::ModelName>,
    },
    ModelSubscription {
        kind: RootFieldKind,
        name: Qualified<models::ModelName>,
        polling_interval_ms: u64,
    },
    FunctionCommand {
        name: Qualified<commands::CommandName>,
        result_type: QualifiedTypeReference,
        result_base_type_kind: TypeKind,
        function_name: Option<commands::FunctionName>,
    },
    ProcedureCommand {
        name: Qualified<commands::CommandName>,
        result_type: QualifiedTypeReference,
        result_base_type_kind: TypeKind,
        procedure_name: Option<commands::ProcedureName>,
    },
    ApolloFederation(ApolloFederationRootFields),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Display)]
pub enum ApolloFederationRootFields {
    Entities {
        typename_mappings: HashMap<ast::TypeName, EntityFieldTypeNameMapping>,
    },
    Service,
}

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq, Display)]
pub enum TypeKind {
    Scalar,
    Object,
}

/// Annotations of the GraphQL output fields/types.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Display)]
pub enum OutputAnnotation {
    RootField(RootFieldAnnotation),
    Field {
        name: types::FieldName,
        field_type: QualifiedTypeReference,
        field_base_type_kind: TypeKind,
        /// The parent type is required to report field usage while analyzing query usage.
        /// Field usage is reported with the name of object type where the field is defined.
        parent_type: Qualified<types::CustomTypeName>,
        argument_types: BTreeMap<ast::Name, QualifiedTypeReference>,
        /// To mark a field as deprecated in the field usage while reporting query usage analytics.
        deprecated: Option<Deprecated>,
    },
    GlobalIDField {
        /// The `global_id_fields` are required to calculate the
        /// value of the `global_id`. Note that, the columns required
        /// to calculate the global id, may or may not be included in
        /// selection set of the query. In either case, we request all the
        /// fields required from the NDC agent prefixed with
        /// `GLOBAL_ID_NDC_PREFIX`, to avoid any conflicts between the
        /// fields requested in the user's selection set and the fields
        /// artificially added by us to calculate the Global ID value.
        global_id_fields: Vec<types::FieldName>,
    },
    RelationshipToModel(output_type::relationship::ModelRelationshipAnnotation),
    RelationshipToModelAggregate(output_type::relationship::ModelAggregateRelationshipAnnotation),
    RelationshipToCommand(output_type::relationship::CommandRelationshipAnnotation),
    RelayNodeInterfaceID {
        typename_mappings: HashMap<ast::TypeName, Vec<types::FieldName>>,
    },
    SDL,
    Aggregate(crate::aggregates::AggregateOutputAnnotation),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Display)]
/// Annotations of the input types/fields related to Models.
pub enum ModelInputAnnotation {
    ModelArgumentsExpression,
    ModelArgument {
        argument_name: ArgumentName,
        argument_type: QualifiedTypeReference,
        argument_kind: metadata_resolve::ArgumentKind,
        ndc_table_argument: Option<DataConnectorArgumentName>,
    },
    ModelOrderByExpression,
    ModelOrderByNestedExpression {
        field_name: types::FieldName,
        parent_type: Qualified<types::CustomTypeName>,
        multiple_input_properties: metadata_resolve::MultipleOrderByInputObjectFields,
    },
    ModelOrderByArgument {
        field_name: types::FieldName,
        /// The parent type is required to report field usage while analyzing query usage.
        /// Field usage is reported with the name of object type where the field is defined.
        parent_type: Qualified<types::CustomTypeName>,
        /// To mark a field as deprecated in the field usage while reporting query usage analytics.
        deprecated: Option<Deprecated>,
    },
    ModelOrderByRelationshipArgument(OrderByRelationshipAnnotation),

    ModelOrderByDirection {
        direction: ModelOrderByDirection,
    },
    ModelLimitArgument,
    ModelOffsetArgument,
    ModelUniqueIdentifierArgument {
        // in future this will be the only thing required
        field_name: types::FieldName,
        // Optional because we allow building schema without specifying a data source
        ndc_column: Option<NdcColumnForComparison>,
    },
    ModelFilterInputArgument,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Display)]
/// Annotations of the input types/fields related to boolean expressions.
pub enum BooleanExpressionAnnotation {
    /// Marks the field that contains the root of the boolean expression. eg. a "where" field
    BooleanExpressionRootField,

    /// Marks a field inside an object boolean expression
    ObjectBooleanExpressionField(ObjectBooleanExpressionField),

    /// Marks a field inside an scalar boolean expression
    ScalarBooleanExpressionField(ScalarBooleanExpressionField),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum ObjectBooleanExpressionField {
    LogicalOperatorField(LogicalOperatorField),
    Field {
        field_name: types::FieldName,
        object_type: Qualified<types::CustomTypeName>,
        object_field_kind: ObjectFieldKind,
        /// To mark a field as deprecated in the field usage while reporting query usage analytics.
        deprecated: Option<Deprecated>,
    },
    RelationshipField(FilterRelationshipAnnotation),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum ScalarBooleanExpressionField {
    LogicalOperatorField(LogicalOperatorField),
    ComparisonOperation {
        #[serde(
            serialize_with = "serialize_non_string_key_btreemap",
            deserialize_with = "deserialize_non_string_key_btreemap"
        )]
        operator_mapping: BTreeMap<Qualified<DataConnectorName>, DataConnectorOperatorName>,
        /// In OpenDD IR we don't need to think about the data connector, so we'll just need this
        /// name:
        operator_name: open_dds::types::OperatorName,
    },
    IsNullOperation,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum LogicalOperatorField {
    AndOp,
    OrOp,
    NotOp,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Display)]
/// Annotations for Relay input arguments/types.
pub enum RelayInputAnnotation {
    NodeFieldIdArgument,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Display)]
/// Annotations for Apollo federation input arguments/types.
pub enum ApolloFederationInputAnnotation {
    AnyScalarInputAnnotation,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Display)]
/// Annotations for GraphQL input arguments/types.
pub enum InputAnnotation {
    Model(ModelInputAnnotation),
    InputObjectField {
        field_name: types::FieldName,
        field_type: QualifiedTypeReference,
        parent_type: Qualified<types::CustomTypeName>,
        /// To mark a field as deprecated in the field usage while reporting query usage analytics.
        deprecated: Option<Deprecated>,
    },
    BooleanExpression(BooleanExpressionAnnotation),
    CommandArgument {
        argument_name: ArgumentName,
        argument_type: QualifiedTypeReference,
        argument_kind: metadata_resolve::ArgumentKind,
        ndc_func_proc_argument: Option<DataConnectorArgumentName>,
    },
    Relay(RelayInputAnnotation),
    ApolloFederationRepresentationsInput(ApolloFederationInputAnnotation),
    FieldArgument {
        argument_name: ArgumentName, // OpenDd argument name
    },
}

/// Contains the different possible entities that can be used to generate
/// the `GDS` type which represents the internal state of the resolved metadata.
///
/// Each entity has two parts to it:
///
/// 1. What schema it is supposed to generate?
///      This is done while building the metadata. The entity is supposed to
///      contain all the data it needs to be able to execute it successfully.
///
/// 2. When a request is executed, how the entity is supposed to be executed using the data it has?
///
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Display)]
pub enum Annotation {
    Output(OutputAnnotation),
    Input(InputAnnotation),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Display)]
pub enum NamespaceAnnotation {
    /// any arguments that we should prefill for a command or type
    Command(
        BTreeMap<
            ArgumentName,
            (
                QualifiedTypeReference,
                metadata_resolve::ValueExpressionOrPredicate,
            ),
        >,
    ),
    /// any filter and arguments for selecting from a model
    Model {
        filter: metadata_resolve::FilterPermission,
        argument_presets: BTreeMap<
            ArgumentName,
            (
                QualifiedTypeReference,
                metadata_resolve::ValueExpressionOrPredicate,
            ),
        >,
        allow_subscriptions: bool,
    },
    /// Field presets for an input field.
    ///
    /// These presets are available in the model permissions context and are injected
    /// while building the input object value during IR generation. Only the normalized
    /// AST is used to analyze query usage, and additional context is not available.
    /// Therefore, the field presets are annotated to track their usage.
    InputFieldPresets {
        presets_fields: BTreeMap<types::FieldName, FieldPresetInfo>,
        type_name: Qualified<types::CustomTypeName>,
    },
    /// The `NodeFieldTypeMappings` contains a Hashmap of typename to the filter permission.
    /// While executing the `node` field, the `id` field is supposed to be decoded and after
    /// decoding, a typename will be obtained. We need to use that typename to look up the
    /// Hashmap to get the appropriate `metadata_resolve::model::FilterPermission`.
    NodeFieldTypeMappings(
        HashMapWithJsonKey<Qualified<types::CustomTypeName>, metadata_resolve::FilterPermission>,
    ),
    /// `EntityTypeMappings` is similar to the `NodeFieldTypeMappings`. While executing the `_entities` field, the
    /// `representations` argument is used, which contains typename. We need to use that typename to look up the hashmap
    /// to get the appropriate `metadata_resolve::model::FilterPermission`.
    EntityTypeMappings(
        HashMapWithJsonKey<Qualified<types::CustomTypeName>, metadata_resolve::FilterPermission>,
    ),
}

#[derive(Serialize, Clone, Debug, Hash, PartialEq, Eq)]
pub enum TypeId {
    QueryRoot {
        graphql_type_name: ast::TypeName,
    },
    MutationRoot {
        graphql_type_name: ast::TypeName,
    },
    SubscriptionRoot {
        graphql_type_name: ast::TypeName,
    },
    OutputType {
        gds_type_name: Qualified<types::CustomTypeName>,
        graphql_type_name: ast::TypeName,
    },
    ScalarType {
        gds_type_name: Qualified<types::CustomTypeName>,
        graphql_type_name: ast::TypeName,
    },
    InputObjectType {
        gds_type_name: Qualified<types::CustomTypeName>,
        graphql_type_name: ast::TypeName,
    },
    InputObjectBooleanExpressionType {
        gds_type_name: Qualified<types::CustomTypeName>,
        graphql_type_name: ast::TypeName,
    },
    InputScalarBooleanExpressionType {
        graphql_type_name: ast::TypeName,
        operators: Vec<(ast::Name, QualifiedTypeReference)>,
        operator_mapping: BTreeMap<Qualified<DataConnectorName>, OperatorMapping>,
        is_null_operator_name: Option<ast::Name>,
        logical_operators: LogicalOperators,
    },
    NodeRoot,
    ModelArgumentsInput {
        model_name: Qualified<models::ModelName>,
        type_name: ast::TypeName,
    },
    OrderByExpression {
        order_by_expression_identifier: Qualified<OrderByExpressionIdentifier>,
        graphql_type_name: ast::TypeName,
    },
    OrderByEnumType {
        graphql_type_name: ast::TypeName,
    },
    ApolloFederationType(PossibleApolloFederationTypes),
    AggregateSelectOutputType {
        aggregate_expression_name: Qualified<aggregates::AggregateExpressionName>,
        graphql_type_name: ast::TypeName,
    },
    ModelFilterInputType {
        model_name: Qualified<models::ModelName>,
        graphql_type_name: ast::TypeName,
    },
}

#[derive(Serialize, Clone, Debug, Hash, PartialEq, Eq)]

pub enum PossibleApolloFederationTypes {
    Entity,
    Any,
    Service,
}

impl Display for TypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.to_type_name().fmt(f)
    }
}

impl TypeId {
    pub fn to_type_name(&self) -> ast::TypeName {
        match self {
            TypeId::QueryRoot { graphql_type_name }
            | TypeId::MutationRoot { graphql_type_name }
            | TypeId::SubscriptionRoot { graphql_type_name }
            | TypeId::OutputType {
                graphql_type_name, ..
            }
            | TypeId::ScalarType {
                graphql_type_name, ..
            }
            | TypeId::InputObjectType {
                graphql_type_name, ..
            }
            | TypeId::InputObjectBooleanExpressionType {
                graphql_type_name, ..
            }
            | TypeId::InputScalarBooleanExpressionType {
                graphql_type_name, ..
            }
            | TypeId::OrderByExpression {
                graphql_type_name, ..
            }
            | TypeId::OrderByEnumType {
                graphql_type_name, ..
            }
            | TypeId::AggregateSelectOutputType {
                graphql_type_name, ..
            }
            | TypeId::ModelFilterInputType {
                graphql_type_name, ..
            } => graphql_type_name.clone(),
            TypeId::NodeRoot => ast::TypeName(mk_name!("Node")),
            TypeId::ModelArgumentsInput { type_name, .. } => type_name.clone(),
            TypeId::ApolloFederationType(PossibleApolloFederationTypes::Entity) => {
                ast::TypeName(mk_name!("_Entity"))
            }
            TypeId::ApolloFederationType(PossibleApolloFederationTypes::Any) => {
                ast::TypeName(mk_name!("_Any"))
            }
            TypeId::ApolloFederationType(PossibleApolloFederationTypes::Service) => {
                ast::TypeName(mk_name!("_Service"))
            }
        }
    }
}
