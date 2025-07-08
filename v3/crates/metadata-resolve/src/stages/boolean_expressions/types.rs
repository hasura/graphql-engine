use crate::types::error::ShouldBeAnError;
use crate::types::subgraph::{Qualified, QualifiedTypeName, QualifiedTypeReference};
use crate::{
    stages::{
        aggregate_boolean_expressions,
        scalar_boolean_expressions::{self, LogicalOperators, LogicalOperatorsGraphqlConfig},
    },
    types::error::ContextualError,
};
use lang_graphql::ast::common as ast;
use open_dds::models::ModelName;
use open_dds::{
    data_connector::{DataConnectorName, DataConnectorObjectType, DataConnectorOperatorName},
    relationships::RelationshipName,
    types::{CustomTypeName, FieldName, OperatorName},
};
use ref_cast::RefCast;
use serde::{Deserialize, Serialize};
use serde_with::serde_as;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Display;

#[derive(Debug, thiserror::Error, Clone, PartialEq, Eq)]
pub enum BooleanExpressionIssue {
    #[error(
        "The data connector '{data_connector_name}' does not support filtering by nested object arrays. The comparable field '{field_name}' within {boolean_expression_type_name}' is of an object array type: {field_type}"
    )]
    DataConnectorDoesNotSupportNestedObjectArrayFiltering {
        data_connector_name: Qualified<DataConnectorName>,
        boolean_expression_type_name: Qualified<CustomTypeName>,
        field_name: FieldName,
        field_type: QualifiedTypeReference,
    },
    #[error(
        "The data connector '{data_connector_name}' does not support filtering by nested scalar arrays. The comparable field '{field_name}' within '{boolean_expression_type_name}' is of a scalar array type: {field_type}"
    )]
    DataConnectorDoesNotSupportNestedScalarArrayFiltering {
        data_connector_name: Qualified<DataConnectorName>,
        boolean_expression_type_name: Qualified<CustomTypeName>,
        field_name: FieldName,
        field_type: QualifiedTypeReference,
    },
    #[error(
        "the comparable field '{name}' is defined more than once in the boolean expression type '{type_name}'"
    )]
    DuplicateComparableFieldFound {
        type_name: Qualified<CustomTypeName>,
        name: FieldName,
    },
    #[error(
        "the comparable relationship '{name}' is defined more than once in the boolean expression type '{type_name}'"
    )]
    DuplicateComparableRelationshipFound {
        type_name: Qualified<CustomTypeName>,
        name: RelationshipName,
    },
    #[error(
        "the boolean expression '{type_name}' has a GraphQL field name conflict between the '{name}' {name_source_1} and the '{name}' {name_source_2}. One of these will need to be renamed."
    )]
    GraphqlFieldNameConflict {
        type_name: Qualified<CustomTypeName>,
        name: String,
        name_source_1: FieldNameSource,
        name_source_2: FieldNameSource,
    },
    #[error(
        "the type of the comparable field '{field_name}' on the boolean expresssion '{boolean_expression_type_name}' is a multidimensional array type: {field_type}. Multidimensional arrays are not supported in boolean expressions"
    )]
    MultidimensionalArrayComparableFieldNotSupported {
        boolean_expression_type_name: Qualified<CustomTypeName>,
        field_name: FieldName,
        field_type: QualifiedTypeReference,
    },
    #[error(
        "the target model '{target_model_name}' of the relationship '{relationship_name}' does not have a boolean expression type defined"
    )]
    ComparableRelationshipToModelWithoutBooleanExpressionType {
        target_model_name: Qualified<ModelName>,
        relationship_name: RelationshipName,
    },
    #[error("the boolean expression type with name {type_name} is defined more than once")]
    DuplicateBooleanExpressionType {
        type_name: Qualified<CustomTypeName>,
    },
}

impl ContextualError for BooleanExpressionIssue {
    fn create_error_context(&self) -> Option<error_context::Context> {
        None
    }
}

impl ShouldBeAnError for BooleanExpressionIssue {
    fn should_be_an_error(&self, flags: &open_dds::flags::OpenDdFlags) -> bool {
        match self {
            BooleanExpressionIssue::DataConnectorDoesNotSupportNestedObjectArrayFiltering {
                ..
            } => flags.contains(open_dds::flags::Flag::RequireNestedArrayFilteringCapability),
            BooleanExpressionIssue::DataConnectorDoesNotSupportNestedScalarArrayFiltering {
                ..
            } => flags
                .contains(open_dds::flags::Flag::DisallowArrayFieldComparedWithScalarBooleanType),
            BooleanExpressionIssue::DuplicateComparableFieldFound { .. }
            | BooleanExpressionIssue::DuplicateComparableRelationshipFound { .. }
            | BooleanExpressionIssue::GraphqlFieldNameConflict { .. } => {
                flags.contains(open_dds::flags::Flag::DisallowDuplicateNamesInBooleanExpressions)
            }
            BooleanExpressionIssue::MultidimensionalArrayComparableFieldNotSupported { .. } => {
                flags.contains(
                    open_dds::flags::Flag::DisallowMultidimensionalArraysInBooleanExpressions,
                )
            }
            BooleanExpressionIssue::DuplicateBooleanExpressionType { .. } => flags
                .contains(open_dds::flags::Flag::DisallowDuplicateNamesAcrossTypesAndExpressions),
            BooleanExpressionIssue::ComparableRelationshipToModelWithoutBooleanExpressionType {
                ..
            } => flags.contains(
                open_dds::flags::Flag::DisallowComparableRelationshipTargetWithNoBooleanExpressionType,
            ),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone, derive_more::Display)]
pub enum FieldNameSource {
    #[display("comparable field")]
    ComparableField,
    #[display("comparable relationship")]
    ComparableRelationship,
    #[display("logical operator")]
    LogicalOperator,
}

#[serde_as]
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Default)]
pub struct BooleanExpressionTypes {
    #[serde_as(as = "Vec<(_, _)>")]
    pub objects: BTreeMap<Qualified<CustomTypeName>, ResolvedObjectBooleanExpressionType>,
    #[serde_as(as = "Vec<(_, _)>")]
    pub scalars: BTreeMap<
        BooleanExpressionTypeIdentifier,
        scalar_boolean_expressions::ResolvedScalarBooleanExpressionType,
    >,
    #[serde_as(as = "Vec<(_, _)>")]
    pub object_aggregates: BTreeMap<
        Qualified<CustomTypeName>,
        aggregate_boolean_expressions::ObjectAggregateBooleanExpression,
    >,
    #[serde_as(as = "Vec<(_, _)>")]
    pub scalar_aggregates: BTreeMap<
        Qualified<CustomTypeName>,
        aggregate_boolean_expressions::ScalarAggregateBooleanExpression,
    >,
}

impl BooleanExpressionTypes {
    pub fn get_type_names(&self) -> BTreeSet<&Qualified<CustomTypeName>> {
        let mut type_names = BTreeSet::new();
        type_names.extend(self.objects.keys());
        type_names.extend(self.scalars.keys().filter_map(|k| match k {
            BooleanExpressionTypeIdentifier::FromBooleanExpressionType(tn) => Some(tn),
            BooleanExpressionTypeIdentifier::FromDataConnectorScalarRepresentation(_) => None,
        }));
        type_names.extend(self.object_aggregates.keys());
        type_names.extend(self.scalar_aggregates.keys());
        type_names
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BooleanExpressionsOutput {
    pub boolean_expression_types: BooleanExpressionTypes,
    pub issues: Vec<BooleanExpressionIssue>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum ComparableFieldKind {
    Scalar,
    ScalarArray,
    Object,
    ObjectArray,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ResolvedObjectBooleanExpressionType {
    pub name: Qualified<CustomTypeName>,
    pub object_type: Qualified<CustomTypeName>,
    pub graphql: Option<BooleanExpressionGraphqlConfig>,
    pub fields: ResolvedObjectBooleanExpressionTypeFields,
    // do we allow _and, _or, etc for this type?
    pub include_logical_operators: IncludeLogicalOperators,
    // only required for checking legacy `ObjectBooleanExpressionType`
    pub data_connector: Option<ObjectBooleanExpressionDataConnector>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ObjectBooleanExpressionDataConnector {
    pub name: Qualified<DataConnectorName>,
    pub object_type: DataConnectorObjectType,
}

impl ResolvedObjectBooleanExpressionType {
    // we should only return fields if a) `graphql` config is provided
    // or b) flag is passed that allows us to expose it to other frontends
    pub fn get_fields(
        &self,
        flags: &open_dds::flags::OpenDdFlags,
    ) -> Option<&ResolvedObjectBooleanExpressionTypeFields> {
        if self.graphql.is_some()
            || flags.contains(open_dds::flags::Flag::AllowBooleanExpressionFieldsWithoutGraphql)
        {
            Some(&self.fields)
        } else {
            None
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ResolvedObjectBooleanExpressionTypeFields {
    pub object_fields: BTreeMap<FieldName, ObjectComparisonExpressionInfo>,
    pub scalar_fields: BTreeMap<FieldName, ComparisonExpressionInfo>,
    pub relationship_fields: BTreeMap<FieldName, BooleanExpressionComparableRelationship>,
}

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq)]
pub enum IncludeLogicalOperators {
    Yes,
    No,
}

#[derive(Serialize, Deserialize, Clone, Debug, Hash, PartialEq, Eq)]
pub struct OperatorMapping(pub BTreeMap<OperatorName, DataConnectorOperatorName>);

impl OperatorMapping {
    // if we cannot find an operator name in the look up, we assume it's the same as the operator
    // name
    pub fn get<'a>(&'a self, operator_name: &'a OperatorName) -> &'a DataConnectorOperatorName {
        self.0
            .get(operator_name)
            .unwrap_or_else(move || DataConnectorOperatorName::ref_cast(operator_name.inner()))
    }

    pub fn new() -> Self {
        OperatorMapping(BTreeMap::new())
    }
}

impl Default for OperatorMapping {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum ScalarComparisonKind {
    Scalar,
    ScalarArray,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct DataConnectorType {
    pub data_connector_name: Qualified<DataConnectorName>,
    pub type_name: QualifiedTypeName,
}

impl Display for DataConnectorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} from {}", self.type_name, self.data_connector_name)
    }
}

// When converting `ObjectBooleanExpressionType` to `BooleanExpressionType`, we need
// a way to identify auto-generated scalar boolean expression types
#[derive(
    Serialize, Deserialize, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, derive_more::Display,
)]
pub enum BooleanExpressionTypeIdentifier {
    FromBooleanExpressionType(Qualified<CustomTypeName>),
    FromDataConnectorScalarRepresentation(DataConnectorType),
}

#[serde_as]
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ComparisonExpressionInfo {
    pub boolean_expression_type_name: BooleanExpressionTypeIdentifier,
    pub operators: BTreeMap<OperatorName, QualifiedTypeReference>,
    #[serde_as(as = "Vec<(_, _)>")]
    pub operator_mapping: BTreeMap<Qualified<DataConnectorName>, OperatorMapping>,
    pub logical_operators: LogicalOperators,
    pub field_kind: ScalarComparisonKind,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum ObjectComparisonKind {
    Object,
    ObjectArray,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ObjectComparisonExpressionInfo {
    pub boolean_expression_type_name: Qualified<CustomTypeName>,
    pub underlying_object_type_name: Qualified<CustomTypeName>,
    pub field_kind: ObjectComparisonKind,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct BooleanExpressionGraphqlFieldConfig {
    pub where_field_name: ast::Name,
    pub logical_operators: LogicalOperatorsGraphqlConfig,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct BooleanExpressionComparableRelationship {
    /// The name of the relationship to use for comparison
    pub relationship_name: RelationshipName,

    /// The boolean expression type to use for comparison. This is optional for relationships to
    /// models, and defaults to the filterExpressionType of the model
    pub boolean_expression_type: Option<Qualified<CustomTypeName>>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ScalarBooleanExpressionGraphqlConfig {
    pub type_name: ast::TypeName,
    pub is_null_operator_name: Option<ast::Name>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ObjectBooleanExpressionGraphqlConfig {
    pub graphql_type_name: ast::TypeName,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct BooleanExpressionGraphqlConfig {
    pub type_name: ast::TypeName,
    pub object_fields: BTreeMap<FieldName, ObjectBooleanExpressionGraphqlConfig>,
    pub scalar_fields: BTreeMap<FieldName, ScalarBooleanExpressionGraphqlConfig>,
    pub field_config: BooleanExpressionGraphqlFieldConfig,
}

/// Defines strategies for executing relationship predicates.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ComparableRelationshipExecutionStrategy {
    /// Pushes predicate resolution to the NDC (Data Connector).
    /// This is feasible only if the data connector supports the 'relation_comparisons' capability
    /// and is used when both source and target connectors are the same (local relationship).
    NDCPushdown,

    /// Resolves predicates within the Engine itself.
    /// This approach is used when dealing with remote relationships or if the data connector lacks
    /// the 'relation_comparisons' capability. The Engine queries field values from the target model
    /// and constructs the necessary comparison expressions.
    InEngine,
}
