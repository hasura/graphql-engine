use crate::metadata::resolved::permission::ValueExpression;
use crate::metadata::resolved::stages::{
    boolean_expressions, data_connector_type_mappings, data_connectors,
};
use crate::metadata::resolved::subgraph::{
    deserialize_qualified_btreemap, serialize_qualified_btreemap, ArgumentInfo, Qualified,
    QualifiedTypeReference,
};
use crate::metadata::resolved::types::NdcColumnForComparison;

use crate::schema::types::output_type::relationship::PredicateRelationshipAnnotation;
use indexmap::IndexMap;
use lang_graphql::ast::common::{self as ast, Name};
use ndc_models;

use open_dds::types::Deprecated;
use open_dds::{
    arguments::ArgumentName,
    data_connector::DataConnectorName,
    models::{ModelName, OrderableField},
    permissions::Role,
    types::{CustomTypeName, FieldName},
};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap, HashSet};

pub struct ModelsOutput {
    pub models: IndexMap<Qualified<ModelName>, Model>,
    pub graphql_types: HashSet<ast::TypeName>,
    pub global_id_enabled_types: HashMap<Qualified<CustomTypeName>, Vec<Qualified<ModelName>>>,
    pub apollo_federation_entity_enabled_types:
        HashMap<Qualified<CustomTypeName>, Option<Qualified<open_dds::models::ModelName>>>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct UniqueIdentifierField {
    pub field_type: QualifiedTypeReference,
    pub ndc_column: Option<NdcColumnForComparison>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct SelectUniqueGraphQlDefinition {
    pub query_root_field: ast::Name,
    pub unique_identifier: IndexMap<FieldName, UniqueIdentifierField>,
    pub description: Option<String>,
    pub deprecated: Option<Deprecated>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct SelectManyGraphQlDefinition {
    pub query_root_field: ast::Name,
    pub description: Option<String>,
    pub deprecated: Option<Deprecated>,
}

// TODO: add support for aggregates
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct OrderByExpressionInfo {
    pub ndc_column: String,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ModelOrderByExpression {
    pub data_connector_name: Qualified<DataConnectorName>,
    pub order_by_type_name: ast::TypeName,
    pub order_by_fields: HashMap<FieldName, OrderByExpressionInfo>,
    pub order_by_field_name: ast::Name,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ModelGraphqlApiArgumentsConfig {
    pub field_name: Name,
    pub type_name: ast::TypeName,
}
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct LimitFieldGraphqlConfig {
    pub field_name: Name,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct OffsetFieldGraphqlConfig {
    pub field_name: Name,
}
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Default)]
pub struct ModelGraphQlApi {
    pub arguments_input_config: Option<ModelGraphqlApiArgumentsConfig>,
    pub select_uniques: Vec<SelectUniqueGraphQlDefinition>,
    pub select_many: Option<SelectManyGraphQlDefinition>,
    pub order_by_expression: Option<ModelOrderByExpression>,
    pub limit_field: Option<LimitFieldGraphqlConfig>,
    pub offset_field: Option<OffsetFieldGraphqlConfig>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ModelSource {
    pub data_connector: data_connectors::DataConnectorLink,
    pub collection: String,
    #[serde(
        serialize_with = "serialize_qualified_btreemap",
        deserialize_with = "deserialize_qualified_btreemap"
    )]
    pub type_mappings:
        BTreeMap<Qualified<CustomTypeName>, data_connector_type_mappings::TypeMapping>,
    pub argument_mappings: HashMap<ArgumentName, String>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum FilterPermission {
    AllowAll,
    Filter(ModelPredicate),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct SelectPermission {
    pub filter: FilterPermission,
    // pub allow_aggregations: bool,
    pub argument_presets: BTreeMap<ArgumentName, (QualifiedTypeReference, ValueExpression)>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum ModelPredicate {
    UnaryFieldComparison {
        field: FieldName,
        ndc_column: String,
        operator: ndc_models::UnaryComparisonOperator,
    },
    BinaryFieldComparison {
        field: FieldName,
        ndc_column: String,
        operator: String,
        argument_type: QualifiedTypeReference,
        value: ValueExpression,
    },
    Relationship {
        relationship_info: PredicateRelationshipAnnotation,
        predicate: Box<ModelPredicate>,
    },
    And(Vec<ModelPredicate>),
    Or(Vec<ModelPredicate>),
    Not(Box<ModelPredicate>),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct Model {
    pub name: Qualified<ModelName>,
    pub data_type: Qualified<CustomTypeName>,
    pub type_fields: IndexMap<FieldName, data_connector_type_mappings::FieldDefinition>,
    pub global_id_fields: Vec<FieldName>,
    pub arguments: IndexMap<ArgumentName, ArgumentInfo>,
    pub graphql_api: ModelGraphQlApi,
    pub source: Option<ModelSource>,
    pub select_permissions: Option<HashMap<Role, SelectPermission>>,
    pub global_id_source: Option<NDCFieldSourceMapping>,
    pub apollo_federation_key_source: Option<NDCFieldSourceMapping>,
    pub filter_expression_type: Option<boolean_expressions::ObjectBooleanExpressionType>,
    pub orderable_fields: Vec<OrderableField>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct NDCFieldSourceMapping {
    pub ndc_mapping: HashMap<FieldName, NdcColumnForComparison>,
}
