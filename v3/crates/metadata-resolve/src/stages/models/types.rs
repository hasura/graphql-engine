use std::sync::Arc;

use crate::data_connectors::ArgumentPresetValue;
use crate::helpers::argument::ArgumentMappingIssue;
use crate::helpers::types::NdcColumnForComparison;
use crate::stages::order_by_expressions::OrderByExpressionIdentifier;
use crate::stages::{data_connectors, object_types, order_by_expressions};
use crate::types::error::ShouldBeAnError;
use crate::types::subgraph::{
    deserialize_qualified_btreemap, serialize_qualified_btreemap, ArgumentInfo, Qualified,
};
use crate::QualifiedTypeReference;

use indexmap::IndexMap;
use open_dds::commands::CommandName;
use open_dds::data_connector::{CollectionName, DataConnectorName};
use open_dds::{
    aggregates::AggregateExpressionName,
    arguments::ArgumentName,
    data_connector::DataConnectorObjectType,
    models::{ModelGraphQlDefinitionV2, ModelName, OrderableField},
    types::{CustomTypeName, DataConnectorArgumentName, FieldName, GraphQlTypeName},
};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ModelSource {
    pub data_connector: Arc<data_connectors::DataConnectorLink>,
    pub collection: CollectionName,
    pub collection_type: DataConnectorObjectType,
    #[serde(
        serialize_with = "serialize_qualified_btreemap",
        deserialize_with = "deserialize_qualified_btreemap"
    )]
    pub type_mappings: BTreeMap<Qualified<CustomTypeName>, object_types::TypeMapping>,
    pub argument_mappings: BTreeMap<ArgumentName, DataConnectorArgumentName>,
    pub data_connector_link_argument_presets:
        BTreeMap<DataConnectorArgumentName, ArgumentPresetValue>,
    pub source_arguments: BTreeMap<DataConnectorArgumentName, ndc_models::Type>,
}

pub struct ModelsOutput {
    pub models: IndexMap<Qualified<ModelName>, Model>,
    pub global_id_enabled_types: BTreeMap<Qualified<CustomTypeName>, Vec<Qualified<ModelName>>>,
    pub apollo_federation_entity_enabled_types:
        BTreeMap<Qualified<CustomTypeName>, Option<Qualified<open_dds::models::ModelName>>>,
    pub issues: Vec<ModelsIssue>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct Model {
    pub path: jsonpath::JSONPath,
    pub name: Qualified<ModelName>,
    pub data_type: Qualified<CustomTypeName>,
    pub data_type_path: jsonpath::JSONPath, // source path for definition of data_type in Model
    pub type_fields: IndexMap<FieldName, object_types::FieldDefinition>,
    pub global_id_fields: Vec<FieldName>,
    pub arguments: IndexMap<ArgumentName, ArgumentInfo>,
    pub source: Option<Arc<ModelSource>>, // wrapped in Arc because we include these in our `Plan`
    pub global_id_source: Option<NDCFieldSourceMapping>,
    pub apollo_federation_key_source: Option<NDCFieldSourceMapping>,
    pub aggregate_expression: Option<Qualified<AggregateExpressionName>>,
    pub raw: ModelRaw,
}

// TODO: move this outside `Model` as we don't need it in final resolved metadata
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct ModelRaw {
    pub filter_expression_type: Option<Qualified<CustomTypeName>>,
    pub graphql: Option<ModelGraphQlDefinitionV2>,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub description: Option<String>,
    pub order_by: ModelOrderBy,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub enum ModelOrderBy {
    ModelV1 {
        graphql_type_name: Option<GraphQlTypeName>,
        orderable_fields: Vec<OrderableField>,
    },
    ModelV2(Option<open_dds::order_by_expression::OrderByExpressionName>),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct NDCFieldSourceMapping {
    pub ndc_mapping: BTreeMap<FieldName, NdcColumnForComparison>,
}

#[derive(Debug, thiserror::Error)]
pub enum ModelsIssue {
    #[error("An issue occurred while mapping arguments in the model {model_name:} to the collection {collection_name:} in the data connector {data_connector_name:}: {issue:}")]
    FunctionArgumentMappingIssue {
        data_connector_name: Qualified<DataConnectorName>,
        model_name: Qualified<ModelName>,
        collection_name: CollectionName,
        issue: ArgumentMappingIssue,
    },
    #[error("The orderable field '{field_name}' in model '{model_name}' is not a scalar field (type: {field_type}) and therefore cannot be used for ordering. Upgrade to version 2 Models and use OrderByExpressions to order by nested fields")]
    ModelV1OrderableFieldIsNotAScalarField {
        model_name: Qualified<ModelName>,
        field_name: FieldName,
        field_type: QualifiedTypeReference,
    },
    #[error("The orderable field '{field_name}' in model '{model_name}' is an array type (type: {field_type}) and therefore cannot be used for ordering")]
    ModelV1OrderableFieldIsAnArrayType {
        model_name: Qualified<ModelName>,
        field_name: FieldName,
        field_type: QualifiedTypeReference,
    },
    #[error("The order by expression '{order_by_expression_identifier}' used in model '{model_name}' contains a nested field '{nested_field_name}', however the data connector '{data_connector_name}' used in the model does not support ordering by nested fields")]
    OrderByExpressionContainsUnsupportedNestedField {
        order_by_expression_identifier: Qualified<OrderByExpressionIdentifier>,
        model_name: Qualified<ModelName>,
        nested_field_name: FieldName,
        data_connector_name: Qualified<DataConnectorName>,
    },
    #[error("Issue in order by expression '{order_by_expression_identifier}' used by model '{model_name}': {error}")]
    OrderableRelationshipError {
        order_by_expression_identifier: Qualified<OrderByExpressionIdentifier>,
        model_name: Qualified<ModelName>,
        error: order_by_expressions::OrderableRelationshipError,
    },
    #[error("The target model '{target_model_name}' of the orderable relationship '{relationship_name}' in order by expression '{order_by_expression_identifier}' used in model '{model_name}' must have a source")]
    OrderableRelationshipTargetModelMustHaveASource {
        order_by_expression_identifier: Qualified<OrderByExpressionIdentifier>,
        model_name: Qualified<ModelName>,
        relationship_name: open_dds::relationships::RelationshipName,
        target_model_name: Qualified<ModelName>,
    },
    #[error("The target command '{target_command_name}' of the orderable relationship '{relationship_name}' in order by expression '{order_by_expression_identifier}' used in model '{model_name}' must have a source")]
    OrderableRelationshipTargetCommandMustHaveASource {
        order_by_expression_identifier: Qualified<OrderByExpressionIdentifier>,
        model_name: Qualified<ModelName>,
        relationship_name: open_dds::relationships::RelationshipName,
        target_command_name: Qualified<CommandName>,
    },
}

impl ShouldBeAnError for ModelsIssue {
    fn should_be_an_error(&self, flags: &open_dds::flags::OpenDdFlags) -> bool {
        match self {
            ModelsIssue::FunctionArgumentMappingIssue { .. } => false,
            ModelsIssue::ModelV1OrderableFieldIsNotAScalarField { .. }
            | ModelsIssue::ModelV1OrderableFieldIsAnArrayType { .. } => {
                flags.contains(open_dds::flags::Flag::DisallowModelV1OrderingNonScalarFields)
            }
            ModelsIssue::OrderByExpressionContainsUnsupportedNestedField { .. }
            | ModelsIssue::OrderableRelationshipError {
                error:
                    order_by_expressions::OrderableRelationshipError::NestedRelationshipsNotSupported {
                        ..
                    },
                ..
            } => flags.contains(open_dds::flags::Flag::RequireNestedSupportForOrderByExpressions),
            ModelsIssue::OrderableRelationshipError { .. }
            | ModelsIssue::OrderableRelationshipTargetModelMustHaveASource { .. }
            | ModelsIssue::OrderableRelationshipTargetCommandMustHaveASource { .. } => {
                flags.contains(open_dds::flags::Flag::DisallowUnsupportedOrderableRelationships)
            }
        }
    }
}
