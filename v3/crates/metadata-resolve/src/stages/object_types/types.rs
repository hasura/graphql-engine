use super::error::ObjectTypesError;
use crate::NdcVersion;
use crate::helpers::type_validation::TypeCompatibilityIssue;
use crate::types::error::ShouldBeAnError;
use crate::types::subgraph::{QualifiedTypeName, QualifiedTypeReference};
use indexmap::IndexMap;
use open_dds::aggregates::{
    DataConnectorAggregationFunctionName, DataConnectorExtractionFunctionName,
};
use open_dds::arguments::ArgumentName;
use open_dds::models::ModelName;
use open_dds::types::{CustomTypeName, DataConnectorArgumentName, Deprecated, FieldName};
use serde::{Deserialize, Serialize};
use serde_with::serde_as;
use std::borrow::Borrow;
use std::collections::BTreeMap;
use std::ops::Deref;

use crate::types::subgraph::Qualified;

use lang_graphql::ast::common as ast;
use open_dds::data_connector::{
    DataConnectorColumnName, DataConnectorName, DataConnectorObjectType, DataConnectorOperatorName,
};

#[serde_as]
/// A mapping from a data connector to their objects, which contain field types.
#[derive(Serialize, Deserialize, Debug, Clone, Eq, PartialEq)]
pub struct DataConnectorTypeMappingsForObject {
    #[serde_as(as = "Vec<(_, _)>")]
    mappings:
        BTreeMap<Qualified<DataConnectorName>, BTreeMap<DataConnectorObjectType, TypeMapping>>,
}

impl Default for DataConnectorTypeMappingsForObject {
    fn default() -> Self {
        Self::new()
    }
}

impl DataConnectorTypeMappingsForObject {
    pub fn new() -> Self {
        Self {
            mappings: BTreeMap::new(),
        }
    }
    pub fn get<TObjectTypeName>(
        &self,
        data_connector_name: &Qualified<DataConnectorName>,
        data_connector_object_type: &TObjectTypeName,
    ) -> Option<&TypeMapping>
    where
        DataConnectorObjectType: Borrow<TObjectTypeName>,
        TObjectTypeName: Ord + ?Sized,
    {
        self.mappings
            .get(data_connector_name)
            .and_then(|data_connector_object_types| {
                data_connector_object_types.get(data_connector_object_type)
            })
    }

    pub fn insert(
        &mut self,
        data_connector_name: &Qualified<DataConnectorName>,
        data_connector_object_type: &DataConnectorObjectType,
        type_mapping: TypeMapping,
    ) -> Result<(), ObjectTypesError> {
        if self
            .mappings
            .entry(data_connector_name.clone())
            .or_default()
            .insert(data_connector_object_type.clone(), type_mapping)
            .is_some()
        {
            return Err(ObjectTypesError::DuplicateDataConnectorObjectTypeMapping {
                data_connector: data_connector_name.clone(),
                data_connector_object_type: data_connector_object_type.to_string(),
            });
        }
        Ok(())
    }

    pub fn data_connector_names(&self) -> impl Iterator<Item = &Qualified<DataConnectorName>> {
        self.mappings.keys()
    }

    pub fn data_connector_mappings(
        &self,
    ) -> impl Iterator<Item = &BTreeMap<DataConnectorObjectType, TypeMapping>> {
        self.mappings.values()
    }

    pub fn object_types_for_data_connector(
        &self,
        data_connector_name: &Qualified<DataConnectorName>,
    ) -> Vec<DataConnectorObjectType> {
        match self.mappings.get(data_connector_name) {
            Some(map) => map.keys().cloned().collect(),
            None => vec![],
        }
    }
}

pub struct ObjectTypesWithTypeMappings(
    pub BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithTypeMappings>,
);

impl Deref for ObjectTypesWithTypeMappings {
    type Target = BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithTypeMappings>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// output of `object_types` step
pub struct ObjectTypesOutput {
    pub global_id_enabled_types: BTreeMap<Qualified<CustomTypeName>, Vec<Qualified<ModelName>>>,
    pub apollo_federation_entity_enabled_types:
        BTreeMap<Qualified<CustomTypeName>, Option<Qualified<open_dds::models::ModelName>>>,
    pub object_types: ObjectTypesWithTypeMappings,
    pub issues: Vec<ObjectTypesIssue>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ObjectTypeRepresentation {
    pub fields: IndexMap<FieldName, FieldDefinition>,
    pub global_id_fields: Vec<FieldName>,
    pub apollo_federation_config: Option<ResolvedObjectApolloFederationConfig>,
    pub graphql_output_type_name: Option<ast::TypeName>,
    pub graphql_input_type_name: Option<ast::TypeName>,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub description: Option<String>,
    // TODO: add graphql_output_type_kind if we support creating interfaces.
}

pub struct ObjectTypeWithTypeMappings {
    pub object_type: ObjectTypeRepresentation,
    pub type_mappings: DataConnectorTypeMappingsForObject,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct FieldDefinition {
    pub field_type: QualifiedTypeReference,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub description: Option<String>,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub deprecated: Option<Deprecated>,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub field_arguments: IndexMap<ArgumentName, FieldArgumentInfo>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct FieldArgumentInfo {
    pub argument_type: QualifiedTypeReference,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub description: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ResolvedObjectApolloFederationConfig {
    pub keys: nonempty::NonEmpty<ResolvedApolloFederationObjectKey>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ResolvedApolloFederationObjectKey {
    pub fields: nonempty::NonEmpty<FieldName>,
}

/// Mapping from a column to its type.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash)]
pub struct FieldMapping {
    pub column: DataConnectorColumnName,
    pub column_type: ndc_models::Type,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub column_type_representation: Option<ndc_models::TypeRepresentation>,
    pub comparison_operators: Option<ComparisonOperators>,
    pub aggregate_functions: Option<AggregateFunctions>,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub extraction_functions: Option<ExtractionFunctions>,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub argument_mappings: BTreeMap<ArgumentName, DataConnectorArgumentName>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash, Default)]
pub struct ComparisonOperators {
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub eq_operator: Option<DataConnectorOperatorName>,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub in_operator: Option<DataConnectorOperatorName>,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub lt_operator: Option<DataConnectorOperatorName>,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub lte_operator: Option<DataConnectorOperatorName>,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub gt_operator: Option<DataConnectorOperatorName>,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub gte_operator: Option<DataConnectorOperatorName>,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub contains_operator: Option<DataConnectorOperatorName>,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub icontains_operator: Option<DataConnectorOperatorName>,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub starts_with_operator: Option<DataConnectorOperatorName>,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub istarts_with_operator: Option<DataConnectorOperatorName>,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub ends_with_operator: Option<DataConnectorOperatorName>,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub iends_with_operator: Option<DataConnectorOperatorName>,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub other_operators: Vec<DataConnectorOperatorName>,
}

impl ComparisonOperators {
    // TODO: this is a very crude backup lookup for operators.
    // We keep it because v0.1 connectors don't have the newer
    // set of comparison operator meanings (lt, lte, gt, gte),
    // so until more connectors are on v0.2, we need a heuristic
    // for finding these operators here.
    pub(crate) fn find_comparison_operator<'a>(
        &'a self,
        operator_str: &str,
        ndc_version: NdcVersion,
    ) -> Option<&'a DataConnectorOperatorName> {
        match ndc_version {
            NdcVersion::V01 => self.other_operators.iter().find(|other_op| {
                other_op.as_str() == operator_str
                    || operator_str
                        .strip_prefix("_")
                        .is_some_and(|op| other_op.as_str() == op)
            }),
            NdcVersion::V02 => None,
        }
    }

    pub fn get_eq_operator(&self, ndc_version: NdcVersion) -> Option<&DataConnectorOperatorName> {
        self.eq_operator
            .as_ref()
            .or_else(|| self.find_comparison_operator("_eq", ndc_version))
    }

    pub fn get_lt_operator(&self, ndc_version: NdcVersion) -> Option<&DataConnectorOperatorName> {
        self.lt_operator
            .as_ref()
            .or_else(|| self.find_comparison_operator("_lt", ndc_version))
    }

    pub fn get_lte_operator(&self, ndc_version: NdcVersion) -> Option<&DataConnectorOperatorName> {
        self.lte_operator
            .as_ref()
            .or_else(|| self.find_comparison_operator("_lte", ndc_version))
    }

    pub fn get_gt_operator(&self, ndc_version: NdcVersion) -> Option<&DataConnectorOperatorName> {
        self.gt_operator
            .as_ref()
            .or_else(|| self.find_comparison_operator("_gt", ndc_version))
    }

    pub fn get_gte_operator(&self, ndc_version: NdcVersion) -> Option<&DataConnectorOperatorName> {
        self.gte_operator
            .as_ref()
            .or_else(|| self.find_comparison_operator("_gte", ndc_version))
    }

    pub fn get_contains_operator(
        &self,
        ndc_version: NdcVersion,
    ) -> Option<&DataConnectorOperatorName> {
        self.contains_operator
            .as_ref()
            .or_else(|| self.find_comparison_operator("_contains", ndc_version))
    }

    pub fn get_icontains_operator(
        &self,
        ndc_version: NdcVersion,
    ) -> Option<&DataConnectorOperatorName> {
        self.icontains_operator
            .as_ref()
            .or_else(|| self.find_comparison_operator("_icontains", ndc_version))
    }

    pub fn get_starts_with_operator(
        &self,
        ndc_version: NdcVersion,
    ) -> Option<&DataConnectorOperatorName> {
        self.starts_with_operator
            .as_ref()
            .or_else(|| self.find_comparison_operator("_starts_with", ndc_version))
    }

    pub fn get_istarts_with_operator(
        &self,
        ndc_version: NdcVersion,
    ) -> Option<&DataConnectorOperatorName> {
        self.istarts_with_operator
            .as_ref()
            .or_else(|| self.find_comparison_operator("_istarts_with", ndc_version))
    }

    pub fn get_ends_with_operator(
        &self,
        ndc_version: NdcVersion,
    ) -> Option<&DataConnectorOperatorName> {
        self.ends_with_operator
            .as_ref()
            .or_else(|| self.find_comparison_operator("_ends_with", ndc_version))
    }

    pub fn get_iends_with_operator(
        &self,
        ndc_version: NdcVersion,
    ) -> Option<&DataConnectorOperatorName> {
        self.iends_with_operator
            .as_ref()
            .or_else(|| self.find_comparison_operator("_iends_with", ndc_version))
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct AggregateFunctions {
    pub sum_function: Option<DataConnectorAggregationFunctionName>,
    pub min_function: Option<DataConnectorAggregationFunctionName>,
    pub max_function: Option<DataConnectorAggregationFunctionName>,
    pub avg_function: Option<DataConnectorAggregationFunctionName>,
    pub other_functions: Vec<DataConnectorAggregationFunctionName>,
}

impl AggregateFunctions {
    // TODO: this is a very crude backup lookup for functions.
    // We keep it because v0.1 connectors don't have the newer
    // set of aggregate function meanings,
    // so until more connectors are on v0.2, we need a heuristic
    // for finding these functions here.
    pub(crate) fn find_function<'a>(
        &'a self,
        operator_str: &str,
        ndc_version: NdcVersion,
    ) -> Option<&'a DataConnectorAggregationFunctionName> {
        match ndc_version {
            NdcVersion::V01 => self
                .other_functions
                .iter()
                .find(|other_op| other_op.as_str() == operator_str),
            NdcVersion::V02 => None,
        }
    }

    pub fn get_sum_function(
        &self,
        ndc_version: NdcVersion,
    ) -> Option<&DataConnectorAggregationFunctionName> {
        self.sum_function
            .as_ref()
            .or_else(|| self.find_function("sum", ndc_version))
    }

    pub fn get_min_function(
        &self,
        ndc_version: NdcVersion,
    ) -> Option<&DataConnectorAggregationFunctionName> {
        self.min_function
            .as_ref()
            .or_else(|| self.find_function("min", ndc_version))
    }

    pub fn get_max_function(
        &self,
        ndc_version: NdcVersion,
    ) -> Option<&DataConnectorAggregationFunctionName> {
        self.max_function
            .as_ref()
            .or_else(|| self.find_function("max", ndc_version))
    }

    pub fn get_avg_function(
        &self,
        ndc_version: NdcVersion,
    ) -> Option<&DataConnectorAggregationFunctionName> {
        self.avg_function
            .as_ref()
            .or_else(|| self.find_function("avg", ndc_version))
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct ExtractionFunctions {
    pub year_function: Option<DataConnectorExtractionFunctionName>,
    pub month_function: Option<DataConnectorExtractionFunctionName>,
    pub day_function: Option<DataConnectorExtractionFunctionName>,
    pub nanosecond_function: Option<DataConnectorExtractionFunctionName>,
    pub microsecond_function: Option<DataConnectorExtractionFunctionName>,
    pub millisecond_function: Option<DataConnectorExtractionFunctionName>,
    pub second_function: Option<DataConnectorExtractionFunctionName>,
    pub minute_function: Option<DataConnectorExtractionFunctionName>,
    pub hour_function: Option<DataConnectorExtractionFunctionName>,
    pub week_function: Option<DataConnectorExtractionFunctionName>,
    pub quarter_function: Option<DataConnectorExtractionFunctionName>,
    pub day_of_week_function: Option<DataConnectorExtractionFunctionName>,
    pub day_of_year_function: Option<DataConnectorExtractionFunctionName>,
    pub other_functions: Vec<DataConnectorExtractionFunctionName>,
}

/// Mapping from an object to their fields, which contain types.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum TypeMapping {
    /// Mapping from an object to their fields, which contain the types of fields.
    Object {
        ndc_object_type_name: DataConnectorObjectType,
        field_mappings: BTreeMap<FieldName, FieldMapping>,
    },
}

#[allow(clippy::enum_variant_names)]
#[derive(Debug, thiserror::Error)]
pub enum ObjectTypesIssue {
    #[error(
        "Multiple {operator_name} operators found for type {scalar_type} in data connector {data_connector_name}"
    )]
    DuplicateOperatorsDefined {
        scalar_type: ndc_models::ScalarTypeName,
        operator_name: String,
        data_connector_name: Qualified<DataConnectorName>,
    },
    #[error(
        "Multiple {function_name} aggregate functions found for type {scalar_type} in data connector {data_connector_name}"
    )]
    DuplicateAggregateFunctionsDefined {
        scalar_type: ndc_models::ScalarTypeName,
        function_name: String,
        data_connector_name: Qualified<DataConnectorName>,
    },
    #[error(
        "Multiple {function_name} extraction functions found for type {scalar_type} in data connector {data_connector_name}"
    )]
    DuplicateExtractionFunctionsDefined {
        scalar_type: ndc_models::ScalarTypeName,
        function_name: String,
        data_connector_name: Qualified<DataConnectorName>,
    },
    #[error(
        "the field {field_name:} in {type_name:} should have the type {expected:} for data connector {data_connector:} but the field has type {provided:}"
    )]
    FieldTypeMismatch {
        field_name: FieldName,
        type_name: Qualified<CustomTypeName>,
        data_connector: Qualified<DataConnectorName>,
        expected: QualifiedTypeName,
        provided: QualifiedTypeName,
    },
    #[error(
        "Field type {field_type} could not be found in field {field_name} for object type {object_type_name}"
    )]
    FieldTypeNotFound {
        field_type: Qualified<CustomTypeName>,
        object_type_name: Qualified<CustomTypeName>,
        field_name: FieldName,
    },
    #[error(
        "Recursive reference detected for object type '{type_name}' through non-null field path: {field_path}"
    )]
    RecursiveObjectType {
        type_name: Qualified<CustomTypeName>,
        field_path: String,
    },
    #[error(
        "The field '{field_name}' in object type '{type_name}' cannot be mapped to data connector '{data_connector}' field '{data_connector_object}.{data_connector_column}' because: {issue}"
    )]
    FieldTypeNdcMappingIssue {
        field_name: FieldName,
        type_name: Qualified<CustomTypeName>,
        data_connector: Qualified<DataConnectorName>,
        data_connector_object: DataConnectorObjectType,
        data_connector_column: DataConnectorColumnName,
        issue: TypeCompatibilityIssue,
    },
}

impl ShouldBeAnError for ObjectTypesIssue {
    fn should_be_an_error(&self, flags: &open_dds::flags::OpenDdFlags) -> bool {
        match self {
            ObjectTypesIssue::DuplicateOperatorsDefined { .. } => flags
                .contains(open_dds::flags::Flag::DisallowDuplicateOperatorDefinitionsForScalarType),
            ObjectTypesIssue::DuplicateAggregateFunctionsDefined { .. } => flags.contains(
                open_dds::flags::Flag::DisallowDuplicateAggregateFunctionDefinitionsForScalarType,
            ),
            ObjectTypesIssue::DuplicateExtractionFunctionsDefined { .. } => true,
            ObjectTypesIssue::FieldTypeMismatch { .. } => {
                flags.contains(open_dds::flags::Flag::DisallowDataConnectorScalarTypesMismatch)
            }
            ObjectTypesIssue::FieldTypeNotFound { .. } => {
                flags.contains(open_dds::flags::Flag::CheckObjectTypeFieldsExist)
            }
            ObjectTypesIssue::RecursiveObjectType { .. } => {
                flags.contains(open_dds::flags::Flag::DisallowRecursiveObjectTypes)
            }
            ObjectTypesIssue::FieldTypeNdcMappingIssue { .. } => flags.contains(
                open_dds::flags::Flag::ValidateObjectTypeDataConnectorTypeMappingFieldTypes,
            ),
        }
    }
}
