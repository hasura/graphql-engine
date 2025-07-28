use core::fmt;
use std::{
    collections::{BTreeMap, BTreeSet},
    str::FromStr,
    sync::LazyLock,
};

use schemars::{JsonSchema, schema};
use serde::{Deserialize, Serialize};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::impl_OpenDd_default_for;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, EnumIter)]
pub enum Flag {
    RequireGraphqlConfig,
    RequireValidNdcV01Version,
    BypassRelationComparisonsNdcCapability,
    RequireNestedArrayFilteringCapability,
    DisallowScalarTypeNamesConflictingWithInbuiltTypes,
    PropagateBooleanExpressionDeprecationStatus,
    RequireUniqueCommandGraphqlNames,
    AllowPartialSupergraph,
    JsonSessionVariables,
    DisallowArrayFieldComparedWithScalarBooleanType,
    AllowBooleanExpressionFieldsWithoutGraphql,
    RequireUniqueModelGraphqlNames,
    DisallowObjectBooleanExpressionType,
    LogicalOperatorsInScalarBooleanExpressions,
    DisallowDuplicateNamesInBooleanExpressions,
    DisallowMultipleInputObjectFieldsInGraphqlOrderBy,
    RequireNestedSupportForOrderByExpressions,
    DisallowModelV1OrderingNonScalarFields,
    DisallowArrayRelationshipInOrderBy,
    DisallowDuplicateOperatorDefinitionsForScalarType,
    DisallowMultidimensionalArraysInBooleanExpressions,
    DisallowDuplicateNamesAcrossTypesAndExpressions,
    DisallowDuplicateAggregateFunctionDefinitionsForScalarType,
    TypecheckObjectTypeValuesInPresets,
    DisallowDataConnectorScalarTypesMismatch,
    CheckObjectTypeFieldsExist,
    DisallowOrderByFieldsWithFieldArguments,
    DisallowUnsupportedOrderableRelationships,
    DisallowLocalRelationshipsOnDataConnectorsWithoutRelationshipsOrVariables,
    DisallowRecursiveObjectTypes,
    DisallowUnknownValuesInArguments,
    RequireValidCommandOutputType,
    ValidateObjectTypeDataConnectorTypeMappingFieldTypes,
    ValidateArgumentMappingTypes,
    DisallowInvalidHeadersInAuthConfig,
    RequireJwtAudienceValidationIfAudClaimPresent,
    DisallowProcedureCommandRelationships,
    DisallowDuplicateModelPermissionsRoles,
    ValidateScalarBooleanExpressionOperators,
    ValidateNonNullGraphqlVariables,
    DisallowComparableRelationshipTargetWithNoBooleanExpressionType,
    SendMissingArgumentsToNdcAsNulls,
    DisallowLiteralsAsBooleanExpressionArguments,
}

impl Flag {
    pub const fn to_str(self) -> &'static str {
        // These labels must be unique, as they'll be used when serializing to JSON
        // A roundtrip unit test will pick it up if you make a boo boo
        match self {
            Flag::RequireGraphqlConfig => "require_graphql_config",
            Flag::RequireValidNdcV01Version => "require_valid_ndc_v01_version",
            Flag::BypassRelationComparisonsNdcCapability => {
                "bypass_relation_comparisons_ndc_capability"
            }
            Flag::RequireNestedArrayFilteringCapability => {
                "require_nested_array_filtering_capability"
            }
            Flag::DisallowScalarTypeNamesConflictingWithInbuiltTypes => {
                "disallow_scalar_type_names_conflicting_with_inbuilt_types"
            }
            Flag::PropagateBooleanExpressionDeprecationStatus => {
                "propagate_boolean_expression_deprecation_status"
            }
            Flag::RequireUniqueCommandGraphqlNames => "require_unique_command_graphql_names",
            Flag::AllowPartialSupergraph => "allow_partial_supergraph",
            Flag::JsonSessionVariables => "json_session_variables",
            Flag::DisallowArrayFieldComparedWithScalarBooleanType => {
                "disallow_array_field_compared_with_scalar_boolean_type"
            }
            Flag::AllowBooleanExpressionFieldsWithoutGraphql => {
                "allow_boolean_expression_fields_without_graphql"
            }
            Flag::RequireUniqueModelGraphqlNames => "require_unique_model_graphql_names",
            Flag::DisallowObjectBooleanExpressionType => "disallow_object_boolean_expression_type",
            Flag::LogicalOperatorsInScalarBooleanExpressions => {
                "logical_operators_in_scalar_boolean_expressions"
            }
            Flag::DisallowDuplicateNamesInBooleanExpressions => {
                "disallow_duplicate_names_in_boolean_expressions"
            }
            Flag::DisallowMultipleInputObjectFieldsInGraphqlOrderBy => {
                "disallow_multiple_input_object_fields_in_graphql_order_by"
            }
            Flag::RequireNestedSupportForOrderByExpressions => {
                "require_nested_support_for_order_by_expressions"
            }
            Flag::DisallowModelV1OrderingNonScalarFields => {
                "disallow_model_v1_ordering_non_scalar_fields"
            }
            Flag::DisallowArrayRelationshipInOrderBy => "disallow_array_relationship_in_order_by",
            Flag::DisallowDuplicateOperatorDefinitionsForScalarType => {
                "disallow_duplicate_operator_definitions_for_scalar_type"
            }
            Flag::DisallowMultidimensionalArraysInBooleanExpressions => {
                "disallow_multidimensional_arrays_in_boolean_expressions"
            }
            Flag::DisallowDuplicateNamesAcrossTypesAndExpressions => {
                "disallow_duplicate_names_across_types_and_expressions"
            }
            Flag::DisallowDuplicateAggregateFunctionDefinitionsForScalarType => {
                "disallow_duplicate_aggregate_function_definitions_for_scalar_type"
            }
            Flag::TypecheckObjectTypeValuesInPresets => "typecheck_object_type_values_in_presets",
            Flag::DisallowDataConnectorScalarTypesMismatch => {
                "disallow_data_connector_scalar_types_mismatch"
            }
            Flag::CheckObjectTypeFieldsExist => "check_object_type_fields_exist",
            Flag::DisallowOrderByFieldsWithFieldArguments => {
                "disallow_order_by_fields_with_field_arguments"
            }
            Flag::DisallowUnsupportedOrderableRelationships => {
                "disallow_unsupported_orderable_relationships"
            }
            Flag::DisallowLocalRelationshipsOnDataConnectorsWithoutRelationshipsOrVariables => {
                "disallow_local_relationships_on_data_connectors_without_relationships_or_variables"
            }
            Flag::DisallowRecursiveObjectTypes => "disallow_recursive_object_types",
            Flag::DisallowUnknownValuesInArguments => "disallow_unknown_values_in_arguments",
            Flag::RequireValidCommandOutputType => "require_valid_command_output_type",
            Flag::ValidateObjectTypeDataConnectorTypeMappingFieldTypes => {
                "validate_object_type_data_connector_type_mapping_field_types"
            }
            Flag::ValidateArgumentMappingTypes => "validate_argument_mapping_types",
            Flag::DisallowInvalidHeadersInAuthConfig => "disallow_invalid_headers_in_auth_config",
            Flag::RequireJwtAudienceValidationIfAudClaimPresent => {
                "require_jwt_audience_validation_if_aud_claim_present"
            }
            Flag::DisallowProcedureCommandRelationships => {
                "disallow_procedure_command_relationships"
            }
            Flag::DisallowDuplicateModelPermissionsRoles => {
                "disallow_duplicate_model_permissions_roles"
            }
            Flag::ValidateScalarBooleanExpressionOperators => {
                "validate_scalar_boolean_expression_operators"
            }
            Flag::ValidateNonNullGraphqlVariables => "validate_non_null_graphql_variables",
            Flag::DisallowComparableRelationshipTargetWithNoBooleanExpressionType => {
                "disallow_comparable_relationship_target_with_no_boolean_expression_type"
            }
            Flag::SendMissingArgumentsToNdcAsNulls => "send_missing_arguments_to_ndc_as_nulls",
            Flag::DisallowLiteralsAsBooleanExpressionArguments => {
                "disallow_literals_as_boolean_expression_arguments"
            }
        }
    }
}

static FLAG_STRING_LOOKUP: LazyLock<BTreeMap<&'static str, Flag>> =
    LazyLock::new(|| Flag::iter().map(|flag| (flag.to_str(), flag)).collect());

static FLAG_STRINGS: LazyLock<Vec<&str>> =
    LazyLock::new(|| Flag::iter().map(Flag::to_str).collect());

impl FromStr for Flag {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        FLAG_STRING_LOOKUP
            .get(s)
            .copied()
            .ok_or_else(|| format!("Unknown flag name: {s}"))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OpenDdFlags(BTreeSet<Flag>);

impl OpenDdFlags {
    pub fn new(flags: BTreeSet<Flag>) -> Self {
        OpenDdFlags(flags)
    }

    pub fn contains(&self, flag: Flag) -> bool {
        self.0.contains(&flag)
    }

    pub fn insert(&mut self, flag: Flag) -> bool {
        self.0.insert(flag)
    }

    pub fn remove(&mut self, flag: Flag) -> bool {
        self.0.remove(&flag)
    }

    pub fn iter(&self) -> std::collections::btree_set::Iter<'_, Flag> {
        self.into_iter()
    }
}

impl Default for OpenDdFlags {
    fn default() -> Self {
        OpenDdFlags::new(BTreeSet::new())
    }
}

impl FromIterator<Flag> for OpenDdFlags {
    fn from_iter<Iter: IntoIterator<Item = Flag>>(iter: Iter) -> Self {
        OpenDdFlags(BTreeSet::from_iter(iter))
    }
}

impl IntoIterator for OpenDdFlags {
    type Item = Flag;
    type IntoIter = <BTreeSet<Flag> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> IntoIterator for &'a OpenDdFlags {
    type Item = &'a Flag;
    type IntoIter = std::collections::btree_set::Iter<'a, Flag>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl From<OpenDdFlags> for BTreeSet<Flag> {
    fn from(value: OpenDdFlags) -> Self {
        value.0
    }
}

impl Serialize for OpenDdFlags {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeMap;
        let mut map = serializer.serialize_map(Some(self.0.len()))?;
        for flag in self {
            map.serialize_entry(flag.to_str(), &true)?;
        }
        map.end()
    }
}

impl<'de> Deserialize<'de> for OpenDdFlags {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct FlagSetVisitor;

        impl<'de> serde::de::Visitor<'de> for FlagSetVisitor {
            type Value = OpenDdFlags;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a map with flag names as keys and booleans as values")
            }

            fn visit_map<V>(self, mut map: V) -> Result<Self::Value, V::Error>
            where
                V: serde::de::MapAccess<'de>,
            {
                let mut set = BTreeSet::new();
                while let Some((flag_name, enabled)) = map.next_entry::<String, bool>()? {
                    let flag = Flag::from_str(flag_name.as_str()).map_err(|_| {
                        serde::de::Error::unknown_field(flag_name.as_str(), FLAG_STRINGS.as_slice())
                    })?;
                    if enabled && !set.insert(flag) {
                        return Err(serde::de::Error::duplicate_field(flag.to_str()));
                    }
                }
                Ok(OpenDdFlags::new(set))
            }
        }

        deserializer.deserialize_map(FlagSetVisitor)
    }
}

impl JsonSchema for OpenDdFlags {
    fn schema_name() -> String {
        "OpenDdFlags".to_owned()
    }

    fn json_schema(_gen: &mut schemars::r#gen::SchemaGenerator) -> schema::Schema {
        let mut properties = schemars::Map::<String, schema::Schema>::new();

        for flag in Flag::iter() {
            let property_schema = schema::Schema::Object(schema::SchemaObject {
                metadata: Some(Box::new(schema::Metadata {
                    default: Some(serde_json::Value::Bool(false)),
                    ..Default::default()
                })),
                instance_type: Some(schema::InstanceType::Boolean.into()),
                ..Default::default()
            });
            properties.insert(flag.to_str().to_owned(), property_schema);
        }

        schema::Schema::Object(schema::SchemaObject {
            metadata: Some(Box::new(schema::Metadata {
                id: Some(format!(
                    "https://hasura.io/jsonschemas/metadata/{}",
                    Self::schema_name()
                )),
                title: Some(Self::schema_name()),
                description: Some("Flags to configure the OpenDD metadata build.".to_owned()),
                ..Default::default()
            })),
            instance_type: Some(schema::InstanceType::Object.into()),
            object: Some(Box::new(schema::ObjectValidation {
                properties,
                additional_properties: Some(Box::new(false.into())),
                ..Default::default()
            })),
            ..Default::default()
        })
    }
}

impl_OpenDd_default_for!(OpenDdFlags);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn to_from_str_roundtrip() {
        for flag in Flag::iter() {
            let str_val = flag.to_str();
            assert_eq!(Flag::from_str(str_val).unwrap(), flag);
        }
    }

    #[test]
    pub fn to_from_json_roundtrip() {
        let all_flags = Flag::iter().collect::<OpenDdFlags>();
        let json = serde_json::to_value(all_flags.clone()).unwrap();
        let deserialized = OpenDdFlags::deserialize(json).unwrap();
        assert_eq!(deserialized, all_flags);
    }

    #[test]
    pub fn empty_json_object_deserializes() {
        let deserialized = serde_json::from_str::<OpenDdFlags>("{}").unwrap();
        assert_eq!(deserialized, OpenDdFlags::default());
    }

    #[test]
    pub fn all_false_json_object_deserializes_to_empty_flags() {
        let flags = serde_json::Value::Object(
            Flag::iter()
                .map(|flag| (flag.to_str().to_owned(), serde_json::Value::Bool(false)))
                .collect(),
        );
        let deserialized = OpenDdFlags::deserialize(flags).unwrap();
        assert_eq!(deserialized, OpenDdFlags::default());
    }

    #[test]
    pub fn json_only_serializes_set_flags() {
        let flags = OpenDdFlags::from_iter([
            Flag::RequireGraphqlConfig,
            Flag::DisallowArrayFieldComparedWithScalarBooleanType,
        ]);
        let json = serde_json::to_value(flags).unwrap();
        let expected = serde_json::json!({
            "require_graphql_config": true,
            "disallow_array_field_compared_with_scalar_boolean_type": true
        });
        assert_eq!(json, expected);
    }
}
