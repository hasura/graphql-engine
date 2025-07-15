use std::collections::BTreeSet;

use chrono::NaiveDate;
use open_dds::flags::Flag;
use strum::IntoEnumIterator;

const DATE_FORMAT: &str = "%Y-%m-%d";

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
/// The date to use for determining the default metadata semantics and Hasura behavior.
pub struct CompatibilityDate(pub NaiveDate);

open_dds::impl_OpenDd_default_for!(CompatibilityDate);

impl std::fmt::Display for CompatibilityDate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.format(DATE_FORMAT))
    }
}

impl std::str::FromStr for CompatibilityDate {
    type Err = <NaiveDate as std::str::FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.parse().map(Self)
    }
}

impl serde::Serialize for CompatibilityDate {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(&format!("{self}"))
    }
}

impl<'de> serde::Deserialize<'de> for CompatibilityDate {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let s = <String as serde::Deserialize>::deserialize(deserializer)?;
        NaiveDate::parse_from_str(&s, DATE_FORMAT)
            .map(Self)
            .map_err(serde::de::Error::custom)
    }
}

impl schemars::JsonSchema for CompatibilityDate {
    fn schema_name() -> String {
        "CompatibilityDate".to_string()
    }

    fn json_schema(_gen: &mut schemars::r#gen::SchemaGenerator) -> schemars::schema::Schema {
        let any_date_schema = schemars::schema::SchemaObject {
            metadata: Some(Box::new(schemars::schema::Metadata {
                description: Some("Any date".to_owned()),
                ..Default::default()
            })),
            instance_type: Some(schemars::schema::InstanceType::String.into()),
            format: Some("date".to_string()),
            ..Default::default()
        }
        .into();

        let known_compatibility_dates = Flag::iter()
            .filter_map(get_compatibility_date_for_flag)
            .collect::<BTreeSet<CompatibilityDate>>();

        let known_compatibility_dates_schema = schemars::schema::SchemaObject {
            metadata: Some(Box::new(schemars::schema::Metadata {
                description: Some("Known compatibility dates".to_owned()),
                ..Default::default()
            })),
            instance_type: Some(schemars::schema::InstanceType::String.into()),
            enum_values: Some(
                known_compatibility_dates
                    .into_iter()
                    .map(|date| serde_json::Value::String(date.to_string()))
                    .collect(),
            ),
            ..Default::default()
        }
        .into();

        schemars::schema::SchemaObject {
            metadata: Some(Box::new(schemars::schema::Metadata {
                id: Some(format!(
                    "https://hasura.io/jsonschemas/metadata/{}",
                    Self::schema_name()
                )),
                title: Some(Self::schema_name()),
                description: Some("Any backwards incompatible changes made to Hasura DDN after this date won't impact the metadata".to_owned()),
                ..Default::default()
            })),
            subschemas: Some(Box::new(schemars::schema::SubschemaValidation {
                any_of: Some(vec![
                    known_compatibility_dates_schema,
                    any_date_schema,
                ]),
                ..Default::default()
            })),
            ..Default::default()
        }
        .into()
    }
}

pub const OLDEST_COMPATIBILITY_DATE: CompatibilityDate = new_compatibility_date(2023, 10, 9);

pub const fn new_compatibility_date(year: i32, month: u32, day: u32) -> CompatibilityDate {
    CompatibilityDate(match NaiveDate::from_ymd_opt(year, month, day) {
        // Need to match instead of unwrap because unwrap is still unstable as const
        Some(date) => date,
        None => panic!("Invalid date"),
    })
}

// Adding a flag? Don't forget to add it to the docs: https://hasura.io/docs/3.0/supergraph-modeling/compatibility-config/
#[allow(clippy::match_same_arms)]
pub fn get_compatibility_date_for_flag(flag: Flag) -> Option<CompatibilityDate> {
    match flag {
        // AllowPartialSupergraph is not triggered by compatibility date, instead it is set by the build settings (ie. using a /partial endpoint)
        Flag::AllowPartialSupergraph => None,
        Flag::RequireGraphqlConfig => Some(new_compatibility_date(2024, 6, 30)),
        Flag::BypassRelationComparisonsNdcCapability => Some(new_compatibility_date(2024, 9, 3)),
        Flag::RequireNestedArrayFilteringCapability => Some(new_compatibility_date(2024, 9, 18)),
        Flag::DisallowScalarTypeNamesConflictingWithInbuiltTypes
        | Flag::PropagateBooleanExpressionDeprecationStatus => {
            Some(new_compatibility_date(2024, 9, 26))
        }
        Flag::RequireUniqueCommandGraphqlNames => Some(new_compatibility_date(2024, 10, 7)),
        Flag::JsonSessionVariables => Some(new_compatibility_date(2024, 10, 16)),
        Flag::DisallowArrayFieldComparedWithScalarBooleanType => {
            Some(new_compatibility_date(2024, 10, 31))
        }
        Flag::AllowBooleanExpressionFieldsWithoutGraphql => {
            Some(new_compatibility_date(2024, 11, 13))
        }
        Flag::RequireValidNdcV01Version | Flag::RequireUniqueModelGraphqlNames => {
            Some(new_compatibility_date(2024, 11, 15))
        }
        Flag::DisallowObjectBooleanExpressionType => Some(new_compatibility_date(2024, 11, 18)),
        Flag::LogicalOperatorsInScalarBooleanExpressions => {
            Some(new_compatibility_date(2024, 11, 26))
        }
        Flag::DisallowDuplicateNamesInBooleanExpressions => {
            Some(new_compatibility_date(2024, 12, 5))
        }
        Flag::DisallowMultipleInputObjectFieldsInGraphqlOrderBy => {
            Some(new_compatibility_date(2024, 12, 10))
        }
        Flag::RequireNestedSupportForOrderByExpressions
        | Flag::DisallowModelV1OrderingNonScalarFields
        | Flag::DisallowArrayRelationshipInOrderBy => Some(new_compatibility_date(2024, 12, 18)),
        Flag::DisallowDuplicateOperatorDefinitionsForScalarType
        | Flag::DisallowMultidimensionalArraysInBooleanExpressions
        | Flag::DisallowDuplicateNamesAcrossTypesAndExpressions => {
            Some(new_compatibility_date(2025, 1, 7))
        }
        Flag::DisallowDuplicateAggregateFunctionDefinitionsForScalarType => {
            Some(new_compatibility_date(2025, 1, 25))
        }
        Flag::TypecheckObjectTypeValuesInPresets
        | Flag::DisallowDataConnectorScalarTypesMismatch
        | Flag::CheckObjectTypeFieldsExist
        | Flag::DisallowOrderByFieldsWithFieldArguments => Some(new_compatibility_date(2025, 2, 4)),
        Flag::DisallowUnsupportedOrderableRelationships
        | Flag::DisallowLocalRelationshipsOnDataConnectorsWithoutRelationshipsOrVariables => {
            Some(new_compatibility_date(2025, 2, 8))
        }
        Flag::DisallowUnknownValuesInArguments | Flag::DisallowRecursiveObjectTypes => {
            Some(new_compatibility_date(2025, 2, 20))
        }
        Flag::RequireValidCommandOutputType => Some(new_compatibility_date(2025, 2, 27)),
        Flag::ValidateObjectTypeDataConnectorTypeMappingFieldTypes
        | Flag::ValidateArgumentMappingTypes
        | Flag::DisallowInvalidHeadersInAuthConfig
        | Flag::RequireJwtAudienceValidationIfAudClaimPresent => {
            Some(new_compatibility_date(2025, 3, 11))
        }
        Flag::DisallowProcedureCommandRelationships => Some(new_compatibility_date(2025, 3, 12)),
        Flag::DisallowDuplicateModelPermissionsRoles => Some(new_compatibility_date(2025, 3, 21)),
        Flag::ValidateScalarBooleanExpressionOperators => Some(new_compatibility_date(2025, 3, 26)),
        Flag::ValidateNonNullGraphqlVariables => Some(new_compatibility_date(2025, 4, 3)),
        Flag::DisallowComparableRelationshipTargetWithNoBooleanExpressionType => {
            Some(new_compatibility_date(2025, 4, 24))
        }
        Flag::SendMissingArgumentsToNdcAsNulls => Some(new_compatibility_date(2025, 5, 30)),
        Flag::DisallowLiteralsAsBooleanExpressionArguments => {
            Some(new_compatibility_date(2025, 7, 11))
        }
    }
}
