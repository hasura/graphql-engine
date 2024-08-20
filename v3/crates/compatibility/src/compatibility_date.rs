use chrono::NaiveDate;

const DATE_FORMAT: &str = "%Y-%m-%d";

#[derive(Clone, Debug, PartialEq, PartialOrd)]
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

    fn json_schema(_gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        schemars::schema::SchemaObject {
            instance_type: Some(schemars::schema::InstanceType::String.into()),
            format: Some("date".to_string()),
            ..Default::default()
        }
        .into()
    }
}
