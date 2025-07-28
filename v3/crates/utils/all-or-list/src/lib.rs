use serde::{Deserialize, Deserializer, Serialize, Serializer, de::Error as SerdeDeError};

use schemars::JsonSchema;
use serde_json::Value;

#[derive(Serialize, Deserialize, Clone, Debug, JsonSchema, PartialEq, Eq)]
#[serde(untagged)]
#[schemars(title = "AllOrList")]
#[schemars(example = "AllOrList::<String>::example")]
/// A list of items or a wildcard.
pub enum AllOrList<T> {
    All(All),
    List(Vec<T>),
}

impl<T: PartialEq> serde_ext::HasDefaultForSerde for AllOrList<T> {
    fn ser_default() -> Self {
        AllOrList::All(All(()))
    }
}

impl<T> AllOrList<T>
where
    for<'de> T: Deserialize<'de>,
{
    fn example() -> Self {
        serde_json::from_str(r#""*""#).unwrap()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
/// Wildcard: match all items
pub struct All(pub ());

impl Serialize for All {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str("*")
    }
}

impl<'de> Deserialize<'de> for All {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let value = Value::deserialize(deserializer)?;
        match value {
            Value::String(s) if s == "*" => Ok(All(())),
            _ => Err(SerdeDeError::custom("Invalid value for All, expected '*'")),
        }
    }
}

impl schemars::JsonSchema for All {
    fn schema_name() -> String {
        "All".to_string()
    }

    fn json_schema(_gen: &mut schemars::r#gen::SchemaGenerator) -> schemars::schema::Schema {
        schemars::schema::Schema::Object(schemars::schema::SchemaObject {
            metadata: Some(Box::new(schemars::schema::Metadata {
                title: Some(Self::schema_name()),
                description: Some("Wildcard: match all items".to_owned()),
                ..Default::default()
            })),
            instance_type: Some(schemars::schema::SingleOrVec::Single(Box::new(
                schemars::schema::InstanceType::String,
            ))),
            enum_values: Some(vec![serde_json::Value::String("*".to_string())]),
            ..Default::default()
        })
    }
}
