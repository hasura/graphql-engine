use oas3::spec::{ObjectOrReference, ObjectSchema, SchemaType, SchemaTypeSet};
use std::collections::BTreeMap;

pub fn bool_schema() -> ObjectSchema {
    ObjectSchema {
        schema_type: Some(SchemaTypeSet::Single(SchemaType::Boolean)),
        ..ObjectSchema::default()
    }
}

pub fn float_schema() -> ObjectSchema {
    ObjectSchema {
        schema_type: Some(SchemaTypeSet::Single(SchemaType::Number)),
        ..ObjectSchema::default()
    }
}

pub fn int_schema() -> ObjectSchema {
    ObjectSchema {
        schema_type: Some(SchemaTypeSet::Single(SchemaType::Integer)),
        ..ObjectSchema::default()
    }
}

// schema for a string that matches one of a list of values
pub fn enum_schema(enum_values: Vec<String>) -> ObjectSchema {
    ObjectSchema {
        enum_values,
        ..ObjectSchema::default()
    }
}

pub fn string_schema(format: Option<String>) -> ObjectSchema {
    ObjectSchema {
        schema_type: Some(SchemaTypeSet::Single(SchemaType::String)),
        format,
        ..ObjectSchema::default()
    }
}

pub fn json_schema() -> ObjectSchema {
    ObjectSchema {
        schema_type: Some(SchemaTypeSet::Single(SchemaType::Object)),
        ..ObjectSchema::default()
    }
}

pub fn object_schema(
    properties: BTreeMap<String, ObjectOrReference<ObjectSchema>>,
    required: Vec<String>,
) -> ObjectSchema {
    ObjectSchema {
        schema_type: Some(SchemaTypeSet::Single(SchemaType::Object)),
        properties,
        required,
        ..ObjectSchema::default()
    }
}

pub fn array_schema(items: ObjectOrReference<ObjectSchema>) -> ObjectSchema {
    ObjectSchema {
        schema_type: Some(SchemaTypeSet::Single(SchemaType::Array)),
        items: Some(Box::new(items)),
        ..ObjectSchema::default()
    }
}
