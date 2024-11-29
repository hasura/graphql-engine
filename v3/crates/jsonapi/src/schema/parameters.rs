use metadata_resolve::Qualified;
use open_dds::types::CustomTypeName;

use super::shared::{array_schema, enum_schema, int_schema, string_schema};
use crate::catalog::{Model, ObjectType, Type};
use std::collections::BTreeMap;
use std::string::ToString;

pub fn page_offset_parameter() -> oas3::spec::Parameter {
    let schema = oas3::spec::ObjectOrReference::Object(int_schema());
    oas3::spec::Parameter {
        name: "page[offset]".into(),
        allow_empty_value: None,
        allow_reserved: None,
        content: None,
        deprecated: None,
        description: Some("Optional offset for fetched items".into()),
        example: Some("10".into()),
        explode: None,
        examples: BTreeMap::new(),
        extensions: BTreeMap::new(),
        location: oas3::spec::ParameterIn::Query,
        schema: Some(schema),
        style: None,
        required: None,
    }
}

pub fn page_limit_parameter() -> oas3::spec::Parameter {
    let schema = oas3::spec::ObjectOrReference::Object(int_schema());
    oas3::spec::Parameter {
        name: "page[limit]".into(),
        allow_empty_value: None,
        allow_reserved: None,
        content: None,
        deprecated: None,
        description: Some("Optional limit for fetched items".into()),
        example: Some("5".into()),
        explode: None,
        examples: BTreeMap::new(),
        extensions: BTreeMap::new(),
        location: oas3::spec::ParameterIn::Query,
        schema: Some(schema),
        style: None,
        required: None,
    }
}

/// To collect and store nested field types of an object type
struct NestedFieldTypes<'a>(BTreeMap<&'a Qualified<CustomTypeName>, &'a ObjectType>);

impl<'a> NestedFieldTypes<'a> {
    /// Collect all nested field types of an object type
    fn collect(
        object_type: &'a ObjectType,
        object_types: &'a BTreeMap<Qualified<CustomTypeName>, ObjectType>,
    ) -> Self {
        let mut collected = BTreeMap::new();
        Self::collect_from_object_type(object_type, object_types, &mut collected);
        NestedFieldTypes(collected)
    }

    /// Collect nested field types of on object type
    fn collect_from_object_type(
        object_type: &'a ObjectType,
        object_types: &'a BTreeMap<Qualified<CustomTypeName>, ObjectType>,
        collected: &mut BTreeMap<&'a Qualified<CustomTypeName>, &'a ObjectType>,
    ) {
        for field_type in object_type.type_fields.values() {
            Self::collect_from_type(field_type, object_types, collected);
        }
    }

    /// Collect nested object types from a type
    fn collect_from_type(
        ty: &'a Type,
        object_types: &'a BTreeMap<Qualified<CustomTypeName>, ObjectType>,
        collected: &mut BTreeMap<&'a Qualified<CustomTypeName>, &'a ObjectType>,
    ) {
        match ty {
            Type::Scalar(_) | Type::ScalarForDataConnector(_) => {}
            Type::List(inner_type) => {
                Self::collect_from_type(inner_type, object_types, collected);
            }
            Type::Object(type_name) => {
                // Collect only if this type is accessible for the role
                if let Some(object_type) = object_types.get(type_name) {
                    // Ignore if the type has already been collected to prevent infinite recursion caused by cyclic types.
                    if collected.insert(type_name, object_type).is_none() {
                        Self::collect_from_object_type(object_type, object_types, collected);
                    }
                }
            }
        }
    }
}

// Generate "fields[TYPE]" parameter for the given object type and its nested field types.
pub fn fields_parameters(
    type_name: &Qualified<CustomTypeName>,
    object_type: &ObjectType,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectType>,
) -> Vec<oas3::spec::Parameter> {
    // Collect all nested field types of this object type
    let NestedFieldTypes(mut types) = NestedFieldTypes::collect(object_type, object_types);
    // Include this object type in the collection
    types.insert(type_name, object_type);
    // Build "fields[TYPE]" parameter for all collected types
    types
        .into_iter()
        .map(|(type_name, object_type)| build_fields_parameter(type_name, object_type))
        .collect()
}

// Generate "fields[TYPE]" parameter for the given object type
fn build_fields_parameter(
    type_name: &Qualified<CustomTypeName>,
    object_type: &ObjectType,
) -> oas3::spec::Parameter {
    let schema = oas3::spec::ObjectOrReference::Object(oas3::spec::ObjectSchema {
        items: Some(Box::new(oas3::spec::ObjectOrReference::Object(
            enum_schema(
                object_type
                    .type_fields
                    .keys()
                    .map(ToString::to_string)
                    .collect(),
            ),
        ))),
        ..oas3::spec::ObjectSchema::default()
    });

    let mut example = String::new();
    for (i, field) in object_type.type_fields.keys().enumerate() {
        if i > 0 && i < object_type.type_fields.len() {
            example.push(',');
        }
        example.push_str(&field.to_string());
    }

    oas3::spec::Parameter {
        name: format!("fields[{}]", type_name.name),
        allow_empty_value: None,
        allow_reserved: None,
        content: None,
        deprecated: None,
        description: Some(format!("Optional list of fields from {} type to include in response. If no fields are provided, all fields are returned", type_name.name)),
        example: Some(example.into()),
        explode: None,
        examples: BTreeMap::new(),
        extensions: BTreeMap::new(),
        location: oas3::spec::ParameterIn::Query,
        schema: Some(schema),
        style: None,
        required: None,
    }
}

pub fn ordering_parameter(model: &Model, object_type: &ObjectType) -> oas3::spec::Parameter {
    // each field can be `thing` (sort ascending by 'thing') or `-thing` (sort descending by
    // 'thing')
    let mut sort_keys = Vec::new();
    for type_field in object_type.type_fields.keys() {
        sort_keys.push(format!("{type_field}"));
        sort_keys.push(format!("-{type_field}"));
    }

    let schema = oas3::spec::ObjectOrReference::Object(oas3::spec::ObjectSchema {
        items: Some(Box::new(oas3::spec::ObjectOrReference::Object(
            enum_schema(sort_keys.clone()),
        ))),
        ..oas3::spec::ObjectSchema::default()
    });

    // pick out the first few for an example
    let mut example = String::new();
    for (i, field) in sort_keys.iter().enumerate() {
        if i % 3 == 0 && i < 10 {
            if i > 0 && i < sort_keys.len() {
                example.push(',');
            }
            example.push_str(&field.to_string());
        }
    }

    oas3::spec::Parameter {
        name: "sort".into(),
        allow_empty_value: None,
        allow_reserved: None,
        content: None,
        deprecated: None,
        description: Some(format!("Optional list of fields from {} to use in sorting response. 'field' will sort in ascending order, whilst '-field' will sort descending.",model.name.name)),
        example: Some(example.into()),
        explode: None,
        examples: BTreeMap::new(),
        extensions: BTreeMap::new(),
        location: oas3::spec::ParameterIn::Query,
        schema: Some(schema),
        style: None,
        required: None,
    }
}

pub fn include_parameter(model: &Model, object_type: &ObjectType) -> oas3::spec::Parameter {
    let schema = oas3::spec::ObjectOrReference::Object(array_schema(
        oas3::spec::ObjectOrReference::Object(string_schema(None)),
    ));
    let example = object_type
        .type_relationships
        .keys()
        .map(ToString::to_string)
        .collect::<Vec<String>>()
        .join(",");
    let description = format!(
        "Optional list of relationships from {} to include in the response. \
         Use dot-separated names to include nested relationships.",
        model.name.name
    );
    oas3::spec::Parameter {
        name: "include".into(),
        allow_empty_value: None,
        allow_reserved: None,
        content: None,
        deprecated: None,
        description: Some(description),
        example: Some(example.into()),
        explode: None,
        examples: BTreeMap::new(),
        extensions: BTreeMap::new(),
        location: oas3::spec::ParameterIn::Query,
        schema: Some(schema),
        style: None,
        required: None,
    }
}
