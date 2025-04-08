use super::helpers::get_object_type;
use super::types::{RelationshipNode, RelationshipTree};
use crate::RequestError;
use crate::catalog::ObjectType;
use jsonapi_library::query::Query;
use metadata_resolve::Qualified;
use open_dds::{relationships::RelationshipType, types::CustomTypeName};
use std::collections::BTreeMap;

// a cheap way to get a unique id for each resource. we should probably get this from the
// underlying data instead.
//
// https://jsonapi.org/format/#document-resource-object-identification
//
// "Within a given API, each resource object’s type and id pair MUST identify a single, unique
// resource."
fn fresh_id(unique_id: &mut i32) -> i32 {
    let id = *unique_id;

    // increment counter
    *unique_id += 1;

    id
}

fn to_resource(
    unique_id: &mut i32,
    rowset: ndc_models::RowSet,
    type_name: &Qualified<CustomTypeName>,
    relationship_tree: &RelationshipTree,
    query: &Query,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectType>,
    collect_relationships: &mut Vec<jsonapi_library::model::Resource>,
) -> Result<Vec<jsonapi_library::model::Resource>, RequestError> {
    let mut resources = vec![];
    if let Some(rows) = rowset.rows {
        for row in rows {
            let resource_id = fresh_id(unique_id);
            let resource = row_to_resource(
                unique_id,
                relationship_tree,
                collect_relationships,
                resource_id,
                type_name,
                query,
                object_types,
                row.into_iter().map(|(k, v)| (k.to_string(), v.0)),
            )?;
            resources.push(resource);
        }
    }
    Ok(resources)
}

fn render_type_name(type_name: &Qualified<CustomTypeName>) -> String {
    format!("{}_{}", type_name.subgraph, type_name.name)
}

fn row_to_resource(
    unique_id: &mut i32,
    relationship_tree: &RelationshipTree,
    collect_relationships: &mut Vec<jsonapi_library::model::Resource>,
    resource_id: i32,
    row_type: &Qualified<CustomTypeName>,
    query: &Query,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectType>,
    row: impl Iterator<Item = (String, serde_json::Value)>,
) -> Result<jsonapi_library::model::Resource, RequestError> {
    let mut attributes = BTreeMap::new();
    let mut relationships = BTreeMap::new();

    for (key, mut value) in row {
        // Check if the key is a relationship
        if let Some(relationship_node) = relationship_tree.relationships.get(key.as_str()) {
            let RelationshipNode {
                nested,
                relationship_type,
                object_type,
                is_command_relationship,
            } = relationship_node;
            let relationship_type_name = render_type_name(object_type);
            let relationship_identifier_data = match value
                .get_mut("rows")
                .and_then(|rows| rows.as_array_mut())
            {
                None => jsonapi_library::model::IdentifierData::None,
                Some(relationship_rows) => match relationship_type {
                    RelationshipType::Object => {
                        if let Some(object_row_value) = relationship_rows.pop() {
                            let relationship_id = fresh_id(unique_id);
                            let resource_identifier = jsonapi_library::model::ResourceIdentifier {
                                _type: relationship_type_name,
                                id: relationship_id.to_string(),
                            };
                            // collect this relationship value
                            collect_relationship_value(
                                unique_id,
                                nested,
                                collect_relationships,
                                relationship_id,
                                object_type,
                                *is_command_relationship,
                                query,
                                object_types,
                                object_row_value,
                            )?;
                            jsonapi_library::model::IdentifierData::Single(resource_identifier)
                        } else {
                            jsonapi_library::model::IdentifierData::None
                        }
                    }
                    RelationshipType::Array => {
                        let mut resource_identifiers = vec![];
                        for object_row_value in relationship_rows.iter_mut() {
                            let relationship_id = fresh_id(unique_id);
                            let resource_identifier = jsonapi_library::model::ResourceIdentifier {
                                _type: relationship_type_name.clone(),
                                id: relationship_id.to_string(),
                            };
                            // collect this relationship value
                            collect_relationship_value(
                                unique_id,
                                nested,
                                collect_relationships,
                                relationship_id,
                                object_type,
                                *is_command_relationship,
                                query,
                                object_types,
                                object_row_value.take(),
                            )?;
                            resource_identifiers.push(resource_identifier);
                        }
                        jsonapi_library::model::IdentifierData::Multiple(resource_identifiers)
                    }
                },
            };
            let relationship = jsonapi_library::model::Relationship {
                data: Some(relationship_identifier_data),
                links: None,
            };
            relationships.insert(key.to_string(), relationship);
        } else {
            let object_type =
                get_object_type(object_types, row_type).map_err(RequestError::ParseError)?;
            let identifier = open_dds::identifier::Identifier::new(key.as_str()).unwrap();
            let field_name = open_dds::types::FieldName::new(identifier);

            if object_type.type_fields.contains_key(&field_name) {
                attributes.insert(key.to_string(), value);
            }
        }
    }

    let rendered_type_name = render_type_name(row_type);
    Ok(jsonapi_library::api::Resource {
        _type: rendered_type_name,
        id: resource_id.to_string(),
        attributes,
        links: None,
        meta: None,
        relationships: if relationships.is_empty() {
            None
        } else {
            Some(relationships)
        },
    })
}

fn collect_relationship_value(
    unique_id: &mut i32,
    relationship_tree: &RelationshipTree,
    collect_relationships: &mut Vec<jsonapi_library::model::Resource>,
    resource_id: i32,
    row_type: &Qualified<CustomTypeName>,
    is_command_relationship: bool,
    query: &Query,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectType>,

    mut value: serde_json::Value,
) -> Result<(), RequestError> {
    if is_command_relationship {
        // If this is a command relationship, we need to extract the value from the 'FUNCTION_IR_VALUE_COLUMN_NAME' key
        // We are ignoring the other keys
        if let Some(v) = value.get_mut(plan_types::FUNCTION_IR_VALUE_COLUMN_NAME) {
            // TODO: Also consider connector's CommandsResponseConfig.
            value = v.take();
        }
    }
    let mut row_object = serde_json::Map::new();
    if let serde_json::Value::Object(object) = value {
        row_object = object;
    }
    let relationship_resource = row_to_resource(
        unique_id,
        relationship_tree,
        collect_relationships,
        resource_id,
        row_type,
        query,
        object_types,
        row_object.into_iter(),
    )?;
    // collect this relationship resource
    collect_relationships.push(relationship_resource);

    Ok(())
}

pub fn process_result(
    rowsets: Vec<ndc_models::RowSet>,
    root_type_name: &Qualified<CustomTypeName>,
    relationship_tree: &RelationshipTree,
    query: &Query,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectType>,
) -> Result<jsonapi_library::api::DocumentData, RequestError> {
    let mut unique_id = 1;

    let mut resources = vec![];
    let mut collect_relationships = vec![];
    if let Some(first_rowset) = rowsets.into_iter().next() {
        resources.extend(to_resource(
            &mut unique_id,
            first_rowset,
            root_type_name,
            relationship_tree,
            query,
            object_types,
            &mut collect_relationships,
        )?);
    }

    let included = if collect_relationships.is_empty() {
        None
    } else {
        Some(collect_relationships)
    };

    Ok(jsonapi_library::api::DocumentData {
        data: Some(jsonapi_library::api::PrimaryData::Multiple(resources)),
        included,
        links: None,
        meta: None,
        jsonapi: None,
    })
}
