use super::types::{RelationshipNode, RelationshipTree};
use super::QueryResult;
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
    collect_relationships: &mut Vec<jsonapi_library::model::Resource>,
) -> Vec<jsonapi_library::model::Resource> {
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
                row.into_iter().map(|(k, v)| (k.to_string(), v.0)),
            );
            resources.push(resource);
        }
    }
    resources
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
    row: impl Iterator<Item = (String, serde_json::Value)>,
) -> jsonapi_library::model::Resource {
    let mut attributes = BTreeMap::new();
    let mut relationships = BTreeMap::new();
    for (key, mut value) in row {
        // Check if the key is a relationship
        if let Some(relationship_node) = relationship_tree.relationships.get(key.as_str()) {
            let RelationshipNode {
                nested,
                relationship_type,
                object_type,
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
                                object_row_value,
                            );
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
                                object_row_value.take(),
                            );
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
            attributes.insert(key.to_string(), value);
        }
    }

    let rendered_type_name = render_type_name(row_type);
    jsonapi_library::api::Resource {
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
    }
}

fn collect_relationship_value(
    unique_id: &mut i32,
    relationship_tree: &RelationshipTree,
    collect_relationships: &mut Vec<jsonapi_library::model::Resource>,
    resource_id: i32,
    row_type: &Qualified<CustomTypeName>,
    value: serde_json::Value,
) {
    // collect this relationship resource
    let object = match value {
        serde_json::Value::Object(o) => o,
        _ => serde_json::Map::new(),
    };
    let relationship_resource = row_to_resource(
        unique_id,
        relationship_tree,
        collect_relationships,
        resource_id,
        row_type,
        object.into_iter(),
    );
    collect_relationships.push(relationship_resource);
}

pub fn process_result(
    result: QueryResult,
    relationship_tree: &RelationshipTree,
) -> jsonapi_library::api::DocumentData {
    let mut unique_id = 1;

    let mut resources = vec![];
    let mut collect_relationships = vec![];
    if let Some(first_rowset) = result.rowsets.into_iter().next() {
        resources.extend(to_resource(
            &mut unique_id,
            first_rowset,
            &result.type_name,
            relationship_tree,
            &mut collect_relationships,
        ));
    }

    let included = if collect_relationships.is_empty() {
        None
    } else {
        Some(collect_relationships)
    };

    jsonapi_library::api::DocumentData {
        data: Some(jsonapi_library::api::PrimaryData::Multiple(resources)),
        included,
        links: None,
        meta: None,
        jsonapi: None,
    }
}
