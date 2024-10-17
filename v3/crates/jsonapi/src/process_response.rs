use super::QueryResult;
use metadata_resolve::Qualified;
use open_dds::types::CustomTypeName;
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
) -> Vec<jsonapi_library::model::Resource> {
    let mut resources = vec![];

    if let Some(rows) = rowset.rows {
        for row in rows {
            let mut attributes = BTreeMap::new();
            let id = fresh_id(unique_id);
            for (key, ndc_models::RowFieldValue(value)) in row {
                attributes.insert(key.to_string(), value);
            }

            let rendered_type_name = format!("{}_{}", type_name.subgraph, type_name.name);

            resources.push(jsonapi_library::api::Resource {
                _type: rendered_type_name,
                id: id.to_string(),
                attributes,
                links: None,
                meta: None,
                relationships: None,
            });
        }
    }
    resources
}

pub fn process_result(result: QueryResult) -> jsonapi_library::api::DocumentData {
    let mut unique_id = 1;

    let mut resources = vec![];
    if let Some(first_rowset) = result.rowsets.into_iter().next() {
        resources.extend(to_resource(&mut unique_id, first_rowset, &result.type_name));
    }

    jsonapi_library::api::DocumentData {
        data: Some(jsonapi_library::api::PrimaryData::Multiple(resources)),
        included: None,
        links: None,
        meta: None,
        jsonapi: None,
    }
}
