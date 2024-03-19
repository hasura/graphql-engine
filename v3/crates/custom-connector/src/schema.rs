use ndc_client::models;

use crate::{collections, functions, procedures, types};

pub fn get_schema() -> models::SchemaResponse {
    models::SchemaResponse {
        scalar_types: types::scalar_types(),
        object_types: types::object_types(),
        collections: collections::get_collections(),
        functions: functions::get_functions(),
        procedures: procedures::get_procedures(),
    }
}

pub fn get_capabilities() -> models::CapabilitiesResponse {
    models::CapabilitiesResponse {
        version: "0.1.0".into(),
        capabilities: models::Capabilities {
            mutation: models::MutationCapabilities {
                transactional: None,
                explain: None,
            },
            query: models::QueryCapabilities {
                explain: None,
                aggregates: Some(models::LeafCapability {}),
                variables: Some(models::LeafCapability {}),
            },
            relationships: Some(models::RelationshipCapabilities {
                relation_comparisons: Some(models::LeafCapability {}),
                order_by_aggregate: Some(models::LeafCapability {}),
            }),
        },
    }
}
