use ndc_models;

use crate::{collections, functions, procedures, types};

pub fn get_schema() -> ndc_models::SchemaResponse {
    ndc_models::SchemaResponse {
        scalar_types: types::scalar_types(),
        object_types: types::object_types(),
        collections: collections::get_collections(),
        functions: functions::get_functions(),
        procedures: procedures::get_procedures(),
    }
}

pub fn get_capabilities() -> ndc_models::CapabilitiesResponse {
    ndc_models::CapabilitiesResponse {
        version: "0.1.2".into(),
        capabilities: ndc_models::Capabilities {
            mutation: ndc_models::MutationCapabilities {
                transactional: None,
                explain: None,
            },
            query: ndc_models::QueryCapabilities {
                explain: None,
                aggregates: Some(ndc_models::LeafCapability {}),
                variables: Some(ndc_models::LeafCapability {}),
            },
            relationships: Some(ndc_models::RelationshipCapabilities {
                relation_comparisons: Some(ndc_models::LeafCapability {}),
                order_by_aggregate: Some(ndc_models::LeafCapability {}),
            }),
        },
    }
}
