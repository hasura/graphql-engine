use ndc_models;

use crate::{collections, functions, procedures, state::AppState, types};

pub fn get_schema() -> ndc_models::SchemaResponse {
    ndc_models::SchemaResponse {
        scalar_types: types::scalar_types(),
        object_types: types::object_types(),
        collections: collections::get_collections(),
        functions: functions::get_functions(),
        procedures: procedures::get_procedures(),
        capabilities: Some(ndc_models::CapabilitySchemaInfo {
            query: Some(ndc_models::QueryCapabilitiesSchemaInfo {
                aggregates: Some(ndc_models::AggregateCapabilitiesSchemaInfo {
                    filter_by: Some(ndc_models::AggregateFilterByCapabilitiesSchemaInfo {
                        count_scalar_type: "Int".to_owned(),
                    }),
                }),
            }),
        }),
    }
}

pub fn get_capabilities(state: &AppState) -> ndc_models::CapabilitiesResponse {
    ndc_models::CapabilitiesResponse {
        version: ndc_models::VERSION.to_owned(),
        capabilities: ndc_models::Capabilities {
            mutation: ndc_models::MutationCapabilities {
                transactional: None,
                explain: None,
            },
            query: ndc_models::QueryCapabilities {
                explain: None,
                aggregates: Some(ndc_models::AggregateCapabilities {
                    filter_by: None,
                    group_by: None,
                }),
                variables: Some(ndc_models::LeafCapability {}),
                nested_fields: ndc_models::NestedFieldCapabilities {
                    aggregates: Some(ndc_models::LeafCapability {}),
                    filter_by: Some(ndc_models::NestedFieldFilterByCapabilities {
                        nested_arrays: None,
                    }),
                    order_by: Some(ndc_models::LeafCapability {}),
                    nested_collections: None,
                },
                exists: ndc_models::ExistsCapabilities {
                    named_scopes: None,
                    unrelated: Some(ndc_models::LeafCapability {}),
                    nested_collections: None,
                    nested_scalar_collections: None,
                },
            },
            relationships: if state.enable_relationship_support {
                Some(ndc_models::RelationshipCapabilities {
                    relation_comparisons: Some(ndc_models::LeafCapability {}),
                    order_by_aggregate: Some(ndc_models::LeafCapability {}),
                    nested: None,
                })
            } else {
                None
            },
        },
    }
}
