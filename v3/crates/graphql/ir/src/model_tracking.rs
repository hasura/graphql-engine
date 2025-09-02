//! Track the models that were used in query.

use super::{
    IR,
    root_field::{self},
};
use plan::extend_usage_count;
use plan_types::UsagesCounts;

// Get all the OpenDDS Models/commands that were for executing a query. We loop through
// each root field in the query and get the models/commands that were used in each root
// field. We then merge the models/commands used in each root field into a single map.
// That means, if the same model was used in multiple root fields, we will
// sum up the number of times that model was used.
pub fn get_all_usage_counts_in_query(ir: &IR<'_, '_>) -> UsagesCounts {
    let mut all_usage_counts = UsagesCounts::new();
    match ir {
        crate::IR::Query(ir) => {
            for ir_field in ir.values() {
                match ir_field {
                    root_field::QueryRootField::TypeName { .. }
                    | root_field::QueryRootField::SchemaField { .. }
                    | root_field::QueryRootField::TypeField { .. }
                    | root_field::QueryRootField::ApolloFederation(
                        root_field::ApolloFederationRootFields::ServiceField { .. },
                    ) => {}
                    root_field::QueryRootField::ModelSelectOne { ir, .. } => {
                        let usage_counts = ir.usage_counts.clone();
                        extend_usage_count(usage_counts, &mut all_usage_counts);
                    }
                    root_field::QueryRootField::ModelSelectMany { ir, .. } => {
                        let usage_counts = ir.usage_counts.clone();
                        extend_usage_count(usage_counts, &mut all_usage_counts);
                    }
                    root_field::QueryRootField::ModelSelectAggregate { ir, .. } => {
                        let usage_counts = ir.usage_counts.clone();
                        extend_usage_count(usage_counts, &mut all_usage_counts);
                    }
                    root_field::QueryRootField::NodeSelect(ir1) => match ir1 {
                        None => {}
                        Some(ir2) => {
                            let usage_counts = ir2.usage_counts.clone();
                            extend_usage_count(usage_counts, &mut all_usage_counts);
                        }
                    },
                    root_field::QueryRootField::FunctionBasedCommand { ir, .. } => {
                        let usage_counts = ir.command_info.usage_counts.clone();
                        extend_usage_count(usage_counts, &mut all_usage_counts);
                    }
                    root_field::QueryRootField::ApolloFederation(
                        root_field::ApolloFederationRootFields::EntitiesSelect(irs),
                    ) => {
                        for ir in irs {
                            let usage_counts = ir.usage_counts.clone();
                            extend_usage_count(usage_counts, &mut all_usage_counts);
                        }
                    }
                }
            }
        }
        crate::IR::Mutation(ir) => {
            for ir_field in ir.values() {
                match ir_field {
                    root_field::MutationRootField::TypeName { .. } => {}
                    root_field::MutationRootField::ProcedureBasedCommand { ir, .. } => {
                        let usage_counts = ir.command_info.usage_counts.clone();
                        extend_usage_count(usage_counts, &mut all_usage_counts);
                    }
                }
            }
        }
        IR::Subscription(_, ir_field) => match ir_field.as_ref() {
            root_field::SubscriptionRootField::ModelSelectOne { ir, .. } => {
                let usage_counts = ir.usage_counts.clone();
                extend_usage_count(usage_counts, &mut all_usage_counts);
            }
            root_field::SubscriptionRootField::ModelSelectMany { ir, .. } => {
                let usage_counts = ir.usage_counts.clone();
                extend_usage_count(usage_counts, &mut all_usage_counts);
            }
            root_field::SubscriptionRootField::ModelSelectAggregate { ir, .. } => {
                let usage_counts = ir.usage_counts.clone();
                extend_usage_count(usage_counts, &mut all_usage_counts);
            }
        },
    }
    all_usage_counts
}
