mod error;
mod execute;
pub mod ndc;

// we explicitly export things used by other crates
pub use error::{FieldError, FieldInternalError, NDCUnexpectedError};
pub use execute::{
    execute_remote_predicates, make_ndc_mutation_request, make_ndc_query_request,
    replace_predicates_in_query_execution_plan, resolve_ndc_mutation_execution,
    resolve_ndc_query_execution, resolve_ndc_subscription_execution,
};
pub use ndc::fetch_from_data_connector;
