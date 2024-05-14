mod http;
mod request;
mod setup;
mod traceable;
mod tracer;

// Avoid conflicts with `http` crate
pub use crate::http::TraceableHttpResponse;
pub use request::get_trace_headers;
pub use setup::{shutdown_tracer, start_tracer};
pub use traceable::{ErrorVisibility, Successful, Traceable, TraceableError};
pub use tracer::{
    add_event_on_active_span, global_tracer, set_attribute_on_active_span,
    set_status_on_current_span, AttributeValue, AttributeVisibility, SpanVisibility,
};

// re-export things from OpenTelemetry to avoid library users importing their own version and
// risking mismatches and multiple globals
pub use opentelemetry::propagation::text_map_propagator::TextMapPropagator;
pub use opentelemetry::trace::get_active_span;
pub use opentelemetry::trace::Status;
pub use opentelemetry::Context;
pub use opentelemetry_contrib::trace::propagator::trace_context_response::TraceContextResponsePropagator;
pub use opentelemetry_http::HeaderInjector;
