mod http;
mod request;
mod setup;
mod traceable;
mod tracer;

// Avoid conflicts with `http` crate
pub use crate::http::TraceableHttpResponse;
pub use request::get_trace_headers;
pub use setup::{initialize_tracing, shutdown_tracer, ExportTracesStdout, PropagateBaggage};
pub use traceable::{ErrorVisibility, Successful, Traceable, TraceableError};
pub use tracer::{
    add_event_on_active_span, global_tracer, run_with_baggage, set_attribute_on_active_span,
    set_status_on_current_span, AttributeValue, AttributeVisibility, SpanLink, SpanVisibility,
};

// re-export things from OpenTelemetry to avoid library users importing their own version and
// risking mismatches and multiple globals
pub use opentelemetry::baggage;
pub use opentelemetry::global::get_text_map_propagator;
pub use opentelemetry::propagation::text_map_propagator::TextMapPropagator;
pub use opentelemetry::trace::get_active_span;
pub use opentelemetry::trace::FutureExt;
pub use opentelemetry::trace::Status;
pub use opentelemetry::Context;
pub use opentelemetry::KeyValue;
pub use opentelemetry_http::HeaderInjector;
