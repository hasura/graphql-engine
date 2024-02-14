mod http;
mod setup;
mod traceable;
mod tracer;

// Avoid conflicts with `http` crate
pub use crate::http::TraceableHttpResponse;
pub use setup::{shutdown_tracer, start_tracer};
pub use traceable::{ErrorVisibility, Traceable, TraceableError};
pub use tracer::{
    add_event_on_active_span, global_tracer, set_attribute_on_active_span,
    set_status_on_current_span, AttributeVisibility, SpanVisibility,
};
