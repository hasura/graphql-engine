use crate::tracer::Tracer;
use opentelemetry::{
    global::{self, BoxedTracer},
    trace::TraceError,
    KeyValue,
};
use opentelemetry_otlp::{WithExportConfig, OTEL_EXPORTER_OTLP_ENDPOINT_DEFAULT};
use opentelemetry_sdk::propagation::TraceContextPropagator;
use opentelemetry_semantic_conventions as semcov;

pub fn start_tracer(
    endpoint: Option<String>,
    service_name: &'static str,
    service_version: String,
) -> Result<Tracer, TraceError> {
    global::set_text_map_propagator(TraceContextPropagator::new());
    let tracer = opentelemetry_otlp::new_pipeline()
        .tracing()
        .with_exporter(
            opentelemetry_otlp::new_exporter()
                .tonic()
                .with_endpoint(endpoint.unwrap_or(OTEL_EXPORTER_OTLP_ENDPOINT_DEFAULT.into())),
        )
        .with_trace_config(opentelemetry::sdk::trace::config().with_resource(
            opentelemetry_sdk::Resource::new(vec![
                KeyValue::new(semcov::resource::SERVICE_NAME, service_name),
                KeyValue::new(semcov::resource::SERVICE_VERSION, service_version),
            ]),
        ))
        .install_batch(opentelemetry::runtime::Tokio)?;
    Ok(Tracer::new(BoxedTracer::new(Box::new(tracer))))
}

pub fn shutdown_tracer() {
    global::shutdown_tracer_provider();
}
