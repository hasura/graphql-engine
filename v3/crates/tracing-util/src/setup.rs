use crate::tracer::Tracer;
use opentelemetry::propagation::composite::TextMapCompositePropagator;
use opentelemetry::{
    global::{self, BoxedTracer},
    trace::TraceError,
    KeyValue,
};
use opentelemetry_otlp::{WithExportConfig, OTEL_EXPORTER_OTLP_ENDPOINT_DEFAULT};
use opentelemetry_sdk::propagation::TraceContextPropagator;
use opentelemetry_semantic_conventions as semcov;

pub fn start_tracer(
    endpoint: Option<&str>,
    service_name: &'static str,
    service_version: &'static str,
) -> Result<Tracer, TraceError> {
    // install global collector configured based on RUST_LOG env var.
    tracing_subscriber::fmt::init();

    global::set_text_map_propagator(TextMapCompositePropagator::new(vec![
        Box::new(TraceContextPropagator::new()),
        Box::new(opentelemetry_zipkin::Propagator::new()),
    ]));
    let tracer = opentelemetry_otlp::new_pipeline()
        .tracing()
        .with_exporter(
            opentelemetry_otlp::new_exporter()
                .tonic()
                .with_endpoint(endpoint.unwrap_or(OTEL_EXPORTER_OTLP_ENDPOINT_DEFAULT)),
        )
        .with_trace_config(opentelemetry_sdk::trace::config().with_resource(
            opentelemetry_sdk::Resource::new(vec![
                KeyValue::new(semcov::resource::SERVICE_NAME, service_name),
                KeyValue::new(semcov::resource::SERVICE_VERSION, service_version),
            ]),
        ))
        .install_batch(opentelemetry_sdk::runtime::Tokio)?;
    Ok(Tracer::new(BoxedTracer::new(Box::new(tracer))))
}

pub fn shutdown_tracer() {
    global::shutdown_tracer_provider();
}
