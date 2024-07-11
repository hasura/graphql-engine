use opentelemetry::propagation::composite::TextMapCompositePropagator;
use opentelemetry::{global, trace::TraceError, KeyValue};
use opentelemetry_otlp::{WithExportConfig, OTEL_EXPORTER_OTLP_ENDPOINT_DEFAULT};
use opentelemetry_sdk::propagation::TraceContextPropagator;
use opentelemetry_sdk::trace::TracerProvider as SDKTracerProvider;
use opentelemetry_semantic_conventions as semcov;

pub fn initialize_tracing(
    endpoint: Option<&str>,
    service_name: &'static str,
    service_version: Option<&'static str>,
) -> Result<(), TraceError> {
    // install global collector configured based on RUST_LOG env var.
    tracing_subscriber::fmt::init();

    global::set_text_map_propagator(TextMapCompositePropagator::new(vec![
        Box::new(TraceContextPropagator::new()),
        Box::new(opentelemetry_zipkin::Propagator::new()),
    ]));

    let mut resource_entries = vec![KeyValue::new(semcov::resource::SERVICE_NAME, service_name)];
    if let Some(service_version) = service_version {
        resource_entries.push(KeyValue::new(
            semcov::resource::SERVICE_VERSION,
            service_version,
        ));
    }
    let config = opentelemetry_sdk::trace::config()
        .with_resource(opentelemetry_sdk::Resource::new(resource_entries));

    let otlp_exporter = opentelemetry_otlp::SpanExporterBuilder::Tonic(
        opentelemetry_otlp::new_exporter()
            .tonic()
            .with_endpoint(endpoint.unwrap_or(OTEL_EXPORTER_OTLP_ENDPOINT_DEFAULT)),
    )
    .build_span_exporter()?;

    let stdout_exporter = opentelemetry_stdout::SpanExporter::default();
    let tracer_provider = SDKTracerProvider::builder()
        .with_simple_exporter(stdout_exporter)
        .with_batch_exporter(otlp_exporter, opentelemetry_sdk::runtime::Tokio)
        .with_config(config)
        .build();

    // Set the global tracer provider so everyone gets this setup.
    global::set_tracer_provider(tracer_provider);
    Ok(())
}

pub fn shutdown_tracer() {
    global::shutdown_tracer_provider();
}
