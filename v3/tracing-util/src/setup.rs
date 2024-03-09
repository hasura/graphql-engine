use crate::tracer::Tracer;
use opentelemetry::{global, trace::TraceError, KeyValue};
use opentelemetry_otlp::{WithExportConfig, OTEL_EXPORTER_OTLP_ENDPOINT_DEFAULT};
use opentelemetry_sdk::{propagation::TraceContextPropagator, trace::TracerProvider};
use opentelemetry_semantic_conventions as semcov;

pub fn start_tracer(
    endpoint: Option<String>,
    service_name: &'static str,
    service_version: String,
) -> Result<Tracer, TraceError> {
    global::set_text_map_propagator(TraceContextPropagator::new());

    let otlp_exporter = opentelemetry_otlp::SpanExporterBuilder::Tonic(
        opentelemetry_otlp::new_exporter()
            .tonic()
            .with_endpoint(endpoint.unwrap_or(OTEL_EXPORTER_OTLP_ENDPOINT_DEFAULT.into())),
    )
    .build_span_exporter()?;

    let stdout_exporter = opentelemetry_stdout::SpanExporter::default();

    let tracer_provider = TracerProvider::builder()
        .with_batch_exporter(otlp_exporter, opentelemetry_sdk::runtime::Tokio)
        .with_simple_exporter(stdout_exporter) // simple exporter outputs one event per line
        .with_config(opentelemetry_sdk::trace::config().with_resource(
            opentelemetry_sdk::Resource::new(vec![
                KeyValue::new(semcov::resource::SERVICE_NAME, service_name),
                KeyValue::new(semcov::resource::SERVICE_VERSION, service_version),
            ]),
        ))
        .build();

    global::set_tracer_provider(tracer_provider);
    Ok(crate::tracer::global_tracer())
}

pub fn shutdown_tracer() {
    global::shutdown_tracer_provider();
}
