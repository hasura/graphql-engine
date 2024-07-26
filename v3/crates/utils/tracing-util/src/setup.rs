use opentelemetry::baggage::BaggageExt;
use opentelemetry::trace::Span; // 'Span'-the-trait, c.f. to 'Span'-the-struct. Used for its
                                // 'set_attribute' method.
use opentelemetry::propagation::composite::TextMapCompositePropagator;
use opentelemetry::{global, trace::TraceError, KeyValue};
pub use opentelemetry_contrib::trace::propagator::trace_context_response::TraceContextResponsePropagator;
use opentelemetry_otlp::{WithExportConfig, OTEL_EXPORTER_OTLP_ENDPOINT_DEFAULT};
use opentelemetry_sdk::propagation::{BaggagePropagator, TraceContextPropagator};
use opentelemetry_sdk::trace::{SpanProcessor, TracerProvider};
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
        Box::new(BaggagePropagator::new()),
        Box::new(TraceContextResponsePropagator::new()),
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
    let tracer_provider = TracerProvider::builder()
        .with_simple_exporter(stdout_exporter)
        .with_batch_exporter(otlp_exporter, opentelemetry_sdk::runtime::Tokio)
        .with_span_processor(BaggageSpanProcessor())
        .with_config(config)
        .build();

    // Set the global tracer provider so everyone gets this setup.
    global::set_tracer_provider(tracer_provider);
    Ok(())
}

/// The sole purpose in life of the 'BaggageSpanProcessor' type is to witness a 'SpanProcessor'
/// instance which outputs each key-value pair stored in the baggage field of the Context to each
/// span. It is only relevant to the tracing setup code.
///
/// "Baggage" is established OpenTelemetry terminology, which signifies contextual data that is
/// passed along across service boundaries.
///
/// A 'BaggagePropagator' is used to inject the recorded baggage into request headers (using the
/// <https://w3c.github.io/baggage/> format) that this process calls, and extract baggage from
/// requests made to this process.
///
/// Baggage may be added to the context using the `Context::current_with_baggage(..)` method.
#[derive(Debug)]
struct BaggageSpanProcessor();

impl SpanProcessor for BaggageSpanProcessor {
    fn on_start(&self, span: &mut opentelemetry_sdk::trace::Span, cx: &opentelemetry::Context) {
        for (key, (value, _)) in cx.baggage() {
            span.set_attribute(KeyValue::new(key.to_string(), value.to_string()));
        }
    }

    fn on_end(&self, _span: opentelemetry_sdk::export::trace::SpanData) {}

    fn force_flush(&self) -> opentelemetry::trace::TraceResult<()> {
        Ok(())
    }

    fn shutdown(&mut self) -> opentelemetry::trace::TraceResult<()> {
        Ok(())
    }
}

pub fn shutdown_tracer() {
    global::shutdown_tracer_provider();
}
