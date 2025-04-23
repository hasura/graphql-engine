use opentelemetry::baggage::BaggageExt;
use opentelemetry::propagation::TextMapPropagator;
use opentelemetry::trace::Span; // 'Span'-the-trait, c.f. to 'Span'-the-struct. Used for its
// 'set_attribute' method.
use opentelemetry::propagation::composite::TextMapCompositePropagator;
use opentelemetry::{KeyValue, global, trace::TraceError};
pub use opentelemetry_contrib::trace::propagator::trace_context_response::TraceContextResponsePropagator;
use opentelemetry_otlp::{OTEL_EXPORTER_OTLP_ENDPOINT_DEFAULT, WithExportConfig};
use opentelemetry_sdk::propagation::{BaggagePropagator, TraceContextPropagator};
use opentelemetry_sdk::trace::{SpanProcessor, TracerProvider};
use opentelemetry_semantic_conventions as semcov;

/// A configuration type to enable/disable baggage propagation
#[derive(Debug, Copy, Clone)]
pub enum PropagateBaggage {
    Enable,
    Disable,
}

/// A configuration type to enable/disable exporting traces to stdout
#[derive(Debug, Copy, Clone)]
pub enum ExportTracesStdout {
    Enable,
    Disable,
}

/// Initialize the tracing setup.
///
/// This includes setting the global tracer and propagators:
///
///  * Exporting to OTEL (e.g. Jaeger)
///  * Exporting to Zipkin (Yours truly don't know why though)
///  * Exporting to StdOut
///  * Propagating context to the standard 'traceparent' etc. headers
///  * Propagating context to the experimental 'traceresponse' header (which is subtly different from 'traceparent' in a way yours truly cannot relay faithfully, but which is what the console uses, so it's important not to break it)
///  * Propagating Baggage via http headers
///  * Adding every baggage item as a span attribute to every span
///
/// A service using the tracer-util may or may not want to propagate baggage from its callers. For
/// example, a service that faces the internet directly may not want to anyone set baggage items
/// since these end up in every span which may have an adversarial effect.
///
/// To this end we provide the 'propagate_caller_baggage' argument. When set to 'true' baggage
/// times provided by callers in the 'baggage' header is carried over without prejudice. When set
/// to 'false' we do not propagate incoming baggage, but will stil export baggage the service
/// provides itself.
pub fn initialize_tracing(
    endpoint: Option<&str>,
    service_name: String,
    service_version: Option<&'static str>,
    propagate_caller_baggage: PropagateBaggage,
    enable_stdout_export: ExportTracesStdout,
) -> Result<(), TraceError> {
    // install global collector configured based on RUST_LOG env var.
    tracing_subscriber::fmt::init();

    global::set_text_map_propagator(TextMapCompositePropagator::new(vec![
        Box::new(TraceContextPropagator::new()),
        Box::new(opentelemetry_zipkin::Propagator::new()),
        match propagate_caller_baggage {
            PropagateBaggage::Enable => Box::new(BaggagePropagator::new()),
            PropagateBaggage::Disable => {
                Box::new(InjectOnlyTextMapPropagator(BaggagePropagator::new()))
            }
        },
        Box::new(TraceContextResponsePropagator::new()),
    ]));

    let mut resource_entries = vec![KeyValue::new(semcov::resource::SERVICE_NAME, service_name)];
    if let Some(service_version) = service_version {
        resource_entries.push(KeyValue::new(
            semcov::resource::SERVICE_VERSION,
            service_version,
        ));
    }
    let config = opentelemetry_sdk::trace::Config::default()
        .with_resource(opentelemetry_sdk::Resource::new(resource_entries));

    let otlp_exporter = opentelemetry_otlp::SpanExporterBuilder::Tonic(
        opentelemetry_otlp::new_exporter()
            .tonic()
            .with_endpoint(endpoint.unwrap_or(OTEL_EXPORTER_OTLP_ENDPOINT_DEFAULT)),
    )
    .build_span_exporter()?;

    let mut tracer_provider_builder = TracerProvider::builder()
        .with_batch_exporter(otlp_exporter, opentelemetry_sdk::runtime::Tokio)
        .with_span_processor(BaggageSpanProcessor())
        .with_config(config);

    if let ExportTracesStdout::Enable = enable_stdout_export {
        let stdout_exporter = opentelemetry_stdout::SpanExporter::default();
        tracer_provider_builder = tracer_provider_builder.with_simple_exporter(stdout_exporter);
    }

    let tracer_provider = tracer_provider_builder.build();

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

    fn shutdown(&self) -> opentelemetry::trace::TraceResult<()> {
        Ok(())
    }
}

/// Produce from another TextMapPropagator a derived TextMapPropagator that only supports
/// injection, i.e., serializing the current Context into some text map (the canonical example of
/// such a map being http headers). Such a propagator will not extract any context values from
/// callers.
#[derive(Debug)]
struct InjectOnlyTextMapPropagator<T>(T);

impl<T: TextMapPropagator> TextMapPropagator for InjectOnlyTextMapPropagator<T> {
    fn inject_context(
        &self,
        cx: &opentelemetry::Context,
        injector: &mut dyn opentelemetry::propagation::Injector,
    ) {
        self.0.inject_context(cx, injector);
    }

    fn extract_with_context(
        &self,
        cx: &opentelemetry::Context,
        _extractor: &dyn opentelemetry::propagation::Extractor,
    ) -> opentelemetry::Context {
        cx.clone()
    }

    fn fields(&self) -> opentelemetry::propagation::text_map_propagator::FieldIter<'_> {
        self.0.fields()
    }
}

pub fn shutdown_tracer() {
    global::shutdown_tracer_provider();
}
