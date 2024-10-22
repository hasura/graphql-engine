use std::future::Future;
use std::pin::Pin;

use http::HeaderMap;
use opentelemetry::baggage::{BaggageExt, KeyValueMetadata};
use opentelemetry::global::{self, BoxedTracer};
use opentelemetry::trace::{
    get_active_span, FutureExt, Span, SpanContext, SpanRef, TraceContextExt, Tracer as OtelTracer,
};
use opentelemetry::{Context, Key};
use opentelemetry_http::HeaderExtractor;

use crate::traceable::{ErrorVisibility, Traceable, TraceableError};

pub static GLOBAL_TRACER_NAME: &str = "engine-tracing-util";

#[derive(Clone, Copy, derive_more::Display)]
pub enum SpanVisibility {
    #[display(fmt = "internal")]
    Internal,
    #[display(fmt = "user")]
    User,
}

#[derive(Clone, Copy)]
pub enum AttributeVisibility {
    Default,
    Internal,
}

pub fn set_status_on_current_span<R>(result: &R)
where
    R: Traceable,
{
    get_active_span(|span| {
        set_span_attributes(&span, SpanVisibility::User, result);
    });
}

fn set_span_attributes<R>(span: &SpanRef, visibility: SpanVisibility, result: &R)
where
    R: Traceable,
{
    set_attribute_on_span(
        span,
        AttributeVisibility::Internal,
        "visibility",
        visibility.to_string(),
    );

    if let Some(e) = result.get_error() {
        let is_private_error = matches!(e.visibility(), ErrorVisibility::Internal)
            && matches!(visibility, SpanVisibility::User);

        span.set_status(opentelemetry::trace::Status::Error {
            description: if is_private_error {
                "Internal error".into()
            } else {
                e.description().into()
            },
        });

        set_attribute_on_span(
            span,
            AttributeVisibility::Internal,
            "error_description",
            e.description(),
        );

        set_attribute_on_span(
            span,
            AttributeVisibility::Internal,
            "error_details",
            e.details(),
        );
    }
}

pub type AttributeValue = opentelemetry::Value;

fn set_attribute_on_span(
    span: &SpanRef,
    visibility: AttributeVisibility,
    key: &'static str,
    value: impl Into<AttributeValue>,
) {
    let key_with_visibility: Key = match visibility {
        AttributeVisibility::Default => key.into(),
        AttributeVisibility::Internal => format!("internal.{key}").into(),
    };

    span.set_attribute(opentelemetry::KeyValue::new(key_with_visibility, value));
}

/// Sets an attribute on the active span, prefixing the `key` with `internal.` if `visibility` is `Internal`.
pub fn set_attribute_on_active_span(
    visibility: AttributeVisibility,
    key: &'static str,
    value: impl Into<AttributeValue>,
) {
    get_active_span(|span| set_attribute_on_span(&span, visibility, key, value));
}

/// Adds an event on the active span, with the given `name` and no attributes.
// TODO: Add support for attributes
pub fn add_event_on_active_span(name: String) {
    get_active_span(|span| span.add_event(name, vec![]));
}

/// Runs the given closure `f` in the current span by attaching the given `baggage` to the current context.
pub fn run_with_baggage<I: Into<KeyValueMetadata>, T>(baggage: Vec<I>, f: impl FnOnce() -> T) -> T {
    // Create a context from the current context with the given baggage.
    let cx = Context::current_with_baggage(baggage);
    // Apply the context with baggage to the closure execution.
    let _guard = cx.attach();
    f()
}

/// A link to a span.
/// Contains the context of the span to link to, and the baggage items to propagate across the link.
#[derive(Clone)]
pub struct SpanLink {
    span_context: SpanContext,
    baggage_items: Vec<KeyValueMetadata>,
}

impl SpanLink {
    /// Creates a new `SpanLink` from the current active span.
    pub fn from_current_span() -> Self {
        let span_context = get_active_span(|span| span.span_context().clone());
        let baggage_items = Context::current()
            .baggage()
            .into_iter()
            .map(|(key, (value, metadata))| {
                KeyValueMetadata::new(key.clone(), value.clone(), metadata.clone())
            })
            .collect();
        Self {
            span_context,
            baggage_items,
        }
    }
}

/// Wrapper around the OpenTelemetry tracer. Used for providing convenience methods to add spans.
pub struct Tracer {
    tracer: BoxedTracer,
}

impl Tracer {
    pub(crate) fn new(tracer: BoxedTracer) -> Self {
        Self { tracer }
    }
}

impl Tracer {
    /// Runs the given closure `f` in a new span with the given `name`, and sets a visibility attribute
    /// on the span based on `visibility` and sets the span's error attributes based on the result of the closure.
    pub fn in_span<R, F>(
        &self,
        name: &'static str,
        display_name: impl Into<AttributeValue>,
        visibility: SpanVisibility,
        f: F,
    ) -> R
    where
        F: FnOnce() -> R,
        R: Traceable,
    {
        self.tracer.in_span(name, |cx| {
            let result = f();
            set_attribute_on_span(
                &cx.span(),
                AttributeVisibility::Default,
                "display.name",
                display_name,
            );
            set_span_attributes(&cx.span(), visibility, &result);
            result
        })
    }

    /// Runs the tive closure `f` asynchronously by opening a span in a new trace with the given `name`, and sets a visibility attribute
    /// on the span based on `visibility` and sets the span's error attributes based on the result of the closure.
    /// The span is linked to the given `link`.
    pub async fn new_trace_async_with_link<'a, R, F>(
        &'a self,
        name: &'static str,
        display_name: impl Into<AttributeValue>,
        visibility: SpanVisibility,
        link: SpanLink,
        f: F,
    ) -> R
    where
        // Note: This uses a Boxed polymorphic future as the return type of `f` instead of using a generic type for the Future
        // because when using generics for this, it takes an extremely long time to build the engine binary.
        F: FnOnce() -> Pin<Box<dyn Future<Output = R> + 'a + Send>>,
        R: Traceable,
    {
        // Create a new empty context with baggage
        let context = Context::new().with_baggage(link.baggage_items);
        // Create a new span with the given name and empty context.
        // This span has no parent, so it opens a new trace.
        let mut span = self.tracer.start_with_context(name, &context);
        // Link the span to the given span context.
        span.add_link(link.span_context, Vec::new());
        async move {
            let result = f().await;
            get_active_span(|span_ref| {
                set_attribute_on_span(
                    &span_ref,
                    AttributeVisibility::Default,
                    "display.name",
                    display_name,
                );
                set_span_attributes(&span_ref, visibility, &result);
            });
            result
        }
        .with_context(context.with_span(span)) // Run the above async block within the new span
        .await
    }

    /// Runs the given closure `f` asynchronously in a new span with the given `name`, and sets a visibility attribute
    /// on the span based on `visibility` and sets the span's error attributes based on the result of the closure.
    pub async fn in_span_async<'a, R, F>(
        &'a self,
        name: &'static str,
        display_name: impl Into<AttributeValue>,
        visibility: SpanVisibility,
        f: F,
    ) -> R
    where
        // Note: This uses a Boxed polymorphic future as the return type of `f` instead of using a generic type for the Future
        // because when using generics for this, it takes an extremely long time to build the engine binary.
        F: FnOnce() -> Pin<Box<dyn Future<Output = R> + 'a + Send>>,
        R: Traceable,
    {
        self.tracer
            .in_span(name, |cx| {
                async move {
                    let result = f().await;
                    get_active_span(|span| {
                        set_attribute_on_span(
                            &span,
                            AttributeVisibility::Default,
                            "display.name",
                            display_name,
                        );
                        set_span_attributes(&span, visibility, &result);
                    });
                    result
                }
                .with_context(cx)
            })
            .await
    }

    pub async fn in_span_async_with_parent_context<'a, R, F>(
        &'a self,
        name: &'static str,
        display_name: impl Into<AttributeValue>,
        visibility: SpanVisibility,
        parent_headers: &HeaderMap<http::HeaderValue>,
        f: F,
    ) -> R
    where
        F: FnOnce() -> Pin<Box<dyn Future<Output = R> + 'a + Send>>,
        R: Traceable,
    {
        let parent_context = global::get_text_map_propagator(|propagator| {
            propagator.extract(&HeaderExtractor(parent_headers))
        });

        self.in_span_async(name, display_name, visibility, f)
            .with_context(parent_context)
            .await
    }
}

/// Util for accessing the globally installed tracer
pub fn global_tracer() -> Tracer {
    Tracer::new(opentelemetry::global::tracer(GLOBAL_TRACER_NAME))
}
