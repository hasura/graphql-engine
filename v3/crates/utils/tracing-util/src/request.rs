//! Functions to provide the appropriate headers when making a HTTP request.

use opentelemetry::baggage::BaggageExt;
use opentelemetry::propagation::TextMapPropagator;
use opentelemetry::{Context, KeyValue};
use opentelemetry_http::HeaderExtractor;
use opentelemetry_sdk::propagation::BaggagePropagator;

// Extract the headers required to propagate the trace context across services from the current
// context.
pub fn get_trace_headers() -> http::HeaderMap {
    let mut headers_map = http::HeaderMap::new();
    let mut header_injector = opentelemetry_http::HeaderInjector(&mut headers_map);
    opentelemetry::global::get_text_map_propagator(|propagator| {
        propagator.inject(&mut header_injector);
    });
    headers_map
}

/// Extract baggage key-value pairs from HTTP headers.
///
/// This function parses the `baggage` header (W3C Baggage format) and returns
/// the key-value pairs as a vector of `KeyValue`.
///
/// Example baggage header: `userId=123,tenantId=456`
pub fn extract_baggage_from_headers(headers: &http::HeaderMap) -> Vec<KeyValue> {
    let propagator = BaggagePropagator::new();
    let extractor = HeaderExtractor(headers);
    let context = propagator.extract_with_context(&Context::new(), &extractor);

    context
        .baggage()
        .iter()
        .map(|(key, (value, _metadata))| KeyValue::new(key.to_string(), value.to_string()))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_baggage_from_headers_empty() {
        let headers = http::HeaderMap::new();
        let baggage = extract_baggage_from_headers(&headers);
        assert!(baggage.is_empty());
    }

    #[test]
    fn test_extract_baggage_from_headers_single_item() {
        let mut headers = http::HeaderMap::new();
        headers.insert("baggage", "userId=123".parse().unwrap());

        let baggage = extract_baggage_from_headers(&headers);

        assert_eq!(baggage.len(), 1);
        assert_eq!(baggage[0].key.as_str(), "userId");
        assert_eq!(baggage[0].value.as_str(), "123");
    }

    #[test]
    fn test_extract_baggage_from_headers_multiple_items() {
        let mut headers = http::HeaderMap::new();
        headers.insert(
            "baggage",
            "userId=123,tenantId=456,env=prod".parse().unwrap(),
        );

        let mut baggage = extract_baggage_from_headers(&headers);
        // Sort by key for deterministic comparison
        baggage.sort_by(|a, b| a.key.as_str().cmp(b.key.as_str()));

        assert_eq!(baggage.len(), 3);

        assert_eq!(baggage[0].key.as_str(), "env");
        assert_eq!(baggage[0].value.as_str(), "prod");

        assert_eq!(baggage[1].key.as_str(), "tenantId");
        assert_eq!(baggage[1].value.as_str(), "456");

        assert_eq!(baggage[2].key.as_str(), "userId");
        assert_eq!(baggage[2].value.as_str(), "123");
    }

    #[test]
    fn test_extract_baggage_from_headers_with_spaces() {
        let mut headers = http::HeaderMap::new();
        headers.insert("baggage", "userId=123, tenantId=456".parse().unwrap());

        let mut baggage = extract_baggage_from_headers(&headers);
        baggage.sort_by(|a, b| a.key.as_str().cmp(b.key.as_str()));

        assert_eq!(baggage.len(), 2);
        assert_eq!(baggage[0].key.as_str(), "tenantId");
        assert_eq!(baggage[0].value.as_str(), "456");
        assert_eq!(baggage[1].key.as_str(), "userId");
        assert_eq!(baggage[1].value.as_str(), "123");
    }

    #[test]
    fn test_extract_baggage_from_headers_url_encoded_value() {
        let mut headers = http::HeaderMap::new();
        // URL-encoded value: "hello world" -> "hello%20world"
        headers.insert("baggage", "message=hello%20world".parse().unwrap());

        let baggage = extract_baggage_from_headers(&headers);

        assert_eq!(baggage.len(), 1);
        assert_eq!(baggage[0].key.as_str(), "message");
        // The BaggagePropagator decodes URL-encoded values
        assert_eq!(baggage[0].value.as_str(), "hello world");
    }

    #[test]
    fn test_extract_baggage_from_headers_ignores_other_headers() {
        let mut headers = http::HeaderMap::new();
        headers.insert("baggage", "userId=123".parse().unwrap());
        headers.insert("content-type", "application/json".parse().unwrap());
        headers.insert("authorization", "Bearer token".parse().unwrap());

        let baggage = extract_baggage_from_headers(&headers);

        assert_eq!(baggage.len(), 1);
        assert_eq!(baggage[0].key.as_str(), "userId");
        assert_eq!(baggage[0].value.as_str(), "123");
    }

    #[test]
    fn test_extract_baggage_from_headers_no_baggage_header() {
        let mut headers = http::HeaderMap::new();
        headers.insert("content-type", "application/json".parse().unwrap());

        let baggage = extract_baggage_from_headers(&headers);
        assert!(baggage.is_empty());
    }
}
