use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use engine::execute::query_plan::{execute_query_plan, generate_query_plan};
use engine::execute::{execute_query_internal, generate_ir};
use engine::schema::GDS;
use hasura_authn_core::Identity;
use lang_graphql::http::RawRequest;
use open_dds::permissions::Role;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use tokio::runtime::Runtime;

extern crate json_value_merge;
use json_value_merge::Merge;
use serde_json::Value;

use std::path::Path;

use lang_graphql as gql;

pub fn merge_with_common_metadata(
    common_metadata_path: &Path,
    metadata_path_string: &Path,
) -> Value {
    let common_metadata = fs::read_to_string(common_metadata_path).unwrap();
    let test_metadata = fs::read_to_string(metadata_path_string).unwrap();

    let mut first_json_value: Value = serde_json::from_str(&common_metadata.to_string()).unwrap();
    let second_json_value: Value = serde_json::from_str(&test_metadata.to_string()).unwrap();
    first_json_value.merge(&second_json_value);
    first_json_value
}

pub fn bench_execute(
    c: &mut Criterion,
    test_path_string: &str,
    common_metadata_path_string: &str,
    benchmark_group: &str,
) {
    let root_test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");
    let test_path = root_test_dir.join(test_path_string);
    let request_path = test_path.join("request.gql");

    let common_metadata_path = root_test_dir.join(common_metadata_path_string);
    let metadata_path = test_path.join("metadata.json");
    let metadata = merge_with_common_metadata(&metadata_path, &common_metadata_path);

    let gds = GDS::new(serde_json::from_value(metadata).unwrap()).unwrap();
    let schema = GDS::build_schema(&gds).unwrap();

    let http_client = reqwest::Client::new();
    let runtime = Runtime::new().unwrap();

    let query = fs::read_to_string(request_path).unwrap();
    let raw_request = RawRequest {
        operation_name: None,
        query,
        variables: None,
    };

    let session = Identity::admin(Role::new("admin"))
        .get_role_authorization(None)
        .unwrap()
        .build_session(HashMap::new());

    let mut group = c.benchmark_group(benchmark_group);

    // Parse request
    group.bench_with_input(
        BenchmarkId::new("bench", "Resolution of raw request"),
        &(&runtime, &raw_request),
        |b, (runtime, request)| {
            b.to_async(*runtime).iter(|| async {
                gql::parser::Parser::new(&request.query)
                    .parse_executable_document()
                    .unwrap()
            })
        },
    );

    let query = gql::parser::Parser::new(&raw_request.query)
        .parse_executable_document()
        .unwrap();

    // Normalize request
    let request = gql::http::Request {
        operation_name: None,
        query,
        variables: HashMap::default(),
    };

    group.bench_with_input(
        BenchmarkId::new("bench_execute", "Normalize request"),
        &(&runtime, &schema, &request),
        |b, (runtime, schema, request)| {
            b.to_async(*runtime).iter(|| async {
                gql::validation::normalize_request(&session.role, schema, request).unwrap();
            })
        },
    );

    let normalized_request =
        gql::validation::normalize_request(&session.role, &schema, &request).unwrap();

    // Generate IR
    group.bench_with_input(
        BenchmarkId::new("bench_execute", "Generate IR"),
        &(&runtime, &schema),
        |b, (runtime, schema)| {
            b.to_async(*runtime)
                .iter(|| async { generate_ir(schema, &session, &normalized_request).unwrap() })
        },
    );

    let ir = generate_ir(&schema, &session, &normalized_request).unwrap();

    // Generate Query Plan
    group.bench_with_input(
        BenchmarkId::new("bench_execute", "Generate Query Plan"),
        &(&runtime),
        |b, runtime| {
            b.to_async(*runtime)
                .iter(|| async { generate_query_plan(&ir).unwrap() })
        },
    );

    // Execute Query plan
    group.bench_with_input(
        BenchmarkId::new("bench_execute", "Execute Query Plan"),
        &(&runtime),
        |b, runtime| {
            b.to_async(*runtime).iter(|| async {
                execute_query_plan(&http_client, generate_query_plan(&ir).unwrap(), None).await
            })
        },
    );

    // Total execution time from start to finish
    group.bench_with_input(
        BenchmarkId::new("bench_execute", "Total Execution time"),
        &(&runtime, &schema, raw_request),
        |b, (runtime, schema, request)| {
            b.to_async(*runtime).iter(|| async {
                execute_query_internal(&http_client, schema, &session, request.clone(), None)
                    .await
                    .unwrap()
            })
        },
    );

    group.finish();
}

fn bench_execute_all(c: &mut Criterion) {
    // Simple select
    let test_path_string = "execute/models/select_one/simple_select";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    bench_execute(
        c,
        test_path_string,
        common_metadata_path_string,
        "simple_select",
    );

    // Select Many
    let test_path_string = "execute/models/select_many/simple_select";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    bench_execute(
        c,
        test_path_string,
        common_metadata_path_string,
        "select_many",
    );

    // Select Many with where clause
    let test_path_string = "execute/models/select_many/where/simple";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    bench_execute(
        c,
        test_path_string,
        common_metadata_path_string,
        "select_many_where",
    );

    // Object Relationships
    let test_path_string = "execute/relationships/object";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    bench_execute(
        c,
        test_path_string,
        common_metadata_path_string,
        "object_relationship",
    );

    // Array Relationships
    let test_path_string = "execute/relationships/array";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    bench_execute(
        c,
        test_path_string,
        common_metadata_path_string,
        "array_relationship",
    );

    // Relay node field
    let test_path_string = "execute/relay/relay";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    bench_execute(
        c,
        test_path_string,
        common_metadata_path_string,
        "relay_node_field",
    );
}

criterion_group!(benches, bench_execute_all);
criterion_main!(benches);
