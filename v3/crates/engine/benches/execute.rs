use core::time::Duration;
use criterion::{BenchmarkId, Criterion, SamplingMode, criterion_group, criterion_main};
use engine_types::{ExposeInternalErrors, HttpContext};
use graphql_frontend::{
    execute_mutation_plan, execute_query_internal, execute_query_plan, generate_ir,
};
use graphql_ir::{RequestPlan, generate_request_plan};
use graphql_schema::GDS;
use hasura_authn_core::Identity;
use lang_graphql::http::RawRequest;
use open_dds::permissions::Role;
use std::collections::BTreeMap;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::runtime::Runtime;
extern crate json_value_merge;
use json_value_merge::Merge;
use serde_json::Value;

use std::path::Path;

use lang_graphql as gql;

// match allocator used by engine binary
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

pub fn merge_with_common_metadata(
    common_metadata_path: &Path,
    metadata_path_string: &Path,
) -> Value {
    let common_metadata = fs::read_to_string(common_metadata_path).unwrap();
    let test_metadata = fs::read_to_string(metadata_path_string).unwrap();

    let mut first_json_value: Value = serde_json::from_str(&common_metadata).unwrap();
    let second_json_value: Value = serde_json::from_str(&test_metadata).unwrap();
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

    let (resolved_metadata, _) = metadata_resolve::resolve(
        open_dds::traits::OpenDd::deserialize(metadata, jsonpath::JSONPath::new()).unwrap(),
        &metadata_resolve::configuration::Configuration::default(),
    )
    .unwrap();

    let validate_non_null_graphql_variables = if resolved_metadata
        .runtime_flags
        .contains(metadata_resolve::flags::ResolvedRuntimeFlag::ValidateNonNullGraphqlVariables)
    {
        gql::validation::NonNullGraphqlVariablesValidation::Validate
    } else {
        gql::validation::NonNullGraphqlVariablesValidation::DoNotValidate
    };

    let gds = GDS {
        metadata: Arc::new(resolved_metadata.clone()),
    };

    let schema = gds.build_schema().unwrap();

    let http_context = HttpContext {
        client: reqwest::Client::new(),
        ndc_response_size_limit: None,
    };
    let runtime = Runtime::new().unwrap();

    let query = fs::read_to_string(request_path).unwrap();
    let raw_request = RawRequest {
        operation_name: None,
        query,
        variables: None,
    };

    let request_headers = reqwest::header::HeaderMap::new();
    let session = Identity::admin(Role::new("admin"))
        .get_role_authorization(None)
        .unwrap()
        .build_session(BTreeMap::new());

    let mut group = c.benchmark_group(benchmark_group);

    // these numbers are fairly low, optimising for runtime of benchmark suite
    group.warm_up_time(Duration::from_millis(100));
    group.sample_size(1000);
    group.measurement_time(Duration::from_secs(5));
    group.sampling_mode(SamplingMode::Flat);

    // Parse request
    group.bench_with_input(
        BenchmarkId::new("bench", "Resolution of raw request"),
        &(&runtime, &raw_request),
        |b, (runtime, request)| {
            b.to_async(*runtime).iter(|| async {
                gql::parser::Parser::new(&request.query)
                    .parse_executable_document()
                    .unwrap()
            });
        },
    );

    let query = gql::parser::Parser::new(&raw_request.query)
        .parse_executable_document()
        .unwrap();

    // Normalize request
    let request = gql::http::Request {
        operation_name: None,
        query,
        variables: BTreeMap::default(),
    };

    group.bench_with_input(
        BenchmarkId::new("bench_execute", "Normalize request"),
        &(&runtime, &schema, &request),
        |b, (runtime, schema, request)| {
            b.to_async(*runtime).iter(|| async {
                gql::validation::normalize_request(
                    &graphql_schema::GDSRoleNamespaceGetter {
                        scope: session.role.clone(),
                    },
                    schema,
                    request,
                    validate_non_null_graphql_variables,
                )
                .unwrap();
            });
        },
    );

    let normalized_request = gql::validation::normalize_request(
        &graphql_schema::GDSRoleNamespaceGetter {
            scope: session.role.clone(),
        },
        &schema,
        &request,
        validate_non_null_graphql_variables,
    )
    .unwrap();

    // Generate IR
    group.bench_with_input(
        BenchmarkId::new("bench_execute", "Generate IR"),
        &(&runtime, &schema),
        |b, (runtime, schema)| {
            b.to_async(*runtime).iter(|| async {
                generate_ir(
                    schema,
                    &gds.metadata,
                    &session,
                    &request_headers,
                    &normalized_request,
                )
                .unwrap()
            });
        },
    );

    let ir = generate_ir(
        &schema,
        &gds.metadata,
        &session,
        &request_headers,
        &normalized_request,
    )
    .unwrap();

    // Generate Query Plan
    group.bench_with_input(
        BenchmarkId::new("bench_execute", "Generate Query Plan"),
        &(&runtime),
        |b, runtime| {
            b.to_async(*runtime).iter(|| async {
                generate_request_plan(&ir, &resolved_metadata, &session, &request_headers).unwrap()
            });
        },
    );

    // Execute Query plan
    group.bench_with_input(
        BenchmarkId::new("bench_execute", "Execute Query Plan"),
        &(&runtime),
        |b, runtime| {
            b.to_async(*runtime).iter(|| async {
                match generate_request_plan(&ir, &resolved_metadata, &session, &request_headers)
                    .unwrap()
                {
                    RequestPlan::QueryPlan(query_plan) => {
                        let execute_query_result =
                            execute_query_plan(&http_context, query_plan, None).await;
                        assert!(
                            !execute_query_result.root_fields.is_empty(),
                            "IndexMap is empty!"
                        );
                    }
                    RequestPlan::MutationPlan(mutation_plan) => {
                        let execute_query_result =
                            execute_mutation_plan(&http_context, mutation_plan, None).await;
                        assert!(
                            !execute_query_result.root_fields.is_empty(),
                            "IndexMap is empty!"
                        );
                    }
                    RequestPlan::SubscriptionPlan(_alias, _subscription_plan) => {
                        // subscriptions are not supported
                        panic!("subscriptions not expected here")
                    }
                }
            });
        },
    );

    // Total execution time from start to finish
    group.bench_with_input(
        BenchmarkId::new("bench_execute", "Total Execution time"),
        &(&runtime, &schema, raw_request),
        |b, (runtime, schema, request)| {
            b.to_async(*runtime).iter(|| async {
                execute_query_internal(
                    ExposeInternalErrors::Expose,
                    &http_context,
                    schema,
                    &resolved_metadata.clone().into(),
                    &session,
                    &request_headers,
                    request.clone(),
                    None,
                )
                .await
                .unwrap()
            });
        },
    );

    group.finish();
}

fn bench_execute_all(c: &mut Criterion) {
    // Simple select
    let test_path_string = "execute/models/select_one/simple_select";
    let common_metadata_path_string =
        "execute/common_metadata/postgres_connector_ndc_v01_schema.json";
    bench_execute(
        c,
        test_path_string,
        common_metadata_path_string,
        "simple_select",
    );

    // Select Many
    let test_path_string = "execute/models/select_many/simple_select";
    let common_metadata_path_string =
        "execute/common_metadata/postgres_connector_ndc_v01_schema.json";
    bench_execute(
        c,
        test_path_string,
        common_metadata_path_string,
        "select_many",
    );

    // Select Many with where clause
    let test_path_string = "execute/models/select_many/where/simple";
    let common_metadata_path_string =
        "execute/common_metadata/postgres_connector_ndc_v01_schema.json";
    bench_execute(
        c,
        test_path_string,
        common_metadata_path_string,
        "select_many_where",
    );

    // Object Relationships
    let test_path_string = "execute/relationships/object";
    let common_metadata_path_string =
        "execute/common_metadata/postgres_connector_ndc_v01_schema.json";
    bench_execute(
        c,
        test_path_string,
        common_metadata_path_string,
        "object_relationship",
    );

    // Array Relationships
    let test_path_string = "execute/relationships/array";
    let common_metadata_path_string =
        "execute/common_metadata/postgres_connector_ndc_v01_schema.json";
    bench_execute(
        c,
        test_path_string,
        common_metadata_path_string,
        "array_relationship",
    );

    // Relay node field
    let test_path_string = "execute/relay/relay";
    let common_metadata_path_string =
        "execute/common_metadata/postgres_connector_ndc_v01_schema.json";
    bench_execute(
        c,
        test_path_string,
        common_metadata_path_string,
        "relay_node_field",
    );
}

criterion_group!(benches, bench_execute_all);
criterion_main!(benches);
