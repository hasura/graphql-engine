use criterion::{BenchmarkId, Criterion, SamplingMode, criterion_group, criterion_main};
use graphql_schema::GDS;
use hasura_authn_core::Identity;
use lang_graphql::http::Request;
use lang_graphql::parser::Parser;
use lang_graphql::validation::{NonNullGraphqlVariablesValidation, normalize_request};
use open_dds::permissions::Role;
use std::collections::BTreeMap;
use std::fs;
use std::path::PathBuf;
use std::time::Duration;

pub fn bench_generate_ir(c: &mut Criterion) {
    let test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");
    let metadata_json_str = fs::read_to_string(test_dir.join("schema.json")).unwrap();
    let metadata = open_dds::Metadata::from_json_str(&metadata_json_str).unwrap();
    let gds = GDS::new_with_default_flags(metadata).unwrap();
    let validate_non_null_graphql_variables = if gds
        .metadata
        .runtime_flags
        .contains(metadata_resolve::flags::ResolvedRuntimeFlag::ValidateNonNullGraphqlVariables)
    {
        NonNullGraphqlVariablesValidation::Validate
    } else {
        NonNullGraphqlVariablesValidation::DoNotValidate
    };
    let schema = GDS::build_schema(&gds).unwrap();

    let mut group = c.benchmark_group("generate_ir");

    // these numbers are fairly low, optimising for runtime of benchmark suite
    group.warm_up_time(Duration::from_millis(500));
    group.sample_size(20);
    group.sampling_mode(SamplingMode::Flat);

    let request_headers = reqwest::header::HeaderMap::new();
    let session = Identity::admin(Role::new("admin"))
        .get_role_authorization(None)
        .unwrap()
        .build_session(BTreeMap::new());

    for input_file in fs::read_dir(test_dir.join("generate_ir")).unwrap() {
        let entry = input_file.unwrap();
        let raw_request = {
            let path = entry.path();
            assert!(path.is_dir());
            fs::read_to_string(path.join("request.gql")).unwrap()
        };
        let path = entry.path();
        let test_name = path.file_name().unwrap().to_str().unwrap();

        let query = Parser::new(&raw_request)
            .parse_executable_document()
            .unwrap();

        let request = Request {
            operation_name: None,
            query,
            variables: BTreeMap::new(),
        };

        let normalized_request = normalize_request(
            &graphql_schema::GDSRoleNamespaceGetter {
                scope: session.role.clone(),
            },
            &schema,
            &request,
            validate_non_null_graphql_variables,
        )
        .unwrap();

        group.bench_with_input(
            BenchmarkId::new("generate_ir", test_name),
            &(&schema, &normalized_request),
            |b, (schema, normalized_request)| {
                b.iter(|| {
                    graphql_frontend::generate_ir(
                        schema,
                        &gds.metadata,
                        &session,
                        &request_headers,
                        normalized_request,
                    )
                    .unwrap()
                });
            },
        );
    }
    group.finish();
}

criterion_group!(benches, bench_generate_ir);
criterion_main!(benches);
