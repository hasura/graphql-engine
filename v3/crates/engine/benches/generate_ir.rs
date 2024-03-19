use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use engine::schema::GDS;
use hasura_authn_core::Identity;
use lang_graphql::http::Request;
use lang_graphql::parser::Parser;
use lang_graphql::validation::normalize_request;
use open_dds::permissions::Role;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

use engine::execute;

pub fn bench_generate_ir(c: &mut Criterion) {
    let test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");
    let metadata_json_str = fs::read_to_string(test_dir.join("schema.json")).unwrap();
    let metadata = open_dds::Metadata::from_json_str(&metadata_json_str).unwrap();
    let gds = GDS::new(metadata).unwrap();
    let schema = GDS::build_schema(&gds).unwrap();

    let mut group = c.benchmark_group("generate_ir");

    let session = Identity::admin(Role::new("admin"))
        .get_role_authorization(None)
        .unwrap()
        .build_session(HashMap::new());

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
            variables: HashMap::new(),
        };

        let normalized_request = normalize_request(&session.role, &schema, &request).unwrap();

        group.bench_with_input(
            BenchmarkId::new("generate_ir", test_name),
            &(&schema, &normalized_request),
            |b, (schema, normalized_request)| {
                b.iter(|| execute::generate_ir(schema, &session, normalized_request).unwrap())
            },
        );
    }
    group.finish();
}

criterion_group!(benches, bench_generate_ir);
criterion_main!(benches);
