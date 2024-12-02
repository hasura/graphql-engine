use lang_graphql::http;
use lang_graphql::parser::Parser;
use lang_graphql::schema::sdl;
use lang_graphql::validation;

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use std::collections::HashMap;
use std::fs;
use std::str::FromStr;

pub fn bench_validation(c: &mut Criterion) {
    let query_files = fs::read_dir("benches/validation/introspection").unwrap();
    let mut group = c.benchmark_group("validation");
    for query_file in query_files {
        let query_path = query_file.unwrap().path();
        let query = fs::read_to_string(&query_path).unwrap();
        // benches/queries/<file_name>.graphql -> <file_name>
        let query_name = query_path.file_stem().unwrap().to_str().unwrap();
        let parsed_query = Parser::new(&query).parse_executable_document().unwrap();
        let fake_schema = sdl::SDL::new("type Query {foo: Int}")
            .and_then(|v| v.build_schema())
            .unwrap();
        let request = http::Request {
            operation_name: Some(lang_graphql::ast::common::Name::from_str(query_name).unwrap()),
            query: parsed_query,
            variables: HashMap::new(),
        };
        validation::normalize_request(&sdl::SDLNamespacedGetter(), &fake_schema, &request).unwrap();
        // parse with our parser
        group.bench_with_input(
            BenchmarkId::new("hasura", query_name),
            &(request, fake_schema),
            |b, (request, schema)| {
                b.iter(|| {
                    validation::normalize_request(&sdl::SDLNamespacedGetter(), schema, request)
                        .unwrap()
                });
            },
        );
    }
    group.finish();
}

criterion_group!(benches, bench_validation);
criterion_main!(benches);
