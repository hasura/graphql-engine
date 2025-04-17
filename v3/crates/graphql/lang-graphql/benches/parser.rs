use lang_graphql::parser::Parser;

use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use std::fs;

pub fn bench_parser(c: &mut Criterion) {
    let query_files = fs::read_dir("benches/queries/").unwrap();
    let mut group = c.benchmark_group("parser");
    for query_file in query_files {
        let query_path = query_file.unwrap().path();
        let query = fs::read_to_string(&query_path).unwrap();
        // benches/queries/<file_name>.graphql -> <file_name>
        let query_name = query_path.file_stem().unwrap().to_str().unwrap();

        // parse with graphql-parser
        group.bench_with_input(
            BenchmarkId::new("graphql-parser", query_name),
            &query,
            |b, query| b.iter(|| graphql_parser::query::parse_query::<&str>(query).unwrap()),
        );

        // parse with async-graphql-parser
        group.bench_with_input(
            BenchmarkId::new("async-graphql", query_name),
            &query,
            |b, query| b.iter(|| async_graphql_parser::parse_query(query).unwrap()),
        );

        // parse with apollo
        group.bench_with_input(
            BenchmarkId::new("apollo", query_name),
            &query,
            |b, query| b.iter(|| apollo_parser::Parser::new(query).parse()),
        );

        // parse with our parser
        group.bench_with_input(
            BenchmarkId::new("hasura", query_name),
            &query,
            |b, query| b.iter(|| Parser::new(query).parse_executable_document().unwrap()),
        );
    }
    group.finish();
}

criterion_group!(benches, bench_parser);
criterion_main!(benches);
