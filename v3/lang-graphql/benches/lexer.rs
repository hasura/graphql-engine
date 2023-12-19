use lang_graphql::lexer::Lexer;

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use std::fs;

pub fn bench_lexer(c: &mut Criterion) {
    let query_files = fs::read_dir("benches/queries/").unwrap();
    let mut group = c.benchmark_group("lexer");
    for query_file in query_files {
        let query_path = query_file.unwrap().path();
        let query = fs::read_to_string(&query_path).unwrap();
        // benches/queries/<file_name>.graphql -> <file_name>
        let query_name = query_path.file_stem().unwrap().to_str().unwrap();

        // lex with juniper
        // group.bench_with_input(
        //     BenchmarkId::new("juniper", query_name),
        //     &query,
        //     |b, query| b.iter(|| juniper::Lexer::new(query).collect::<Vec<juniper::LexerResult>>()),
        // );

        // lex with apollo
        group.bench_with_input(
            BenchmarkId::new("apollo", query_name),
            &query,
            |b, query| b.iter(|| apollo_parser::Lexer::new(query).collect::<Vec<_>>()),
        );

        // lex with logos
        // group.bench_with_input(BenchmarkId::new("logos", query_name), &query, |b, query| {
        //     b.iter(|| TokenKind::lexer(query).collect::<Vec<TokenKind>>())
        // });

        // lex with our library
        group.bench_with_input(
            BenchmarkId::new("hasura", query_name),
            &query,
            |b, query| b.iter(|| Lexer::new(query).collect::<Vec<_>>()),
        );
    }
    group.finish();
}

criterion_group!(benches, bench_lexer);
criterion_main!(benches);
