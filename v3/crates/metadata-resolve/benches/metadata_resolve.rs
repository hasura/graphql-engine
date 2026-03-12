use criterion::{BatchSize, BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};
use metadata_resolve::configuration::Configuration;
use open_dds::traits::OpenDd;
use std::fs;
use std::path::PathBuf;

/// Helper function to load and parse metadata from a test file
fn load_metadata(test_path: &str) -> (open_dds::Metadata, Configuration, usize) {
    let metadata_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("passing")
        .join(test_path)
        .join("metadata.json");

    load_metadata_from_path(&metadata_path)
}

/// Helper function to load and parse metadata from an absolute path
fn load_metadata_from_path(metadata_path: &PathBuf) -> (open_dds::Metadata, Configuration, usize) {
    let metadata_json_text = fs::read_to_string(metadata_path)
        .unwrap_or_else(|error| panic!("Could not read file {metadata_path:?}: {error}"));

    let file_size = metadata_json_text.len();

    let metadata_json_value = serde_json::from_str(&metadata_json_text)
        .unwrap_or_else(|error| panic!("Could not parse JSON: {error}"));

    let metadata = open_dds::Metadata::deserialize(metadata_json_value, jsonpath::JSONPath::new())
        .unwrap_or_else(|error| panic!("Could not deserialize metadata: {error}"));

    let configuration = Configuration::default();

    (metadata, configuration, file_size)
}

/// Benchmark metadata resolution for different complexity levels
fn bench_metadata_resolve(c: &mut Criterion) {
    let mut group = c.benchmark_group("metadata_resolve");

    // Test cases with different complexity levels
    let test_cases = vec![
        ("simple", "Simple empty metadata"),
        ("boolean_expression_type/basic", "Basic boolean expression"),
        (
            "models/all_args_are_set_including_connector_link_presets",
            "Model with arguments",
        ),
        (
            "aggregate_expressions/relationship",
            "Complex aggregate expressions",
        ),
        (
            "boolean_expression_type/nested_recursive_object",
            "Nested recursive objects",
        ),
        (
            "boolean_expression_type/partial_supergraph",
            "Partial supergraph",
        ),
        (
            "boolean_expression_type/regression",
            "Large regression test case",
        ),
    ];

    for (test_path, description) in test_cases {
        let (_, _, file_size) = load_metadata(test_path);
        group.throughput(Throughput::Bytes(file_size as u64));

        group.bench_with_input(
            BenchmarkId::new("resolve", description),
            &test_path,
            |b, &test_path| {
                b.iter_batched(
                    || load_metadata(test_path),
                    |(metadata, configuration, _)| {
                        metadata_resolve::resolve(metadata, &configuration)
                            .expect("Metadata resolution should succeed")
                    },
                    BatchSize::SmallInput,
                );
            },
        );
    }

    group.finish();
}

/// Benchmark individual stages of metadata resolution
fn bench_metadata_resolve_stages(c: &mut Criterion) {
    let mut group = c.benchmark_group("metadata_resolve_stages");

    // Use the largest test case for detailed stage analysis
    let (metadata, configuration, file_size) = load_metadata("boolean_expression_type/regression");
    group.throughput(Throughput::Bytes(file_size as u64));

    group.bench_function("full_resolve", |b| {
        b.iter_batched(
            || (metadata.clone(), configuration.clone()),
            |(metadata, configuration)| {
                metadata_resolve::resolve(metadata, &configuration)
                    .expect("Metadata resolution should succeed")
            },
            BatchSize::SmallInput,
        );
    });

    group.finish();
}

/// Benchmark parsing vs resolution separately
fn bench_parsing_vs_resolution(c: &mut Criterion) {
    let mut group = c.benchmark_group("parsing_vs_resolution");

    let test_path = "boolean_expression_type/regression";
    let metadata_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("passing")
        .join(test_path)
        .join("metadata.json");

    let metadata_json_text = fs::read_to_string(&metadata_path).unwrap();
    let file_size = metadata_json_text.len();
    group.throughput(Throughput::Bytes(file_size as u64));

    // Benchmark JSON parsing
    group.bench_function("json_parsing", |b| {
        b.iter_batched(
            || metadata_json_text.clone(),
            |json_text| {
                serde_json::from_str::<serde_json::Value>(&json_text)
                    .expect("JSON parsing should succeed")
            },
            BatchSize::SmallInput,
        );
    });

    // Benchmark from_json_str (end-to-end parsing)
    group.bench_function("from_json_str", |b| {
        b.iter_batched(
            || metadata_json_text.clone(),
            |json_text| {
                open_dds::Metadata::from_json_str(&json_text).expect("from_json_str should succeed")
            },
            BatchSize::SmallInput,
        );
    });

    // Benchmark OpenDDS deserialization
    group.bench_function("opendds_deserialize", |b| {
        b.iter_batched(
            || serde_json::from_str::<serde_json::Value>(&metadata_json_text).unwrap(),
            |json_value| {
                open_dds::Metadata::deserialize(json_value, jsonpath::JSONPath::new())
                    .expect("OpenDDS deserialization should succeed")
            },
            BatchSize::SmallInput,
        );
    });

    // Benchmark MetadataAccessor creation separately
    group.bench_function("metadata_accessor_only", |b| {
        let json_value = serde_json::from_str::<serde_json::Value>(&metadata_json_text).unwrap();
        let metadata =
            open_dds::Metadata::deserialize(json_value, jsonpath::JSONPath::new()).unwrap();
        b.iter_batched(
            || metadata.clone(),
            open_dds::accessor::MetadataAccessor::new,
            BatchSize::SmallInput,
        );
    });

    // Benchmark metadata resolution only
    group.bench_function("metadata_resolve_only", |b| {
        let (metadata, configuration, _) = load_metadata(test_path);
        b.iter_batched(
            || (metadata.clone(), configuration.clone()),
            |(metadata, configuration)| {
                metadata_resolve::resolve(metadata, &configuration)
                    .expect("Metadata resolution should succeed")
            },
            BatchSize::SmallInput,
        );
    });

    group.finish();
}

/// Benchmark individual stages within metadata resolution
fn bench_detailed_stages(c: &mut Criterion) {
    let mut group = c.benchmark_group("detailed_stages");

    let test_path = "boolean_expression_type/regression";
    let (metadata, _configuration, file_size) = load_metadata(test_path);
    group.throughput(Throughput::Bytes(file_size as u64));

    // Benchmark MetadataAccessor creation
    group.bench_function("metadata_accessor_creation", |b| {
        b.iter_batched(
            || metadata.clone(),
            open_dds::accessor::MetadataAccessor::new,
            BatchSize::SmallInput,
        );
    });

    // Benchmark just the metadata cloning to see the overhead
    group.bench_function("metadata_clone", |b| {
        b.iter_batched(
            || &metadata,
            std::clone::Clone::clone,
            BatchSize::SmallInput,
        );
    });

    // For more detailed stage benchmarking, we'd need to expose internal functions
    // or create a version of resolve_internal that allows stage-by-stage measurement

    group.finish();
}

fn benchmark_data_path(filename: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("cloud")
        .join("build-artifacts")
        .join("benchmark-data")
        .join(filename)
}

/// Benchmark with a large real-world metadata file
fn bench_resolve_real_metadata(group_name: &str, filename: &str, c: &mut Criterion) {
    let mut group = c.benchmark_group(group_name);

    let metadata_path = benchmark_data_path(filename);

    let metadata_json_text = fs::read_to_string(&metadata_path)
        .unwrap_or_else(|error| panic!("Could not read file {metadata_path:?}: {error}"));
    let file_size = metadata_json_text.len();
    group.throughput(Throughput::Bytes(file_size as u64));

    // Benchmark end-to-end from_json_str
    group.bench_function("from_json_str", |b| {
        b.iter_batched(
            || metadata_json_text.clone(),
            |json_text| {
                open_dds::Metadata::from_json_str(&json_text).expect("from_json_str should succeed")
            },
            BatchSize::LargeInput,
        );
    });

    // Benchmark OpenDDS deserialization only (from pre-parsed JSON Value)
    group.bench_function("opendds_deserialize", |b| {
        b.iter_batched(
            || serde_json::from_str::<serde_json::Value>(&metadata_json_text).unwrap(),
            |json_value| {
                open_dds::Metadata::deserialize(json_value, jsonpath::JSONPath::new())
                    .expect("OpenDDS deserialization should succeed")
            },
            BatchSize::LargeInput,
        );
    });

    group.bench_function("metadata_accessor_creation", |b| {
        let json_value = serde_json::from_str::<serde_json::Value>(&metadata_json_text).unwrap();
        let metadata =
            open_dds::Metadata::deserialize(json_value, jsonpath::JSONPath::new()).unwrap();
        b.iter_batched(
            || metadata.clone(),
            open_dds::accessor::MetadataAccessor::new,
            BatchSize::LargeInput,
        );
    });

    group.bench_function("full_resolve", |b| {
        let (metadata, configuration, _) = load_metadata_from_path(&metadata_path);
        b.iter_batched(
            || (metadata.clone(), configuration.clone()),
            |(metadata, configuration)| {
                metadata_resolve::resolve(metadata, &configuration)
                    .expect("Metadata resolution should succeed")
            },
            BatchSize::LargeInput,
        );
    });

    group.finish();
}

fn bench_medium_metadata(c: &mut Criterion) {
    bench_resolve_real_metadata("medium_metadata", "medium-metadata.json", c);
}

fn bench_medium_no_graphql_metadata(c: &mut Criterion) {
    bench_resolve_real_metadata(
        "medium_no_graphql_metadata",
        "medium-no-graphql-metadata.json",
        c,
    );
}

criterion_group!(
    benches,
    bench_metadata_resolve,
    bench_metadata_resolve_stages,
    bench_parsing_vs_resolution,
    bench_detailed_stages,
    bench_medium_metadata,
    bench_medium_no_graphql_metadata
);
criterion_main!(benches);
