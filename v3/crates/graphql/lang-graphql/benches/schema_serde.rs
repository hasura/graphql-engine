#![allow(clippy::cast_precision_loss)]

use human_bytes::human_bytes;
use lang_graphql::schema::{Schema, sdl};

use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};

fn generate_schema(type_count: usize) -> String {
    let mut schema_str = String::new();
    for type_i in 0..type_count {
        schema_str.push_str(&format!("type SampleType{type_i} {{\n"));
        for field_i in 0..20 {
            schema_str.push_str(&format!("    sampleField{field_i}: String\n"));
        }
        schema_str.push_str("}\n");
    }
    schema_str.push_str("type Query {\n");
    for type_i in 0..type_count {
        schema_str.push_str(&format!("   rootField{type_i}: SampleType{type_i}"));
    }
    schema_str.push('}');
    schema_str
}

#[allow(clippy::print_stdout)]
pub fn bench_serde(c: &mut Criterion) {
    let mut group = c.benchmark_group("schema_serde");
    for type_count in [100, 1000, 10000] {
        let schema_str = generate_schema(type_count);
        let schema = sdl::SDL::new(&schema_str)
            .and_then(|v| v.build_schema())
            .unwrap();

        // json
        {
            let serialized = serde_json::to_string(&schema).unwrap();
            let deserialized: Schema<sdl::SDL> = serde_json::from_str(&serialized).unwrap();
            assert_eq!(deserialized, schema);
            println!(
                "size of {} types serialized to json is {}",
                type_count,
                human_bytes(serialized.len() as f64)
            );
            group.bench_with_input(
                BenchmarkId::new("ser-json", type_count),
                &schema,
                |b, schema| b.iter(|| serde_json::to_string(schema)),
            );
            group.bench_with_input(
                BenchmarkId::new("de-json", type_count),
                &serialized,
                |b, serialized| b.iter(|| serde_json::from_str::<Schema<sdl::SDL>>(serialized)),
            );
        }
        // bson
        {
            let serialized = bson::to_vec(&schema).unwrap();
            let deserialized = bson::from_slice::<Schema<sdl::SDL>>(&serialized).unwrap();
            assert_eq!(deserialized, schema);
            println!(
                "size of {} types serialized to bson is {}",
                type_count,
                human_bytes(serialized.len() as f64)
            );
            group.bench_with_input(
                BenchmarkId::new("ser-bson", type_count),
                &schema,
                |b, schema| b.iter(|| bson::to_vec(schema)),
            );
            group.bench_with_input(
                BenchmarkId::new("de-bson", type_count),
                &serialized,
                |b, serialized| b.iter(|| bson::from_slice::<Schema<sdl::SDL>>(serialized)),
            );
        }
        // bincode
        {
            let serialized = bincode::serialize(&schema).unwrap();
            let deserialized =
                bincode::deserialize::<Schema<sdl::SDL>>(serialized.as_slice()).unwrap();
            assert_eq!(deserialized, schema);
            println!(
                "size of {} types serialized to bincode is {}",
                type_count,
                human_bytes(serialized.len() as f64)
            );
            group.bench_with_input(
                BenchmarkId::new("ser-bincode", type_count),
                &schema,
                |b, schema| b.iter(|| bincode::serialize(schema)),
            );
            group.bench_with_input(
                BenchmarkId::new("de-bincode", type_count),
                &serialized,
                |b, serialized| {
                    b.iter(|| bincode::deserialize::<Schema<sdl::SDL>>(serialized.as_slice()));
                },
            );
        }
        // msgpack
        {
            let serialized = rmp_serde::to_vec(&schema).unwrap();
            let deserialized: Schema<sdl::SDL> =
                rmp_serde::from_read(serialized.as_slice()).unwrap();
            assert_eq!(deserialized, schema);
            println!(
                "size of {} types serialized to msgpack is {}",
                type_count,
                human_bytes(serialized.len() as f64)
            );
            group.bench_with_input(
                BenchmarkId::new("ser-msgpack", type_count),
                &schema,
                |b, schema| b.iter(|| rmp_serde::to_vec(schema)),
            );
            group.bench_with_input(
                BenchmarkId::new("de-msgpack", type_count),
                &serialized,
                |b, serialized| {
                    b.iter(|| rmp_serde::from_read::<_, Schema<sdl::SDL>>(serialized.as_slice()));
                },
            );
        }
        // postcard
        {
            let serialized = postcard::to_stdvec(&schema).unwrap();
            let deserialized: Schema<sdl::SDL> =
                postcard::from_bytes(serialized.as_slice()).unwrap();
            assert_eq!(deserialized, schema);
            println!(
                "size of {} types serialized to postcard is {}",
                type_count,
                human_bytes(serialized.len() as f64)
            );
            group.bench_with_input(
                BenchmarkId::new("ser-postcard", type_count),
                &schema,
                |b, schema| b.iter(|| postcard::to_stdvec(schema)),
            );
            group.bench_with_input(
                BenchmarkId::new("de-postcard", type_count),
                &serialized,
                |b, serialized| {
                    b.iter(|| postcard::from_bytes::<Schema<sdl::SDL>>(serialized.as_slice()));
                },
            );
        }
    }
    group.finish();
}

criterion_group!(benches, bench_serde);
criterion_main!(benches);
