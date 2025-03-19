use std::io::Write;
use std::path::PathBuf;

use goldenfile::Mint;
use schemars::schema_for;

use plan_pushdown_types::Rel;

#[test]
fn test_json_schemas() {
    let test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");

    let mut mint = Mint::new(test_dir);

    test_json_schema(&mut mint, schema_for!(Rel), "rel.jsonschema");
}

fn test_json_schema(mint: &mut Mint, mut schema: schemars::schema::RootSchema, filename: &str) {
    let expected_path = PathBuf::from_iter(["json_schema", filename]);

    let mut expected = mint.new_goldenfile(expected_path).unwrap();

    schema.definitions.sort_keys();

    write!(
        expected,
        "{}",
        serde_json::to_string_pretty(&schema).unwrap()
    )
    .unwrap();
}
