use engine::schema::GDS;
use std::{fs, path::PathBuf};

#[test]
fn test_select_many_model_arguments_without_arguments_input_type() {
    let test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");

    let schema = fs::read_to_string(
        test_dir.join("validate_metadata_artifacts/metadata_with_model_arguments_without_arguments_input_type.json"),
    )
        .unwrap();

    let gds = GDS::new(serde_json::from_str(&schema).unwrap()).unwrap();
    assert_eq!(
        GDS::build_schema(&gds).unwrap_err().to_string(),
        "Cannot generate arguments for model Actors (in subgraph default) since argumentsInputType isn't defined"
    );
}

#[test]
fn test_duplicate_field_path_relationship_mappings() {
    let test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");

    let schema = fs::read_to_string(test_dir.join(
        "validate_metadata_artifacts/metadata_with_duplicate_field_mappings_in_relationship.json",
    ))
    .unwrap();

    assert_eq!(
        GDS::new(serde_json::from_str(&schema).unwrap()).unwrap_err().to_string(),
        "metadata is not consistent: Mapping for source field movie_id already exists in the relationship Movies on type actor (in subgraph default)"
    );
}

#[test]
fn test_field_path_to_argument_relationship_mapping() {
    let test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");

    let schema = fs::read_to_string(
        test_dir.join("validate_metadata_artifacts/metadata_with_field_path_to_argument_relationship_mapping.json"),
    )
        .unwrap();

    assert_eq!(
        GDS::new(serde_json::from_str(&schema).unwrap()).unwrap_err().to_string(),
        "metadata is not consistent: Relationship mappings to model arguments expressions are not supported yet."
    );
}

#[test]
fn test_relationship_mapping_unknown_source_field() {
    let schema = fs::read_to_string(
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/validate_metadata_artifacts/metadata_with_unknown_source_field_in_relationship_mapping.json"),
    )
        .unwrap();

    assert_eq!(
        GDS::new(serde_json::from_str(&schema).unwrap()).unwrap_err().to_string(),
        "metadata is not consistent: source field author_id_unknown_field in field mapping for relationship author on type Article (in subgraph default) is unknown."
    );
}

#[test]
fn test_relationship_mapping_unknown_target_field() {
    let schema = fs::read_to_string(
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/validate_metadata_artifacts/metadata_with_unknown_target_field_in_relationship_mapping.json"),
    )
        .unwrap();

    assert_eq!(
        GDS::new(serde_json::from_str(&schema).unwrap()).unwrap_err().to_string(),
        "metadata is not consistent: target field author_id in field mapping for relationship author on type Article (in subgraph default) to model Authors (in subgraph default) is unknown."
    );
}

#[test]
fn test_pre_namespace_aware_metadata() {
    let schema = fs::read_to_string(
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("tests/validate_metadata_artifacts/metadata_before_namespace_aware_open_dd.json"),
    )
    .unwrap();
    let gds = GDS::new(serde_json::from_str(&schema).unwrap());
    assert!(gds.is_ok())
}

#[test]
fn test_pre_subgraph_terminology_metadata() {
    let schema = fs::read_to_string(PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(
        "tests/validate_metadata_artifacts/metadata_before_subgraph_terminology_open_dd.json",
    ))
    .unwrap();
    let gds = GDS::new(serde_json::from_str(&schema).unwrap());
    assert!(gds.is_ok())
}

#[test]
fn test_scalar_comparison_type_reuse() {
    let schema = fs::read_to_string(PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(
        "tests/validate_metadata_artifacts/metadata_with_scalar_comparison_type_reused.json",
    ))
    .unwrap();
    let gds = GDS::new(serde_json::from_str(&schema).unwrap()).unwrap();
    let built_schema = gds.build_schema();
    assert!(built_schema.is_ok());

    let schema = fs::read_to_string(PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(
        "tests/validate_metadata_artifacts/metadata_with_scalar_comparison_type_reused_for_different_scalars.json",
    ))
    .unwrap();
    let gds = GDS::new(serde_json::from_str(&schema).unwrap()).unwrap();
    let built_schema = gds.build_schema();
    assert_eq!(built_schema.unwrap_err().to_string(),
        "internal error while building schema: multiple definitions of graphql type: Int_Comparison_Exp"
    );
}

#[test]
fn test_global_if_fields_present_in_object_type_but_no_model_has_global_id_source_true_for_object_type(
) {
    let test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");

    let schema = fs::read_to_string(
        test_dir.join("validate_metadata_artifacts/metadata_with_global_if_fields_present_in_object_type_but_no_model_has_global_id_source_true_for_object_type.json"),
    )
        .unwrap();

    assert_eq!(
        GDS::new(serde_json::from_str(&schema).unwrap()).unwrap_err().to_string(),
        "metadata is not consistent: 'globalIdFields' for type actor (in subgraph default) found, but no model found with 'globalIdSource: true' for type actor (in subgraph default)"
    );
}
