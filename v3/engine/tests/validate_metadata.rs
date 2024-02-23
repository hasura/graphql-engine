use engine::metadata::resolved::error::Error::ModelTypeMappingValidationError;
use engine::schema::Error::ResolveError;
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
        "Cannot generate arguments for model Actors (in subgraph default) since argumentsInputType and it's corresponding graphql config argumentsInput isn't defined"
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

// Test Global GraphQL Config

// Filter Expression (Where clause) Tests

// `filterExpressionType` is present in the `graphql` field of the model
// but `filterInput` is not present in the `GraphqlConfig` kind
#[test]
fn test_filter_error_filter_expression_type_present_filter_input_not_present() {
    let test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");

    let schema = fs::read_to_string(
        test_dir.join("validate_metadata_artifacts/global_graphql_config/metadata_with_filter_expression_type_present_filter_input_not_present.json"),
    )
        .unwrap();

    assert_eq!(
        GDS::new(serde_json::from_str(&schema).unwrap()).unwrap_err().to_string(),
        "metadata is not consistent: the filterInput need to be defined in GraphqlConfig, when models have filterExpressionType"
    );
}

// Order By Tests

// `orderByExpressionType` is present in the `graphql` field of the model
// but `orderByInput` is not present in the `GraphqlConfig` kind
#[test]
fn test_order_by_error_order_by_expression_type_present_order_by_input_not_present() {
    let test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");

    let schema = fs::read_to_string(
        test_dir.join("validate_metadata_artifacts/global_graphql_config/metadata_with_order_by_expression_type_present_order_by_input_not_present.json"),
    )
        .unwrap();

    assert_eq!(
        GDS::new(serde_json::from_str(&schema).unwrap()).unwrap_err().to_string(),
        "metadata is not consistent: the orderByInput need to be defined in GraphqlConfig, when models have orderByExpressionType"
    );
}

// `orderByExpressionType` is present in the `graphql` field of the model
// but `enumTypeNames` directions is other than `["asc", "desc"]` in the`orderByInput` in the `GraphqlConfig` kind
#[test]
fn test_order_by_error_order_by_expression_type_present_and_only_asc_in_enum_type_directions_in_order_by_input_present(
) {
    let test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");

    let schema = fs::read_to_string(
        test_dir.join("validate_metadata_artifacts/global_graphql_config/metadata_with_order_by_expression_type_present_and_only_asc_in_enum_type_directions_in_order_by_input_present.json"),
    )
        .unwrap();

    assert_eq!(
        GDS::new(serde_json::from_str(&schema).unwrap()).unwrap_err().to_string(),
        "metadata is not consistent: invalid directions: Asc defined in orderByInput of GraphqlConfig , currenlty there is no support for partial directions. Please specify a type which has both 'asc' and 'desc' directions"
    );
}

// Argument Tests

// `argumentsInputType` is present in the `graphql` field of the model
// but `argumentsInput` is not present in the `GraphqlConfig` kind
#[test]
fn test_arguments_error_arguments_input_type_present_arguments_input_not_present() {
    let test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");

    let schema = fs::read_to_string(
        test_dir.join("validate_metadata_artifacts/global_graphql_config/metadata_with_arguments_input_type_present_arguments_input_not_present.json"),
    )
        .unwrap();

    assert_eq!(
        GDS::new(serde_json::from_str(&schema).unwrap()).unwrap_err().to_string(),
        "metadata is not consistent: the fieldName for argumentsInput need to be defined in GraphqlConfig, when models have argumentsInputType"
    );
}

// "require_graphql_config" flag test
// The `require_graphql_flag` is set to true but there is no `GraphqlConfig` object in metadata
#[test]
fn test_require_graphql_config_flag_no_graphql_config_present() {
    let test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");

    let schema = fs::read_to_string(
        test_dir.join("validate_metadata_artifacts/global_graphql_config/metadata_with_require_grapqhl_config_flag_graphql_config_not_present.json"),
    )
        .unwrap();

    assert_eq!(
        GDS::new(serde_json::from_str(&schema).unwrap())
            .unwrap_err()
            .to_string(),
        "metadata is not consistent: graphql configuration is not defined in supergraph"
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

#[test]
// Remove this test (and add an execution test instead) if we ever support this use case
fn test_disallow_object_mapped_to_scalar() {
    let metadata_path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(
        "tests/validate_metadata_artifacts/metadata_with_opendd_object_mapped_to_ndc_scalar.json",
    );
    let metadata =
        GDS::new(serde_json::from_str(&fs::read_to_string(metadata_path).unwrap()).unwrap());
    println!("{metadata:?}");
    assert!(matches!(
        metadata,
        Err(ResolveError {
            error: ModelTypeMappingValidationError { .. }
        })
    ));
}
