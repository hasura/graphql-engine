use std::fs;
use std::path::PathBuf;

use graphql_schema::Error as SchemaError;
use graphql_schema::GDS;
use metadata_resolve::Error as ResolveError;
use metadata_resolve::{BooleanExpressionError, ModelGraphqlError};

#[test]
fn test_select_many_model_arguments_without_arguments_input_type() -> anyhow::Result<()> {
    let metadata = read_metadata(
        "validate_metadata_artifacts/metadata_with_model_arguments_without_arguments_input_type.json",
    )?;

    let gds = GDS::new_with_default_flags(metadata)?;

    assert_eq!(
        GDS::build_schema(&gds).unwrap_err().to_string(),
        "Cannot generate arguments for model Actors (in subgraph default) since argumentsInputType and it's corresponding graphql config argumentsInput isn't defined"
    );
    Ok(())
}

#[test]
fn test_duplicate_field_path_relationship_mappings() -> anyhow::Result<()> {
    let metadata = read_metadata(
        "validate_metadata_artifacts/metadata_with_duplicate_field_mappings_in_relationship.json",
    )?;

    let gds = GDS::new_with_default_flags(metadata);

    assert_eq!(
        gds.unwrap_err().to_string(),
        "metadata is not consistent: Mapping for source field movie_id already exists in the relationship Movies on type actor (in subgraph default)"
    );
    Ok(())
}

#[test]
fn test_relationship_mapping_unknown_source_field() -> anyhow::Result<()> {
    let metadata = read_metadata(
        "validate_metadata_artifacts/metadata_with_unknown_source_field_in_relationship_mapping.json",
    )?;

    let gds = GDS::new_with_default_flags(metadata);

    assert_eq!(
        gds.unwrap_err().to_string(),
        "metadata is not consistent: source field author_id_unknown_field in field mapping for relationship author on type Article (in subgraph default) is unknown."
    );
    Ok(())
}

#[test]
fn test_relationship_mapping_unknown_target_field() -> anyhow::Result<()> {
    let metadata = read_metadata(
        "validate_metadata_artifacts/metadata_with_unknown_target_field_in_relationship_mapping.json",
    )?;

    let gds = GDS::new_with_default_flags(metadata);

    assert_eq!(
        gds.unwrap_err().to_string(),
        "metadata is not consistent: target field id_unknown_field in field mapping for relationship author on type Article (in subgraph default) to model Authors (in subgraph default) is unknown."
    );
    Ok(())
}

#[test]
fn test_pre_namespace_aware_metadata() -> anyhow::Result<()> {
    let metadata =
        read_metadata("validate_metadata_artifacts/metadata_before_namespace_aware_open_dd.json")?;

    let gds = GDS::new_with_default_flags(metadata);

    gds?; // assert that it is OK
    Ok(())
}

#[test]
fn test_pre_subgraph_terminology_metadata() -> anyhow::Result<()> {
    let metadata = read_metadata(
        "validate_metadata_artifacts/metadata_before_subgraph_terminology_open_dd.json",
    )?;

    let gds = GDS::new_with_default_flags(metadata);

    gds?; // assert that it is OK
    Ok(())
}

#[test]
fn test_scalar_comparison_type_reuse() -> anyhow::Result<()> {
    let metadata = read_metadata(
        "validate_metadata_artifacts/metadata_with_scalar_comparison_type_reused.json",
    )?;

    let gds = GDS::new_with_default_flags(metadata)?;
    let built_schema = gds.build_schema();
    built_schema?; // assert that it is OK

    let metadata = read_metadata(
        "validate_metadata_artifacts/metadata_with_scalar_comparison_type_reused_for_different_scalars.json",
    )?;

    let gds = GDS::new_with_default_flags(metadata)?;
    let built_schema = gds.build_schema();
    assert_eq!(
        built_schema.unwrap_err().to_string(),
        "internal error while building schema: multiple definitions of graphql type: Int_Comparison_Exp"
    );
    Ok(())
}

// Test Global GraphQL Config

// Filter Expression (Where clause) Tests

// `filterExpressionType` is present in the `graphql` field of the model
// but `filterInput` is not present in the `GraphqlConfig` kind
#[test]
fn test_filter_error_filter_expression_type_present_filter_input_not_present() -> anyhow::Result<()>
{
    let metadata = read_metadata(
        "validate_metadata_artifacts/global_graphql_config/metadata_with_filter_expression_type_present_filter_input_not_present.json",
    )?;

    let gds = GDS::new_with_default_flags(metadata);

    assert_eq!(
        gds.unwrap_err().to_string(),
        "metadata is not consistent: the filterInput needs to be defined in GraphqlConfig, when models have filterExpressionType"
    );
    Ok(())
}

// Order By Tests

// `orderByExpressionType` is present in the `graphql` field of the model
// but `orderByInput` is not present in the `GraphqlConfig` kind
#[test]
fn test_order_by_error_order_by_expression_type_present_order_by_input_not_present()
-> anyhow::Result<()> {
    let metadata = read_metadata(
        "validate_metadata_artifacts/global_graphql_config/metadata_with_order_by_expression_type_present_order_by_input_not_present.json",
    )?;

    let gds = GDS::new_with_default_flags(metadata);

    assert_eq!(
        gds.unwrap_err().to_string(),
        "metadata is not consistent: the orderByInput needs to be defined in GraphqlConfig, when models have orderByExpressionType"
    );
    Ok(())
}

// `orderByExpressionType` is present in the `graphql` field of the model
// but `enumTypeNames` directions is other than `["asc", "desc"]` in the`orderByInput` in the `GraphqlConfig` kind
#[test]
fn test_order_by_error_order_by_expression_type_present_and_only_asc_in_enum_type_directions_in_order_by_input_present()
-> anyhow::Result<()> {
    let metadata = read_metadata(
        "validate_metadata_artifacts/global_graphql_config/metadata_with_order_by_expression_type_present_and_only_asc_in_enum_type_directions_in_order_by_input_present.json",
    )?;

    let gds = GDS::new_with_default_flags(metadata);

    assert_eq!(
        gds.unwrap_err().to_string(),
        "metadata is not consistent: invalid directions: Asc defined in orderByInput of GraphqlConfig , currently there is no support for partial directions. Please specify a type which has both 'asc' and 'desc' directions"
    );
    Ok(())
}

// Argument Tests

// `argumentsInputType` is present in the `graphql` field of the model
// but `argumentsInput` is not present in the `GraphqlConfig` kind
#[test]
fn test_arguments_error_arguments_input_type_present_arguments_input_not_present()
-> anyhow::Result<()> {
    let metadata = read_metadata(
        "validate_metadata_artifacts/global_graphql_config/metadata_with_arguments_input_type_present_arguments_input_not_present.json",
    )?;

    let gds = GDS::new_with_default_flags(metadata);

    assert_eq!(
        gds.unwrap_err().to_string(),
        "metadata is not consistent: the fieldName for argumentsInput needs to be defined in GraphqlConfig, when models have argumentsInputType"
    );
    Ok(())
}

// "require_graphql_config" flag test
// The `require_graphql_flag` is set to true but there is no `GraphqlConfig` object in metadata
#[test]
fn test_require_graphql_config_flag_no_graphql_config_present() -> anyhow::Result<()> {
    let metadata = read_metadata(
        "validate_metadata_artifacts/global_graphql_config/metadata_with_require_grapqhl_config_flag_graphql_config_not_present.json",
    )?;

    let gds = GDS::new_with_default_flags(metadata);

    assert_eq!(
        gds.unwrap_err().to_string(),
        "metadata is not consistent: graphql configuration is not defined in supergraph"
    );
    Ok(())
}

#[test]
fn test_global_if_fields_present_in_object_type_but_no_model_has_global_id_source_true_for_object_type()
-> anyhow::Result<()> {
    let metadata = read_metadata(
        "validate_metadata_artifacts/metadata_with_global_if_fields_present_in_object_type_but_no_model_has_global_id_source_true_for_object_type.json",
    )?;

    let gds = GDS::new_with_default_flags(metadata);

    assert_eq!(
        gds.unwrap_err().to_string(),
        "metadata is not consistent: 'globalIdFields' for type actor (in subgraph default) found, but no model found with 'globalIdSource: true' for type actor (in subgraph default)"
    );
    Ok(())
}

#[test]
// Remove this test (and add an execution test instead) if we ever support this use case
fn test_disallow_object_mapped_to_scalar() -> anyhow::Result<()> {
    let metadata = read_metadata(
        "validate_metadata_artifacts/metadata_with_opendd_object_mapped_to_ndc_scalar.json",
    )?;

    let gds = GDS::new_with_default_flags(metadata);

    assert!(
        matches!(
            gds,
            Err(SchemaError::ResolveError {
                error: metadata_resolve::WithContext::Raw(ResolveError::ModelsError(
                    metadata_resolve::ModelsError::ModelTypeMappingCollectionError { .. }
                ))
            })
        ),
        "actual: {gds:?}"
    );
    Ok(())
}

#[test]
fn test_disallow_filter_expression_without_source() -> anyhow::Result<()> {
    let metadata = read_metadata(
        "validate_metadata_artifacts/boolean_expressions/filter_expression_without_source.json",
    )?;

    let gds = GDS::new_with_default_flags(metadata);

    assert!(
        matches!(
            gds,
            Err(SchemaError::ResolveError {
                error: metadata_resolve::WithContext::Raw(ResolveError::ModelGraphqlError(
                    ModelGraphqlError::BooleanExpressionError(
                        BooleanExpressionError::CannotUseFilterExpressionsWithoutSource { .. }
                    )
                ))
            })
        ),
        "actual: {gds:?}"
    );
    Ok(())
}

#[test]
fn test_disallow_filter_expression_with_object_type_mismatch() -> anyhow::Result<()> {
    let metadata = read_metadata(
        "validate_metadata_artifacts/boolean_expressions/filter_expression_with_object_type_mismatch.json",
    )?;

    let gds = GDS::new_with_default_flags(metadata);

    assert!(
        matches!(
            gds,
            Err(SchemaError::ResolveError {
                error: metadata_resolve::WithContext::Raw(ResolveError::ModelGraphqlError(
                    ModelGraphqlError::BooleanExpressionError(
                        BooleanExpressionError::BooleanExpressionTypeForInvalidObjectTypeInModel { .. }
                    )
                ))
            })
        ),
        "actual: {gds:?}"
    );
    Ok(())
}

#[test]
fn test_disallow_boolean_expression_without_mapping() -> anyhow::Result<()> {
    let metadata = read_metadata(
        "validate_metadata_artifacts/boolean_expressions/boolean_expression_without_mapping.json",
    )?;

    let gds = GDS::new_with_default_flags(metadata);
    assert!(
        matches!(
            gds,
            Err(SchemaError::ResolveError {
                error: metadata_resolve::WithContext::Raw(
                           ResolveError::BooleanExpressionError (BooleanExpressionError::NoDataConnectorTypeMappingForObjectTypeInBooleanExpression { .. }))
            })
        ),
        "actual: {gds:?}"
    );
    Ok(())
}

#[test]
fn test_disallow_conflicting_object_field_name_and_relationship_name() -> anyhow::Result<()> {
    let metadata = read_metadata(
        "validate_metadata_artifacts/relationships/conflicting_object_field_name_and_relationship_name.json",
    )?;

    let gds = GDS::new_with_default_flags(metadata)?;
    let schema = gds.build_schema();

    assert!(
        matches!(
            schema,
            Err(SchemaError::RelationshipFieldNameConflict { .. })
        ),
        "actual: {schema:?}"
    );
    Ok(())
}

#[test]
fn test_allow_metadata_with_deprecated_field() -> anyhow::Result<()> {
    // {"kind": "Relationship", "source": "<source>"} is deprecated in favor of "sourceType"
    let metadata =
        read_metadata("validate_metadata_artifacts/metadata_with_deprecated_field.json")?;

    let gds = GDS::new_with_default_flags(metadata);

    gds?; // assert that it is OK
    Ok(())
}

fn read_metadata(path: &str) -> anyhow::Result<open_dds::Metadata> {
    let json = read_file(path)?;
    let value = open_dds::Metadata::from_json_str(&json)?;
    Ok(value)
}

fn read_file(path: &str) -> anyhow::Result<String> {
    let test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");
    let schema = fs::read_to_string(test_dir.join(path))?;
    Ok(schema)
}
