#[allow(dead_code)]
mod common;

#[test]
fn test_explain_introspection() -> anyhow::Result<()> {
    common::test_execute_explain(
        "explain/introspection_query",
        "explain/introspection_query/metadata.json",
        &[],
    )
}

#[test]
fn test_multi_root_field_queries() -> anyhow::Result<()> {
    common::test_execute_explain(
        "explain/multi_root_field_queries",
        "execute/multiple_root_fields/successful_execution/metadata.json",
        &[],
    )
}

#[test]
fn test_field_with_remote_relationship() -> anyhow::Result<()> {
    common::test_execute_explain(
        "explain/field_with_remote_relationship",
        "execute/remote_relationships/array/metadata.json",
        &["execute/common_metadata/two_postgres_connector_schema.json"],
    )
}

#[test]
fn test_field_with_local_relationship() -> anyhow::Result<()> {
    common::test_execute_explain(
        "explain/field_with_local_relationship",
        "execute/relationships/array/metadata.json",
        &["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
    )
}

#[test]
fn test_field_with_multi_remote_relationship_subfields() -> anyhow::Result<()> {
    common::test_execute_explain(
        "explain/field_with_multi_remote_relationship_subfields",
        "explain/field_with_multi_remote_relationship_subfields/metadata.json",
        &["execute/common_metadata/two_postgres_connector_schema.json"],
    )
}

#[test]
fn test_field_with_nested_remote_relationship_1() -> anyhow::Result<()> {
    common::test_execute_explain(
        "explain/field_with_nested_remote_relationship_1",
        "explain/field_with_nested_remote_relationship_1/metadata.json",
        &["execute/common_metadata/two_postgres_connector_schema.json"],
    )
}

#[test]
fn test_field_with_nested_remote_relationship_2() -> anyhow::Result<()> {
    common::test_execute_explain(
        "explain/field_with_nested_remote_relationship_2",
        "explain/field_with_nested_remote_relationship_2/metadata.json",
        &["execute/common_metadata/two_postgres_connector_schema.json"],
    )
}

#[test]
fn test_remote_relationship_filter_object() -> anyhow::Result<()> {
    common::test_execute_explain(
        "explain/remote_relationship_filter/object",
        "explain/remote_relationship_filter/common_metadata.json",
        &["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
    )
}

#[test]
fn test_remote_relationship_filter_array() -> anyhow::Result<()> {
    common::test_execute_explain(
        "explain/remote_relationship_filter/array",
        "explain/remote_relationship_filter/common_metadata.json",
        &["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
    )
}
