#[allow(dead_code)]
mod common;

#[test]
fn test_explain_introspection() {
    common::test_execute_explain(
        "explain/introspection_query/",
        "explain/introspection_query/metadata.json",
        &[],
    );
}

#[test]
fn test_multi_root_field_queries() {
    common::test_execute_explain(
        "explain/multi_root_field_queries/",
        "execute/multiple_root_fields/successful_execution/metadata.json",
        &[],
    );
}

#[test]
fn test_field_with_remote_relationship() {
    common::test_execute_explain(
        "explain/field_with_remote_relationship/",
        "execute/remote_relationships/array/metadata.json",
        &["execute/common_metadata/two_postgres_connector_schema.json"],
    );
}

#[test]
fn test_field_with_local_relationship() {
    common::test_execute_explain(
        "explain/field_with_local_relationship/",
        "execute/relationships/array/metadata.json",
        &["execute/common_metadata/postgres_connector_schema.json"],
    );
}

#[test]
fn test_field_with_multi_remote_relationship_subfields() {
    common::test_execute_explain(
        "explain/field_with_multi_remote_relationship_subfields/",
        "explain/field_with_multi_remote_relationship_subfields/metadata.json",
        &["execute/common_metadata/two_postgres_connector_schema.json"],
    );
}

#[test]
fn test_field_with_nested_remote_relationship_1() {
    common::test_execute_explain(
        "explain/field_with_nested_remote_relationship_1/",
        "explain/field_with_nested_remote_relationship_1/metadata.json",
        &["execute/common_metadata/two_postgres_connector_schema.json"],
    );
}

#[test]
fn test_field_with_nested_remote_relationship_2() {
    common::test_execute_explain(
        "explain/field_with_nested_remote_relationship_2/",
        "explain/field_with_nested_remote_relationship_2/metadata.json",
        &["execute/common_metadata/two_postgres_connector_schema.json"],
    );
}
