#[allow(dead_code)]
mod common;

#[test]
fn test_explain_introspection() {
    common::test_execute_explain("explain/metadata.json", "explain/introspection_query/");
}

#[test]
fn test_multi_root_field_queries() {
    common::test_execute_explain("explain/metadata.json", "explain/multi_root_field_queries/");
}

#[test]
fn test_field_with_remote_relationship() {
    common::test_execute_explain(
        "explain/metadata.json",
        "explain/field_with_remote_relationship/",
    );
}

#[test]
fn test_field_with_local_relationship() {
    common::test_execute_explain(
        "explain/metadata.json",
        "explain/field_with_local_relationship/",
    );
}

#[test]
fn test_field_with_multi_remote_relationship_subfields() {
    common::test_execute_explain(
        "explain/metadata.json",
        "explain/field_with_multi_remote_relationship_subfields/",
    );
}

#[test]
fn test_field_with_nested_remote_relationship_1() {
    common::test_execute_explain(
        "explain/metadata.json",
        "explain/field_with_nested_remote_relationship_1/",
    );
}

#[test]
fn test_field_with_nested_remote_relationship_2() {
    common::test_execute_explain(
        "explain/metadata.json",
        "explain/field_with_nested_remote_relationship_2/",
    );
}
