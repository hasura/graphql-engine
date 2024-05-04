// We are going to test the relationships. For each test, we are going to test against 3 roles:
// 1. admin
//    - has permission to select all fields
//    - has permission to select from all models and commands
// 2. user1
//    - has permission to select subset of fields
//    - has permission to select rows based on the following predicate:
//      - <some-id> == x-hasura-user-id
// 3. user2
//    - has permission to select subset of fields
//    - has permission to select rows based on the following predicate:
//      - and/or
//        - <some-id> == x-hasura-user-id
//        - <some-field> <<Some-operator>> <some-literal-value>
//

mod common;

// Non-remote relationships (relationship within the same connector)

#[test]
fn test_local_relationships_model_to_model_object() -> anyhow::Result<()> {
    let test_path_string = "execute/relationships/object";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_local_relationships_model_to_model_multi_mapping() -> anyhow::Result<()> {
    let test_path_string = "execute/relationships/multi_mapping";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_local_relationships_model_to_model_array() -> anyhow::Result<()> {
    let test_path_string = "execute/relationships/array";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let common_metadata_graphql_config =
        "execute/relationships/common_metadata/graphql_config.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string, common_metadata_graphql_config],
    )
}

#[test]
fn test_local_relationships_model_to_model_array_with_arguments() -> anyhow::Result<()> {
    let test_path_string = "execute/relationships/array/arguments";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_relationships_array_with_arguments_with_graphql_config() -> anyhow::Result<()> {
    let test_path_string = "execute/relationships/array/arguments/with_graphql_config";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let common_metadata_graphql_config =
        "execute/relationships/common_metadata/graphql_config.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string, common_metadata_graphql_config],
    )
}

#[test]
fn test_local_relationships_command_to_model() -> anyhow::Result<()> {
    let test_path_string = "execute/relationships/command_to_model";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_relationships_command_to_model_with_graphql_config() -> anyhow::Result<()> {
    let test_path_string = "execute/relationships/command_to_model/with_graphql_config";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
    let common_metadata_graphql_config =
        "execute/relationships/common_metadata/graphql_config.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string, common_metadata_graphql_config],
    )
}

#[test]
fn test_local_relationships_model_to_command() -> anyhow::Result<()> {
    let test_path_string = "execute/relationships/model_to_command";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    )
}

#[test]
fn test_local_relationships_command_to_command() -> anyhow::Result<()> {
    let test_path_string = "execute/relationships/command_to_command";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    )
}

#[test]
fn test_local_mutually_recursive_relationships_to_command() -> anyhow::Result<()> {
    let test_path_string = "execute/relationships/command_to_command/mutually_recursive";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    )
}

#[test]
fn test_local_relationships_permissions_target_model_filter_predicate() -> anyhow::Result<()> {
    let test_path_string = "execute/relationships/permissions/target_model_filter_predicate";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

// Remote Relationships tests

#[test]
fn test_relationships_model_to_model_across_namespace() -> anyhow::Result<()> {
    let test_path_string = "execute/relationships/across_namespace";
    let common_metadata_path_string =
        "execute/relationships/across_namespace/namespaced_connectors.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_relationships_command_to_model_across_namespace() -> anyhow::Result<()> {
    let test_path_string = "execute/relationships/command_to_model/across_namespace";
    let common_metadata_path_string =
        "execute/relationships/command_to_model/across_namespace/namespaced_connectors.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_relationships_command_to_command_across_namespace() -> anyhow::Result<()> {
    let test_path_string = "execute/relationships/command_to_command/across_namespace";
    let common_metadata_path_string =
        "execute/relationships/command_to_command/across_namespace/namespaced_connectors.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_remote_mutually_recursive_relationships_to_command_across_namespace() -> anyhow::Result<()>
{
    let test_path_string =
        "execute/relationships/command_to_command/mutually_recursive_across_namespace";
    let common_metadata_path_string =
        "execute/relationships/command_to_command/mutually_recursive_across_namespace/namespaced_connectors.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_remote_relationships_remote_object_in_local_array_1() -> anyhow::Result<()> {
    let test_path_string =
        "execute/remote_relationships/remote_in_local/local_array_remote_object/two_pg_ndc";
    let common_metadata_path_string = "execute/common_metadata/two_postgres_connector_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_remote_relationships_remote_object_in_local_array_2() -> anyhow::Result<()> {
    let test_path_string = "execute/remote_relationships/remote_in_local/local_array_remote_object/custom_ndc_and_pg_ndc";
    let common_metadata_path_string = "execute/common_metadata/two_connectors_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_remote_relationships_model_to_model_array() -> anyhow::Result<()> {
    let test_path_string = "execute/remote_relationships/array";
    let common_metadata_path_string = "execute/common_metadata/two_postgres_connector_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_remote_relationships_model_to_command_array() -> anyhow::Result<()> {
    let test_path_string = "execute/remote_relationships/command/model_to_command";
    let common_metadata_path_string = "execute/common_metadata/two_connectors_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_remote_mutually_recursive_relationships_model_to_command() -> anyhow::Result<()> {
    let test_path_string =
        "execute/remote_relationships/command/model_to_command/mutually_recursive";
    let common_metadata_path_string = "execute/common_metadata/two_connectors_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_remote_relationships_model_to_model_array_with_arguments() -> anyhow::Result<()> {
    let test_path_string = "execute/remote_relationships/array/arguments";
    let common_metadata_path_string = "execute/common_metadata/two_postgres_connector_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_remote_relationships_remote_in_local() -> anyhow::Result<()> {
    let test_path_string = "execute/remote_relationships/remote_in_local";
    let common_metadata_path_string = "execute/common_metadata/two_connectors_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_remote_relationships_from_nested() -> anyhow::Result<()> {
    let test_path_string = "execute/remote_relationships/from_nested";
    let common_metadata_path_string = "execute/common_metadata/two_connectors_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_remote_relationships_model_to_command_remote_in_local() -> anyhow::Result<()> {
    let test_path_string = "execute/remote_relationships/command/remote_in_local";
    let common_metadata_path_string = "execute/common_metadata/two_connectors_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_remote_relationships_mutually_recursive() -> anyhow::Result<()> {
    let test_path_string = "execute/remote_relationships/mutually_recursive";
    let common_metadata_path_string = "execute/common_metadata/two_connectors_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_remote_relationships_mutually_recursive_with_where() -> anyhow::Result<()> {
    let test_path_string = "execute/remote_relationships/mutually_recursive_with_where";
    let common_metadata_path_string = "execute/common_metadata/two_connectors_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_remote_relationships_multi_field_mapping() -> anyhow::Result<()> {
    let test_path_string = "execute/remote_relationships/multi_field_mapping";
    let common_metadata_path_string = "execute/common_metadata/two_connectors_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_relationships_permissions_target_model_type_permission() -> anyhow::Result<()> {
    let test_path_string = "execute/relationships/permissions/target_model_type_permission";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_relationships_permissions_source_type_permission() -> anyhow::Result<()> {
    let test_path_string = "execute/relationships/permissions/source_type_permission";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

// Miscellaneous tests

// What is being tested?
// object relationship with type permissions
// 1.ArticleByID is accessible (root level Model Select Permission), if this is not the case, error message would be different
// 2.author_id is not part of schema for role user -> we check the snapshot with the right error message
// TODO: This should ideally be a schema test
#[test]
fn test_relationships_permissions_target_model_type_field_not_selectable() -> anyhow::Result<()> {
    let test_path_string =
        "execute/relationships/permissions/target_model_type_field_not_selectable";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation_legacy(test_path_string, &[common_metadata_path_string])
}

// TODO: This should ideally be a schema test
#[test]
fn test_relationships_permissions_target_model_not_selectable() -> anyhow::Result<()> {
    let test_path_string = "execute/relationships/permissions/target_model_not_selectable";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation_legacy(test_path_string, &[common_metadata_path_string])
}

// What is being tested? - We are testing if unique names are being generated
// for the relationships A query whose fields have same relationship names work
// as expected.
// 1. There are two models `Articles` backed by `article` type and
//    `NewspaperArticle` backed by `newspaper_article` type. These types have a
//    object relationship with author, and both of them have the same
//    relationship name `Author`
// 2. There is a `Author` model backed by `author` type and has relationships
//    with `Articles` and `NewspaperArticle` model
//
// We test that, when the same relationship name is used by two different
// relationships for a model, everything works as expected.
// Ref Bug report: https://github.com/hasura/v3-engine/issues/168
#[test]
fn test_relationships_with_same_name() -> anyhow::Result<()> {
    let test_path_string = "execute/relationships/same_relationship_name";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation_legacy(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_relationship_with_no_relationship_capability() -> anyhow::Result<()> {
    let test_path_string: &str = "execute/relationships/no_relationship_capability";
    common::test_execution_expectation_legacy(test_path_string, &[])
}
