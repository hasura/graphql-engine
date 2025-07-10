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

use std::collections::BTreeMap;

use metadata_resolve::data_connectors::NdcVersion;

mod common;

// Non-remote relationships (relationship within the same connector)

#[test]
fn test_local_relationships_model_to_model_object() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/relationships/object",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_local_relationships_model_to_model_multi_mapping() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/relationships/multi_mapping",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_local_relationships_model_to_model_array() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/relationships/array",
        &["execute/relationships/common_metadata/graphql_config.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_local_relationships_model_to_model_array_with_arguments() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/relationships/array/arguments",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_relationships_array_with_arguments_with_graphql_config() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/relationships/array/arguments/with_graphql_config",
        &["execute/relationships/common_metadata/graphql_config.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_local_relationships_model_to_model_array_with_arguments_null_inputs() -> anyhow::Result<()>
{
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/relationships/array/arguments_null_inputs",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_relationships_array_with_arguments_null_inputs_with_graphql_config() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/relationships/array/arguments_null_inputs/with_graphql_config",
        &["execute/relationships/common_metadata/graphql_config.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_local_relationships_command_to_model() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/relationships/command_to_model",
        &[],
        BTreeMap::from([
            // This test can't use the old NDC v0.1.x connector, the embedded actors data has changed
            // (
            //     NdcVersion::V01,
            //     vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            // ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_relationships_command_to_model_with_graphql_config() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/relationships/command_to_model/with_graphql_config",
        &["execute/relationships/common_metadata/graphql_config.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_local_relationships_model_to_command() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/relationships/model_to_command",
        &["execute/common_metadata/command_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_local_relationships_command_to_command() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/relationships/command_to_command",
        &["execute/common_metadata/command_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_local_mutually_recursive_relationships_to_command() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/relationships/command_to_command/mutually_recursive",
        &["execute/common_metadata/command_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_local_relationships_permissions_target_model_filter_predicate() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/relationships/permissions/target_model_filter_predicate",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

// Remote Relationships tests

#[test]
fn test_relationships_model_to_model_across_namespace() -> anyhow::Result<()> {
    let test_path_string = "execute/relationships/across_namespace";
    common::test_execution_expectation(
        test_path_string,
        &["execute/common_metadata/two_postgres_connector_schema.json"],
    )
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
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/relationships/command_to_command/mutually_recursive_across_namespace",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec![
                    "execute/relationships/command_to_command/mutually_recursive_across_namespace/namespaced_connectors_v01.json",
                ],
            ),
            (
                NdcVersion::V02,
                vec![
                    "execute/relationships/command_to_command/mutually_recursive_across_namespace/namespaced_connectors_v02.json",
                ],
            ),
        ]),
    )
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
    common::test_execution_expectation(
        test_path_string,
        &[
            "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
            "execute/common_metadata/custom_connector_v02_schema.json",
        ],
    )
}

#[test]
fn test_remote_relationships_model_to_model_array() -> anyhow::Result<()> {
    let test_path_string = "execute/remote_relationships/array";
    let common_metadata_path_string = "execute/common_metadata/two_postgres_connector_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_remote_relationships_model_to_model_array_aggregate() -> anyhow::Result<()> {
    let test_path_string = "execute/remote_relationships/array/aggregate";
    let common_metadata_path_string = "execute/common_metadata/two_postgres_connector_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

// Test remote joins with a Procedure LHS. This exercises:
//  - join to a model (object relationship), with permissions
//  - join to a collection
//    - ...with a nested join
//
// (adapted from test_command_procedures_multiple_arguments and
// test_select_many_model_arguments_without_arguments_input_type)
#[test]
fn test_remote_join_procedure_to_model() -> anyhow::Result<()> {
    let test_path_string = "execute/remote_relationships/command/procedures";
    common::test_execution_expectation_for_multiple_ndc_versions(
        test_path_string,
        &[
            "execute/common_metadata/command_metadata.json",
            "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
        ],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_remote_relationships_model_to_command_array() -> anyhow::Result<()> {
    let test_path_string = "execute/remote_relationships/command/model_to_command";
    common::test_execution_expectation(
        test_path_string,
        &[
            "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
            "execute/common_metadata/custom_connector_v02_schema.json",
        ],
    )
}

#[test]
fn test_remote_relationships_model_to_command_with_relationship_argument() -> anyhow::Result<()> {
    let test_path_string =
        "execute/remote_relationships/command/model_to_command/with_relationship_argument";
    common::test_execution_expectation(
        test_path_string,
        &[
            "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
            "execute/common_metadata/custom_connector_v02_schema.json",
        ],
    )
}

#[test]
fn test_remote_relationships_model_to_multiple_commands_not_nested() -> anyhow::Result<()> {
    let test_path_string =
        "execute/remote_relationships/command/model_to_command/multiple_commands/not_nested";
    common::test_execution_expectation(
        test_path_string,
        &[
            "execute/remote_relationships/command/model_to_command/multiple_commands/metadata.json",
            "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
            "execute/common_metadata/custom_connector_v02_schema.json",
        ],
    )
}

#[test]
fn test_remote_relationships_model_to_multiple_commands_nested() -> anyhow::Result<()> {
    let test_path_string =
        "execute/remote_relationships/command/model_to_command/multiple_commands/nested";
    common::test_execution_expectation(
        test_path_string,
        &[
            "execute/remote_relationships/command/model_to_command/multiple_commands/metadata.json",
            "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
            "execute/common_metadata/custom_connector_v02_schema.json",
        ],
    )
}

#[test]
fn test_remote_relationships_model_to_multiple_commands_very_nested() -> anyhow::Result<()> {
    let test_path_string =
        "execute/remote_relationships/command/model_to_command/multiple_commands/very_nested";
    common::test_execution_expectation(
        test_path_string,
        &[
            "execute/remote_relationships/command/model_to_command/multiple_commands/metadata.json",
            "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
            "execute/common_metadata/custom_connector_v02_schema.json",
        ],
    )
}

#[test]
fn test_remote_mutually_recursive_relationships_model_to_command() -> anyhow::Result<()> {
    let test_path_string =
        "execute/remote_relationships/command/model_to_command/mutually_recursive";
    common::test_execution_expectation(
        test_path_string,
        &[
            "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
            "execute/common_metadata/custom_connector_v02_schema.json",
        ],
    )
}

#[test]
fn test_remote_relationships_command_join_with_object_value() -> anyhow::Result<()> {
    let test_path_string = "execute/remote_relationships/command/join_with_object_value";
    common::test_execution_expectation(
        test_path_string,
        &[
            "execute/common_metadata/custom_connector_v02_no_relationships_schema.json",
            "execute/common_metadata/custom_connector_v02_schema.json",
        ],
    )
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
    common::test_execution_expectation(
        test_path_string,
        &[
            "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
            "execute/common_metadata/custom_connector_v02_schema.json",
        ],
    )
}

#[test]
fn test_remote_relationships_from_nested() -> anyhow::Result<()> {
    let test_path_string = "execute/remote_relationships/from_nested";
    common::test_execution_expectation(
        test_path_string,
        &[
            "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
            "execute/common_metadata/custom_connector_v02_schema.json",
        ],
    )
}

#[test]
fn test_remote_relationships_model_to_command_remote_in_local() -> anyhow::Result<()> {
    let test_path_string = "execute/remote_relationships/command/remote_in_local";
    common::test_execution_expectation(
        test_path_string,
        &[
            "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
            "execute/common_metadata/custom_connector_v02_schema.json",
        ],
    )
}

#[test]
fn test_remote_relationships_mutually_recursive() -> anyhow::Result<()> {
    let test_path_string = "execute/remote_relationships/mutually_recursive";
    common::test_execution_expectation(
        test_path_string,
        &[
            "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
            "execute/common_metadata/custom_connector_v02_schema.json",
        ],
    )
}

#[test]
fn test_remote_relationships_mutually_recursive_with_where() -> anyhow::Result<()> {
    let test_path_string = "execute/remote_relationships/mutually_recursive_with_where";
    common::test_execution_expectation(
        test_path_string,
        &[
            "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
            "execute/common_metadata/custom_connector_v02_schema.json",
        ],
    )
}

#[test]
fn test_remote_relationships_multi_field_mapping() -> anyhow::Result<()> {
    let test_path_string = "execute/remote_relationships/multi_field_mapping";
    common::test_execution_expectation(
        test_path_string,
        &[
            "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
            "execute/common_metadata/custom_connector_v02_schema.json",
        ],
    )
}

#[test]
fn test_relationships_permissions_target_model_type_permission() -> anyhow::Result<()> {
    let test_path_string = "execute/relationships/permissions/target_model_type_permission";
    let common_metadata_path_string =
        "execute/common_metadata/postgres_connector_ndc_v01_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_relationships_permissions_source_type_permission() -> anyhow::Result<()> {
    let test_path_string = "execute/relationships/permissions/source_type_permission";
    let common_metadata_path_string =
        "execute/common_metadata/postgres_connector_ndc_v01_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_relationships_nested_selection() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/relationships/nested/selection",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_relationships_nested_selection_no_nested_capability() -> anyhow::Result<()> {
    let test_path_string = "execute/relationships/nested/selection_no_nested_capability";
    let common_metadata_path_string =
        "execute/common_metadata/custom_connector_v02_no_relationships_schema.json";
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
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/relationships/permissions/target_model_type_field_not_selectable",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

// What is being tested?
// Where and Order By on the target model fields
// Testing permissions behavior when using where and order_by clauses in array relationships:
// 1. With admin role: Full access to filter and order articles
// 2. With user1 role: Can only see own articles (author_id = user-id) with filtering/ordering
// 3. With user2 role: Fails because 'title' field is not in allowed fields
// 4. With user3 role: Fails because 'article_id' field is not in allowed fields for ordering
#[test]
fn test_relationship_permission_target_model_where_and_order_by() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/relationships/permissions/target_model_where_and_order_by",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

// TODO: This should ideally be a schema test
#[test]
fn test_relationships_permissions_target_model_not_selectable() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/relationships/permissions/target_model_not_selectable",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
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
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/relationships/same_relationship_name",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_relationship_with_no_relationship_capability() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/relationships/no_relationship_capability",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_remote_relationships_target_model_with_arguments() -> anyhow::Result<()> {
    let test_path_string = "execute/remote_relationships/target_model_with_arguments";
    common::test_execution_expectation(
        test_path_string,
        &["execute/common_metadata/custom_connector_v02_no_relationships_schema.json"],
    )
}

#[test]
fn test_remote_relationships_target_model_with_arguments_and_fields() -> anyhow::Result<()> {
    let test_path_string = "execute/remote_relationships/target_model_with_arguments_and_fields";
    common::test_execution_expectation(
        test_path_string,
        &["execute/common_metadata/custom_connector_v02_no_relationships_schema.json"],
    )
}
