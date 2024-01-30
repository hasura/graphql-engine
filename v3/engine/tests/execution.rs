mod common;

#[test]
fn test_model_select_one_simple_select() {
    let test_path_string = "execute/models/select_one/simple_select";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation_legacy(test_path_string, &[common_metadata_path_string]);
}

#[test]
fn test_model_select_one_with_type_permission() {
    let test_path_string = "execute/models/select_one/type_permission";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string]);
}

#[test]
fn test_model_select_one_simple_select_introspection() {
    let test_path_string = "execute/models/select_one/simple_select/introspection/introspection";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string]);
}

#[test]
fn test_model_select_one_filter() {
    let test_path_string = "execute/models/select_one/simple_select/filter";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation_legacy(test_path_string, &[common_metadata_path_string]);
}

#[test]
fn test_model_select_one_custom_scalar() {
    let test_path_string = "execute/models/select_one/custom_scalar";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation_legacy(test_path_string, &[common_metadata_path_string]);
}

// Select Many Tests
#[test]
fn test_model_select_many_simple_select() {
    let test_path_string = "execute/models/select_many/simple_select";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation_legacy(test_path_string, &[common_metadata_path_string]);
}

#[test]
fn test_model_select_many_simple_select_introspection() {
    let test_path_string = "execute/models/select_many/simple_select/introspection/introspection";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation_legacy(test_path_string, &[common_metadata_path_string]);
}

#[test]
fn test_model_select_many_simple_select_introspection_user_1() {
    let test_path_string =
        "execute/models/select_many/simple_select/introspection/introspection_user_1";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation_legacy(test_path_string, &[common_metadata_path_string]);
}

#[test]
fn test_model_select_many_simple_select_introspection_with_graphql_config() {
    let test_path_string =
        "execute/models/select_many/simple_select/introspection/with_graphql_config";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let metadata_graphql_json = "execute/models/select_many/common_metadata/graphql_config.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string, metadata_graphql_json],
    );
}

#[test]
fn test_model_select_many_filter() {
    let test_path_string = "execute/models/select_many/simple_select/filter";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation_legacy(test_path_string, &[common_metadata_path_string]);
}

// Order By Tests
#[test]
fn test_model_select_many_order_by() {
    let test_path_string = "execute/models/select_many/order_by";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation_legacy(test_path_string, &[common_metadata_path_string]);
}

#[test]
fn test_model_select_many_order_by_filter() {
    let test_path_string = "execute/models/select_many/order_by/filter";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation_legacy(test_path_string, &[common_metadata_path_string]);
}

#[test]
fn test_model_select_many_order_by_with_graphql_config() {
    let test_path_string = "execute/models/select_many/order_by/with_graphql_config";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let metadata_graphql_json = "execute/models/select_many/common_metadata/graphql_config.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string, metadata_graphql_json],
    );
}

// What is being tested? - We are testing the order_by sorts correctly when
// multiple columns are being ordered.
// We are sorting the `Tracks` models where we want `AlbumId` in ascending order
// and `TrackId` columns in descending order.
// We expect the results to be sorted by `AlbumId`, and then if there are
// multiple rows that have the same value in `AlbumId`, it will then order these
// rows by `TrackId`
#[test]
fn test_model_select_many_order_by_multiple_columns() {
    let test_path_string = "execute/models/select_many/order_by/multiple_columns";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string]);
}

// Type Permissions
#[test]
fn test_model_select_many_type_permission_order_by() {
    let test_path_string = "execute/models/select_many/type_permission/order_by";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string]);
}

// Relationships in order_by expressions
// What is being tested:
// 1. Object relationships in order_by expressions (Simple, Nested Object relationships). We also test multi column boolean expressions

#[test]
fn test_model_select_many_order_by_object_relationship_simple() {
    let test_path_string = "execute/models/select_many/order_by/relationships/object/simple";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/order_by/relationships/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
    );
}

#[test]
fn test_model_select_many_order_by_object_relationship_nested() {
    let test_path_string = "execute/models/select_many/order_by/relationships/object/nested";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/order_by/relationships/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
    );
}

#[test]
fn test_model_select_many_type_permission_where() {
    let test_path_string = "execute/models/select_many/type_permission/where";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string]);
}

// Where Tests
#[test]
fn test_model_select_many_where() {
    let test_path_string = "execute/models/select_many/where/simple";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string]);
}

#[test]
fn test_model_select_many_where_is_null() {
    let test_path_string = "execute/models/select_many/where/is_null";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation_legacy(test_path_string, &[common_metadata_path_string]);
}

#[test]
fn test_model_select_many_where_filter() {
    let test_path_string = "execute/models/select_many/where/filter";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation_legacy(test_path_string, &[common_metadata_path_string]);
}

#[test]
fn test_model_select_many_where_with_grapqhl_config() {
    let test_path_string = "execute/models/select_many/where/with_graphql_config";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let metadata_graphql_json = "execute/models/select_many/common_metadata/graphql_config.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string, metadata_graphql_json],
    );
}

#[test]
fn test_model_select_many_select_with_args() {
    let test_path_string = "execute/models/select_many/select_with_args";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation_legacy(test_path_string, &[common_metadata_path_string]);
}

#[test]
fn test_model_select_many_select_with_args_filter() {
    let test_path_string = "execute/models/select_many/select_with_args/filter";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation_legacy(test_path_string, &[common_metadata_path_string]);
}

#[test]
fn test_model_select_many_select_with_args_with_graphql_config() {
    let test_path_string = "execute/models/select_many/select_with_args/with_graphql_config";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let metadata_graphql_json = "execute/models/select_many/common_metadata/graphql_config.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string, metadata_graphql_json],
    );
}

#[test]
fn test_model_select_many_where_ndc_operators() {
    let test_path_string = "execute/models/select_many/where/ndc_operators";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string]);
}

// Relationships in boolean expressions
// What is being tested:
// 1. Array relationships in boolean expressions (Simple, Nested array relationships). We also test multi column boolean expressions

#[test]
fn test_model_select_many_where_array_relationship_simple() {
    let test_path_string = "execute/models/select_many/where/relationships/array/simple";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/where/relationships/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
    );
}

#[test]
fn test_model_select_many_where_array_relationship_nested() {
    let test_path_string = "execute/models/select_many/where/relationships/array/nested";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/where/relationships/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
    );
}

// Object relationships in boolean expressions (Simple, Nested object relationships). We also test multi column boolean expressions
#[test]
fn test_model_select_many_where_object_relationship_simple() {
    let test_path_string = "execute/models/select_many/where/relationships/object/simple";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/where/relationships/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
    );
}

#[test]
fn test_model_select_many_where_object_relationship_nested() {
    let test_path_string = "execute/models/select_many/where/relationships/object/nested";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/where/relationships/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
    );
}

#[test]
fn test_model_select_many_object_type_input_arguments() {
    let test_path_string = "execute/models/select_many/object_type_input_arguments";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
    common::test_execution_expectation_legacy(test_path_string, &[common_metadata_path_string]);
}

// Limit Tests
#[test]
fn test_model_select_many_limit() {
    let test_path_string = "execute/models/select_many/limit_offset/limit";
    let ndc_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let common_metadata_path_string =
        "execute/models/select_many/limit_offset/common_metadata/metadata.json";

    common::test_execution_expectation(
        test_path_string,
        &[ndc_metadata_path_string, common_metadata_path_string],
    );
}

// Test is_null in model select permissions
#[test]
fn test_model_select_many_predicate_is_null() {
    let test_path_string = "execute/models/select_many/predicate/is_null";
    let ndc_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";

    common::test_execution_expectation(test_path_string, &[ndc_metadata_path_string]);
}

// ---------- Offset Tests
#[test]
fn test_model_select_many_offset() {
    let test_path_string = "execute/models/select_many/limit_offset/offset";
    let ndc_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let common_metadata_path_string =
        "execute/models/select_many/limit_offset/common_metadata/metadata.json";

    common::test_execution_expectation(
        test_path_string,
        &[ndc_metadata_path_string, common_metadata_path_string],
    );
}

// ---------- Limit and Offset Tests
#[test]
fn test_model_select_many_limit_offset() {
    let test_path_string = "execute/models/select_many/limit_offset/limit_offset";
    let ndc_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let common_metadata_path_string =
        "execute/models/select_many/limit_offset/common_metadata/metadata.json";

    common::test_execution_expectation(
        test_path_string,
        &[ndc_metadata_path_string, common_metadata_path_string],
    );
}

#[test]
fn test_relay() {
    let test_path_string = "execute/relay/relay";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string]);
}

#[test]
/// Tests the generation of the `id` field in select queries including relationships.
fn test_relay_id_in_select() {
    let test_path_string = "execute/relay/relay_id_in_select";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string]);
}

#[test]
/// Test querying the relay global ID with a role that doesn't have access to
/// all the global ID fields.
fn test_relay_global_id_permission() {
    let test_path_string = "execute/relay/relay_global_id_permission";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string]);
}

#[test]
fn test_relay_node_field() {
    let test_path_string = "execute/relay/relay_node_field";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let common_articles_metadata_path_string = "execute/relay/article_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            common_articles_metadata_path_string,
        ],
    );
}

#[test]
fn test_relay_node_type_permissions() {
    let test_path_string = "execute/relay/relay_node_type_permissions";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string]);
}

#[test]
fn test_relay_node_field_permission() {
    let test_path_string = "execute/relay/relay_node_field_permissions";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string]);
}

#[test]
/// Tests a role should not be able to access the relay `node` field,
/// if the Node interface doesn't implement any objects for that role.
fn test_relay_node_interface_permissions() {
    let test_path_string = "execute/relay/relay_node_interface_permission";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string]);
}

#[test]
/// Tests the `node` root field with a role that has no model select permissions.
fn test_relay_node_model_select_permissions_with_role_without_model_select_permission() {
    let test_path_string =
        "execute/relay/relay_node_model_select_permissions/no_select_permission_exists_for_role";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string]);
}

#[test]
/// Test the `node` root field with a role that has model select permissions.
fn test_relay_node_model_select_permissions() {
    let test_path_string =
        "execute/relay/relay_node_model_select_permissions/successful_model_select_permissions";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string]);
}

#[test]
fn test_typename() {
    let test_path_string = "execute/typename";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation_legacy(test_path_string, &[common_metadata_path_string]);
}

// Command Functions

#[test]
fn test_command_functions() {
    let test_path_string = "execute/commands/functions";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";
    common::test_execution_expectation_legacy(
        test_path_string,
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    );
}

#[test]
fn test_command_object_type_input_arguments() {
    let test_path_string = "execute/commands/object_type_input_arguments";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";
    common::test_execution_expectation_legacy(
        test_path_string,
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    );
}

#[test]
fn test_command_custom_scalar_inputs() {
    let test_path_string = "execute/commands/custom_scalar_inputs";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";
    common::test_execution_expectation_legacy(
        test_path_string,
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    );
}

// Tests a query command with scalar (Int) output type (different command permissions for roles: admin, user_1, user_2)
#[test]
fn test_command_functions_scalar_output_type() {
    let test_path_string = "execute/commands/functions/scalar_output_type";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    );
}

// Tests a query command with object (commandActor) output type (different command permissions for roles: admin, user_1, user_2)
#[test]
fn test_command_functions_object_output_type_command_permissions() {
    let test_path_string = "execute/commands/functions/object_output_type/command_permissions";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    );
}

// Tests a query command with object (commandActor) output type (different object output subset field permissions for roles: admin,
// user_1, user_2)
#[test]
fn test_command_functions_object_output_type_output_permissions() {
    let test_path_string = "execute/commands/functions/object_output_type/output_permissions";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    );
}

// Tests a query command with array of scalar ([String]) output type (different command permissions for roles: admin, user_1, user_2)
#[test]
fn test_command_functions_scalar_array_output_type() {
    let test_path_string = "execute/commands/functions/scalar_array_output_type";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    );
}

// Tests a query command with array of object ([commandActor]) output type (different command permissions for roles: admin, user_1, user_2)
#[test]
fn test_command_functions_object_array_output_type_command_permissions() {
    let test_path_string =
        "execute/commands/functions/object_array_output_type/command_permissions";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    );
}

// Tests a query command with array of object ([commandActor]) output type (different object output subset field permissions for roles: admin,
// user_1, user_2)
#[test]
fn test_command_functions_object_array_output_type_output_permissions() {
    let test_path_string = "execute/commands/functions/object_array_output_type/output_permissions";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    );
}

// Tests a query command with multiple arguments:
//  arguments: 2 arguments (taken as bounds and return the list of commandActors with movie_id between the bounds)
//  output: array of object ([commandActor]) output type
//  permission: different command permissions for roles: admin, user_1, user_2
#[test]
fn test_command_functions_multiple_arguments() {
    let test_path_string = "execute/commands/functions/multiple_arguments";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    );
}

// Command Procedures

// Tests a mutation command with scalar (String) output type (different command permissions for roles: admin, user_1,
// user_2). This mutation doesn't perform any mutation on the database, it just returns a string
#[test]
fn test_command_procedures_scalar_output_type_command_permissions() {
    let test_path_string = "execute/commands/procedures/scalar_output_type";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    );
}

// Tests a mutation command with object (commandActor) output type (different command permissions for roles: admin, user_1, user_2)
#[test]
fn test_command_procedures_object_output_type_command_permissions() {
    let test_path_string = "execute/commands/procedures/object_output_type/command_permissions";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    );
}

// Tests a mutation command with object (commandActor) output type (different object output subset field permissions for roles: admin,
// user_1, user_2)
#[test]
fn test_command_procedures_object_output_type_output_permissions() {
    let test_path_string = "execute/commands/procedures/object_output_type/output_permissions";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    );
}

// Tests a mutation command with array of scalar ([String]) output type (different command permissions for roles: admin, user_1, user_2)
#[test]
fn test_command_procedures_scalar_array_output_type() {
    let test_path_string = "execute/commands/procedures/scalar_array_output_type";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    );
}

// Tests a mutation command with array of object ([commandActor]) output type (different command permissions for roles: admin, user_1, user_2)
#[test]
fn test_command_procedures_object_array_output_type_command_permissions() {
    let test_path_string =
        "execute/commands/procedures/object_array_output_type/command_permissions";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    );
}

// Tests a mutation command with array of object ([commandActor]) output type (different object output subset field permissions for roles: admin,
// user_1, user_2)
#[test]
fn test_command_procedures_object_array_output_type_output_permissions() {
    let test_path_string =
        "execute/commands/procedures/object_array_output_type/output_permissions";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    );
}

// Tests a mutation command with multiple arguments:
// arguments: 2 arguments (taken as id and new name for an actor and returns the updated commandActor row )
// output: object (commandActor) output type
// permission: different command permissions for roles: admin, user_1, user_2
#[test]
fn test_command_procedures_multiple_arguments() {
    let test_path_string = "execute/commands/procedures/multiple_arguments";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    );
}

// Tests using relationships in predicates
// Array relationship
#[test]
fn test_model_select_many_relationship_predicate_array_simple() {
    let test_path_string = "execute/models/select_many/relationship_predicates/array/simple";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/relationship_predicates/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
    );
}

// Tests using relationships in predicates

// Nested Array relationship
#[test]
fn test_model_select_many_relationship_predicate_array_nested() {
    let test_path_string = "execute/models/select_many/relationship_predicates/array/nested";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/relationship_predicates/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
    );
}

// Tests using relationships in predicates
// Object relationship
#[test]
fn test_model_select_many_relationship_predicate_object_simple() {
    let test_path_string = "execute/models/select_many/relationship_predicates/object/simple";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/relationship_predicates/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
    );
}

// Tests using relationships in predicates
// Nested bject relationship
#[test]
fn test_model_select_many_relationship_predicate_object_nested() {
    let test_path_string = "execute/models/select_many/relationship_predicates/object/nested";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/relationship_predicates/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
    );
}

// Tests using relationships in predicates
// We have the following relationships:
//  1.  'Tracks' array relationship to 'Album' model
//  2.  'Album' object relationship to 'Track' model
//
// Predicates using the relationship are defined on both the models as follows:
// 1. The select permission for 'user' role on 'Album' model is defined as:
//      Select only those Album whose `TrackId` from the relationship `Track` is equal to "x-hasura-user-id"
// 2. The select permission for 'user' role on 'Track' model is defined as:
//      Select only those Track whose `Title` from the relationship `Album` is equal to "x-hasura-album-title"
//
// In this test, we test what happens when we query both the `Tracks` and `Album` relationship in the same query.
// The query we make is:
//   query MyQuery {
//      Album(limit: 1) {
//          Tracks {
//              TrackId
//              Name
//              Album {
//                  Title
//              }
//          }
//      }
//  }
// We expect the following results:
//      Fetch all the tracks of the Albums whose `TrackId` is equal to "x-hasura-user-id" and then
//      filter those tracks based on the "x-hasura-album-title" value.
#[test]
fn test_model_select_many_relationship_predicate_on_two_fields() {
    let test_path_string = "execute/models/select_many/relationship_predicates/on_two_fields";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/relationship_predicates/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
    );
}

// Tests using relationships in predicates
// We have the following relationships:
//  1.  'Tracks' object relationship to 'Album' model
//  2.  'Album' object relationship to 'Track' model
//  3.  'Genre' object relationship to 'Track' model
//
// We have the following select permission defined for "user" role
//    It filters only those Albums whose Tracks's Album's AlbumnId is equal to "x-hasura-user-id" and
//    whose Tracks's Genre's GenreId is equal to "x-hasura-genre-name"
#[test]
fn test_model_select_many_relationship_predicate_object_two_relationship_fields() {
    let test_path_string =
        "execute/models/select_many/relationship_predicates/object/two_relationship_fields";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/relationship_predicates/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
    );
}

#[test]
fn test_graphql_descriptions() {
    let test_path_string = "execute/description";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let common_custom_connector_path_string =
        "execute/common_metadata/custom_connector_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            common_custom_connector_path_string,
            common_command_metadata_path_string,
        ],
    );
}
