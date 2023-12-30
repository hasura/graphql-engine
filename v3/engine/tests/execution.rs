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
fn test_model_select_many_where_ndc_operators() {
    let test_path_string = "execute/models/select_many/where/ndc_operators";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string]);
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
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string]);
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
