#!/usr/bin/env python3

import pytest

from conftest import extract_server_address_from
from context import PytestConf
from remote_server import NodeGraphQL
from validate import check_query_f

pytestmark = [
    pytest.mark.admin_secret,
    pytest.mark.hge_env('HASURA_GRAPHQL_ENABLE_REMOTE_SCHEMA_PERMISSIONS', 'true'),
]

@pytest.fixture(scope='class')
@pytest.mark.early
def graphql_service_1(worker_id: str, hge_fixture_env: dict[str, str]):
    (_, port) = extract_server_address_from('GRAPHQL_SERVICE_1')
    server = NodeGraphQL(worker_id, 'remote_schemas/nodejs/remote_schema_perms.js', port=port)
    server.start()
    print(f'{graphql_service_1.__name__} server started on {server.url}')
    hge_fixture_env['GRAPHQL_SERVICE_1'] = server.url
    yield server
    server.stop()

@pytest.fixture(scope='class')
@pytest.mark.early
def graphql_service_2(worker_id: str, hge_fixture_env: dict[str, str]):
    (_, port) = extract_server_address_from('GRAPHQL_SERVICE_2')
    server = NodeGraphQL(worker_id, 'remote_schemas/nodejs/secondary_remote_schema_perms.js', port=port)
    server.start()
    print(f'{graphql_service_2.__name__} server started on {server.url}')
    hge_fixture_env['GRAPHQL_SERVICE_2'] = server.url
    yield server
    server.stop()

@pytest.fixture(scope='class')
@pytest.mark.early
def graphql_service_3(worker_id: str, hge_fixture_env: dict[str, str]):
    (_, port) = extract_server_address_from('GRAPHQL_SERVICE_3')
    server = NodeGraphQL(worker_id, 'remote_schemas/nodejs/secondary_remote_schema_perms_error.js', port=port)
    server.start()
    print(f'{graphql_service_3.__name__} server started on {server.url}')
    hge_fixture_env['GRAPHQL_SERVICE_3'] = server.url
    yield server
    server.stop()

use_test_fixtures = pytest.mark.usefixtures(
    'graphql_service_1',
    'graphql_service_2',
    'graphql_service_3',
    'per_method_tests_db_state',
)

@use_test_fixtures
class TestAddRemoteSchemaPermissions:

    @classmethod
    def dir(cls):
        return "queries/remote_schemas/permissions/"

    def test_add_permission_with_valid_subset_of_fields(self, hge_ctx):
        hge_ctx.v1metadataq_f(self.dir() + 'add_permission_with_valid_subset_of_fields.yaml')

    """ Here the schemas are compatible """
    def test_update_remote_schema_details_with_permissions_set(self, hge_ctx):
        """ Permissions check """
        hge_ctx.v1metadataq_f(self.dir() + 'add_permission_with_valid_subset_of_fields.yaml')

        hge_ctx.v1metadataq_f(self.dir() + 'update_remote_schema/update_schema.yaml')
        """ check the details of remote schema in metadata """
        resp = hge_ctx.v1metadataq({"type": "export_metadata", "args": {}})
        assert resp['remote_schemas'][0]['definition']['url'] == "{{GRAPHQL_SERVICE_2}}"
        assert resp['remote_schemas'][0]['comment'] == 'this is from update query', resp
        assert resp['remote_schemas'][0]['definition']['timeout_seconds'] == 120, resp
        """ reset the changes to the original config """
        hge_ctx.v1metadataq_f(self.dir() + 'update_remote_schema/revert_to_original_config.yaml')

    def test_update_remote_schema_details_with_permissions_set_with_error(self, hge_ctx):
        hge_ctx.v1metadataq_f(self.dir() + 'add_permission_with_valid_subset_of_fields.yaml')
        hge_ctx.v1metadataq_f(self.dir() + 'update_remote_schema/update_schema_error.yaml', expected_status_code = 400)

    def test_add_permission_with_valid_subset_of_arguments(self, hge_ctx):
        hge_ctx.v1metadataq_f(self.dir() + 'add_permission_with_valid_subset_of_arguments.yaml')

    def test_role_based_schema_enums_validation(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'role_based_schema_enum_validations.yaml')

    def test_role_based_schema_scalars_validation(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'role_based_schema_scalar_validation.yaml')

    def test_role_based_schema_interface_validation(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'role_based_schema_interface_validation.yaml')

    def test_role_based_schema_union_validation(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'role_based_schema_union_validation.yaml')

    def test_role_based_schema_input_object_validation(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'role_based_schema_input_object_validation.yaml')

    def test_role_based_schema_object_validation(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'role_based_schema_object_validation.yaml')

    def test_preset_directive_validation(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'argument_preset_validation.yaml')

@use_test_fixtures
class TestRemoteSchemaPermissionsExecution:

    @classmethod
    def dir(cls):
        return "queries/remote_schemas/permissions/"

    def test_execution_with_subset_of_fields_exposed_to_role(self, hge_ctx):
        hge_ctx.v1metadataq_f(self.dir() + 'add_permission_with_valid_subset_of_fields.yaml')
        check_query_f(hge_ctx, self.dir() + 'execution_with_partial_fields_exposed_to_role.yaml')

    @pytest.mark.skipif(not PytestConf.config.getoption('--redis-url'), reason="Must enable redis")
    def test_execution_with_subset_of_fields_exposed_to_role_with_caching(self, hge_ctx):
        hge_ctx.v1metadataq_f(self.dir() + 'add_permission_with_valid_subset_of_fields.yaml')
        check_query_f(hge_ctx, self.dir() + 'execution_with_partial_fields_exposed_to_role_cached.yaml')

    def test_execution_with_subset_of_arguments_exposed_to_role(self, hge_ctx):
        hge_ctx.v1metadataq_f(self.dir() + 'add_permission_with_valid_subset_of_arguments.yaml')
        check_query_f(hge_ctx, self.dir() + 'execution_with_partial_args_exposed_to_role.yaml')

    def test_execution_with_unknown_role(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'unknown_role_execution.yaml')

@use_test_fixtures
class TestCustomizedRemoteSchemaPermissionsExecution:

    @classmethod
    def dir(cls):
        return "queries/remote_schemas/permissions/schema_customization/"

    def test_execution_with_subset_of_fields_exposed_to_role(self, hge_ctx):
        hge_ctx.v1metadataq_f(self.dir() + 'add_permission_with_valid_subset_of_fields.yaml')
        check_query_f(hge_ctx, self.dir() + 'execution_with_partial_fields_exposed_to_role.yaml')

    def test_execution_with_subset_of_arguments_exposed_to_role(self, hge_ctx):
        hge_ctx.v1metadataq_f(self.dir() + 'add_permission_with_valid_subset_of_arguments.yaml')
        check_query_f(hge_ctx, self.dir() + 'execution_with_partial_args_exposed_to_role.yaml')

@use_test_fixtures
class TestRemoteSchemaPermissionsArgumentPresets:

    @classmethod
    def dir(cls):
        return "queries/remote_schemas/permissions/argument_presets/"

    def test_execution_with_static_argument_preset(self, hge_ctx):
        hge_ctx.v1metadataq_f(self.dir() + 'add_permission_with_static_preset_argument.yaml')
        check_query_f(hge_ctx, self.dir() + 'execution_with_static_preset_args.yaml')

    def test_execution_with_session_argument_preset(self, hge_ctx):
        hge_ctx.v1metadataq_f(self.dir() + 'add_permission_with_session_preset_argument.yaml')
        check_query_f(hge_ctx, self.dir() + 'execution_with_session_preset_args.yaml')

@pytest.mark.usefixtures(
    'graphql_service_1',
    'graphql_service_2',
    'graphql_service_3',
)
class TestRemoteRelationshipPermissions:

    @classmethod
    def dir(cls):
        return "queries/remote_schemas/permissions/remote_relationships/"

    @pytest.fixture(autouse=True)
    def transact(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_with_permissions.yaml')
        yield
        hge_ctx.v1q_f(self.dir() + 'teardown.yaml')

    def test_basic_relationship(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic.yaml')
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic_user.yaml')
        check_query_f(hge_ctx, self.dir() + 'basic_remote_relationship_without_remote_schema_permissions_configured.yaml')
        check_query_f(hge_ctx, self.dir() + 'basic_remote_relationship_with_remote_schema_permissions_configured.yaml')

    # Test queries that combine several remote relationships, nested in
    # different ways, variously filtering different bits using permissions.
    def test_complex_multiple_joins(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_multiple_remote_rel.yaml')
        check_query_f(hge_ctx, self.dir() + 'complex_multiple_joins.yaml')

    def test_remote_relationship_with_field_containing_preset_argument(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic.yaml')
        check_query_f(hge_ctx, self.dir() + 'derive_remote_relationship_with_joining_field_containing_preset.yaml')

    def test_partial_arguments_of_remote_relationship_from_preset(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_messages_single_field.yaml')
        check_query_f(hge_ctx, self.dir() + 'partial_arguments_from_preset.yaml')
