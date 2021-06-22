#!/usr/bin/env python3

import pytest
import subprocess
import time

from validate import check_query_f
from remote_server import NodeGraphQL
from context import PytestConf

if not PytestConf.config.getoption('--enable-remote-schema-permissions'):
    pytest.skip('--enable-remote-schema-permissions is missing, skipping remote schema permissions tests', allow_module_level=True)

@pytest.fixture(scope="module")
def graphql_service():
    svc = NodeGraphQL(["node", "remote_schemas/nodejs/remote_schema_perms.js"])
    svc.start()
    yield svc
    svc.stop()

@pytest.fixture(scope="module")
def graphql_service_2():
    svc = NodeGraphQL(["node", "remote_schemas/nodejs/secondary_remote_schema_perms.js"])
    svc.start()
    yield svc
    svc.stop()

@pytest.fixture(scope="module")
def graphql_service_3():
    svc = NodeGraphQL(["node", "remote_schemas/nodejs/secondary_remote_schema_perms_error.js"])
    svc.start()
    yield svc
    svc.stop()

use_test_fixtures = pytest.mark.usefixtures (
    "graphql_service",
    "graphql_service_2",
    "graphql_service_3",
    "per_method_tests_db_state"
)

@use_test_fixtures
class TestAddRemoteSchemaPermissions:

    @classmethod
    def dir(cls):
        return "queries/remote_schemas/permissions/"

    def test_add_permission_with_valid_subset_of_fields(self, hge_ctx):
        st_code, resp = hge_ctx.v1metadataq_f(self.dir() + 'add_permission_with_valid_subset_of_fields.yaml')
        assert st_code == 200, resp

    """ Here the schemas are compatible """
    def test_update_remote_schema_details_with_permissions_set(self, hge_ctx):
        """ Permissions check """
        st_code, resp = hge_ctx.v1metadataq_f(self.dir() + 'add_permission_with_valid_subset_of_fields.yaml')
        assert st_code == 200, resp

        st_code, resp = hge_ctx.v1metadataq_f(self.dir() + 'update_remote_schema/update_schema.yaml')
        assert st_code == 200, resp
        """ check the details of remote schema in metadata """
        st_code, resp = hge_ctx.v1metadataq({"type": "export_metadata", "args": {}})
        assert st_code == 200, resp
        assert resp['remote_schemas'][0]['definition']['url'] == "http://localhost:4021"
        assert resp['remote_schemas'][0]['comment'] == 'this is from update query', resp
        assert resp['remote_schemas'][0]['definition']['timeout_seconds'] == 120, resp
        """ reset the changes to the original config """
        st_code, resp = hge_ctx.v1metadataq_f(self.dir() + 'update_remote_schema/revert_to_original_config.yaml')
        assert st_code == 200, resp
    
    def test_update_remote_schema_details_with_permissions_set_with_error(self, hge_ctx):
        st_code, resp = hge_ctx.v1metadataq_f(self.dir() + 'add_permission_with_valid_subset_of_fields.yaml')
        assert st_code == 200, resp

        st_code, resp = hge_ctx.v1metadataq_f(self.dir() + 'update_remote_schema/update_schema_error.yaml')
        assert st_code == 400, resp

    def test_add_permission_with_valid_subset_of_arguments(self, hge_ctx):
        st_code, resp = hge_ctx.v1metadataq_f(self.dir() + 'add_permission_with_valid_subset_of_arguments.yaml')
        assert st_code == 200, resp

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
        st_code, resp = hge_ctx.v1metadataq_f(self.dir() + 'add_permission_with_valid_subset_of_fields.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'execution_with_partial_fields_exposed_to_role.yaml')

    @pytest.mark.skipif(not PytestConf.config.getoption('--redis-url'), reason="Must enable redis")
    def test_execution_with_subset_of_fields_exposed_to_role_with_caching(self, hge_ctx):
        st_code, resp = hge_ctx.v1metadataq_f(self.dir() + 'add_permission_with_valid_subset_of_fields.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'execution_with_partial_fields_exposed_to_role_cached.yaml')

    def test_execution_with_subset_of_arguments_exposed_to_role(self, hge_ctx):
        st_code, resp = hge_ctx.v1metadataq_f(self.dir() + 'add_permission_with_valid_subset_of_arguments.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'execution_with_partial_args_exposed_to_role.yaml')

    def test_execution_with_unknown_role(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'unknown_role_execution.yaml')

@use_test_fixtures
class TestRemoteSchemaPermissionsArgumentPresets:

    @classmethod
    def dir(cls):
        return "queries/remote_schemas/permissions/argument_presets/"

    def test_execution_with_static_argument_preset(self, hge_ctx):
        st_code, resp = hge_ctx.v1metadataq_f(self.dir() + 'add_permission_with_static_preset_argument.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'execution_with_static_preset_args.yaml')

    def test_execution_with_session_argument_preset(self, hge_ctx):
        st_code, resp = hge_ctx.v1metadataq_f(self.dir() + 'add_permission_with_session_preset_argument.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'execution_with_session_preset_args.yaml')

class TestRemoteRelationshipPermissions:

    @classmethod
    def dir(cls):
        return "queries/remote_schemas/permissions/remote_relationships/"

    @pytest.fixture(autouse=True)
    def transact(self, hge_ctx, graphql_service):
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_with_permissions.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'teardown.yaml')
        assert st_code == 200, resp

    def test_basic_relationship(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic.yaml')
        assert st_code == 200, resp
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic_user.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'basic_remote_relationship_without_remote_schema_permissions_configured.yaml')
        check_query_f(hge_ctx, self.dir() + 'basic_remote_relationship_with_remote_schema_permissions_configured.yaml')

    # Test queries that combine several remote relationships, nested in
    # different ways, variously filtering different bits using permissions.
    def test_complex_multiple_joins(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + '../../remote_relationships/setup_multiple_remote_rel.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'complex_multiple_joins.yaml')

    def test_remote_relationship_with_field_containing_preset_argument(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'derive_remote_relationship_with_joining_field_containing_preset.yaml')

    def test_partial_arguments_of_remote_relationship_from_preset(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_messages_single_field.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'partial_arguments_from_preset.yaml')
