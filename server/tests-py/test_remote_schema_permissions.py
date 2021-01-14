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

use_test_fixtures = pytest.mark.usefixtures (
    "graphql_service",
    "per_method_tests_db_state"
)

@use_test_fixtures
class TestAddRemoteSchemaPermissions:

    @classmethod
    def dir(cls):
        return "queries/remote_schemas/permissions/"

    def test_add_permission_with_valid_subset_of_fields(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'add_permission_with_valid_subset_of_fields.yaml')
        assert st_code == 200, resp

    def test_add_permission_with_valid_subset_of_arguments(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'add_permission_with_valid_subset_of_arguments.yaml')
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
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'add_permission_with_valid_subset_of_fields.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'execution_with_partial_fields_exposed_to_role.yaml')

    def test_execution_with_subset_of_arguments_exposed_to_role(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'add_permission_with_valid_subset_of_arguments.yaml')
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
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'add_permission_with_static_preset_argument.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'execution_with_static_preset_args.yaml')

    def test_execution_with_session_argument_preset(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'add_permission_with_session_preset_argument.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'execution_with_session_preset_args.yaml')
