#!/usr/bin/env python3

import pytest
import subprocess
import time

from validate import check_query_f
from remote_server import NodeGraphQL

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
