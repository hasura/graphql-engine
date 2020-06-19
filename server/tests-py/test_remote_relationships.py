#!/usr/bin/env python3

import pytest
import subprocess
import time

from validate import check_query_f, check_query
from remote_server import NodeGraphQL

@pytest.fixture(scope="module")
def graphql_service():
    svc = NodeGraphQL(["node", "remote_schemas/nodejs/index.js"])
    svc.start()
    yield svc
    svc.stop()

use_test_fixtures = pytest.mark.usefixtures(
    "graphql_service",
    "per_method_tests_db_state"
)

@use_test_fixtures
class TestCreateRemoteRelationship:
    @classmethod
    def dir(cls):
        return "queries/remote_schemas/remote_relationships/"

    # @pytest.fixture(autouse=True)
    # def transact(self, hge_ctx, graphql_service):
    #     print("In setup method")
    #     graphql_service.start()
    #     try:
    #         st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup.yaml')
    #         assert st_code == 200, resp
    #         yield
    #         st_code, resp = hge_ctx.v1q_f(self.dir() + 'teardown.yaml')
    #         assert st_code == 200, resp
    #     finally:
    #         graphql_service.stop()

    def test_create_valid(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic.yaml')
        assert st_code == 200, resp

        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_nested_args.yaml')
        assert st_code == 200, resp

        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_array.yaml')
        assert st_code == 200, resp

        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_nested_fields.yaml')
        assert st_code == 200, resp

        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_multiple_fields.yaml')
        assert st_code == 200, resp

        # st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_with_interface.yaml')
        # assert st_code == 200, resp

    def test_create_invalid(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_invalid_remote_rel_hasura_field.yaml')
        assert st_code == 400, resp

        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_invalid_remote_rel_literal.yaml')
        assert st_code == 400, resp

        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_invalid_remote_rel_variable.yaml')
        assert st_code == 400, resp

        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_invalid_remote_rel_remote_args.yaml')
        assert st_code == 400, resp

        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_invalid_remote_rel_remote_schema.yaml')
        assert st_code == 400, resp

        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_invalid_remote_rel_remote_field.yaml')
        assert st_code == 400, resp

        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_invalid_remote_rel_type.yaml')
        assert st_code == 400, resp

        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_invalid_remote_rel_nested_args.yaml')
        assert st_code == 400, resp

        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_invalid_remote_rel_array.yaml')
        assert st_code == 400, resp

    def test_generation(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic.yaml')
        assert st_code == 200, resp
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_nested_args.yaml')
        assert st_code == 200, resp

        check_query_f(hge_ctx, self.dir() + 'select_remote_fields.yaml')

@use_test_fixtures
class TestDeleteRemoteRelationship:
    @classmethod
    def dir(cls):
        return "queries/remote_schemas/remote_relationships/"

    def test_delete(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic.yaml')
        assert st_code == 200, resp
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'delete_remote_rel.yaml')
        assert st_code == 200, resp

    def test_delete_dependencies(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic.yaml')
        assert st_code == 200, resp
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'remove_remote_schema.yaml')
        assert st_code == 400, resp
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'delete_remote_rel.yaml')
        assert st_code == 200, resp

    def test_deleting_column_with_remote_relationship_dependency(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'drop_col_with_remote_rel_dependency.yaml')

    def test_deleting_table_with_remote_relationship_dependency(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'drop_table_with_remote_rel_dependency.yaml')

@use_test_fixtures
class TestUpdateRemoteRelationship:
    @classmethod
    def dir(cls):
        return "queries/remote_schemas/remote_relationships/"

    def test_update(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'basic_relationship.yaml')
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'update_remote_rel_basic.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'update_basic_query.yaml')

@use_test_fixtures
class TestExecution:

    @classmethod
    def dir(cls):
        return "queries/remote_schemas/remote_relationships/"

    # def test_basic_mixed(self, hge_ctx):
    #     check_query_f(hge_ctx, self.dir() + 'basic_mixed.yaml')

    def test_basic_relationship(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'basic_relationship.yaml')

    def test_basic_relationship_on_object(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'query_with_object_rel.yaml')

        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_nested_args.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'query_with_arr_rel.yaml')

    def test_basic_array(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_array.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'basic_array.yaml')

    def test_basic_array_without_join_key(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_array.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'basic_array_without_join_key.yaml')

    def test_multiple_fields(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_multiple_fields.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'basic_multiple_fields.yaml')

    def test_nested_fields(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_nested_fields.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'basic_nested_fields.yaml')

    def test_arguments(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_nested_args.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'query_with_arguments.yaml')

    # def test_with_variables(self, hge_ctx):
    #     check_query_f(hge_ctx, self.dir() + 'mixed_variables.yaml')
    #     st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_nested_args.yaml')
    #     assert st_code == 200, resp
    #     check_query_f(hge_ctx, self.dir() + 'remote_rel_variables.yaml')

    # def test_with_fragments(self, hge_ctx):
    #     check_query_f(hge_ctx, self.dir() + 'mixed_fragments.yaml')
    #     st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic.yaml')
    #     assert st_code == 200, resp
    #     check_query_f(hge_ctx, self.dir() + 'remote_rel_fragments.yaml')

    # TODO: Support interface in remote relationships
    # def test_with_interface(self, hge_ctx):
    #     check_query_f(hge_ctx, self.dir() + 'mixed_interface.yaml')
    #     check_query_f(hge_ctx, self.dir() + 'remote_rel_interface.yaml')

    def test_with_errors(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'query_with_errors_obj.yaml')
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_nested_args.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'query_with_errors_arr.yaml')

    def test_with_scalar_relationship(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_scalar.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'query_with_scalar_rel.yaml')

    def test_renaming_column_with_remote_relationship_dependency(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_nested_args.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'rename_col_with_remote_rel_dependency.yaml')

    def test_renaming_table_with_remote_relationship_dependency(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_nested_args.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'rename_table_with_remote_rel_dependency.yaml')


class TestDeepExecution:

    @classmethod
    def dir(cls):
        return "queries/remote_schemas/remote_relationships/"

    @pytest.fixture(autouse=True)
    def transact(self, hge_ctx, graphql_service):
        print("In setup method")
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup.yaml')
        assert st_code == 200, resp
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_address.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'teardown_address.yaml')
        assert st_code == 200, resp
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'teardown.yaml')
        assert st_code == 200, resp

    def test_with_deep_object(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'query_with_deep_nesting_obj.yaml')

    def test_with_deep_array(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_nested_args.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'query_with_deep_nesting_arr.yaml')

    def test_with_complex_path_object(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'query_with_deep_nesting_complex_path_obj.yaml')
        check_query_f(hge_ctx, self.dir() + 'query_with_deep_nesting_complex_path_obj2.yaml')

    def test_with_complex_path_array(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'query_with_deep_nesting_complex_path_arr.yaml')
        check_query_f(hge_ctx, self.dir() + 'query_with_deep_nesting_complex_path_arr2.yaml')


class TestExecutionWithPermissions:

    @classmethod
    def dir(cls):
        return "queries/remote_schemas/remote_relationships/"

    @pytest.fixture(autouse=True)
    def transact(self, hge_ctx, graphql_service):
        print("In setup method")
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_with_permissions.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'teardown.yaml')
        assert st_code == 200, resp

    def test_basic_relationship(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'basic_relationship_with_permissions1.yaml')
        check_query_f(hge_ctx, self.dir() + 'basic_relationship_with_permissions2.yaml')

    # Test queries that combine several remote relationships, nested in
    # different ways, variously filtering different bits using permissions.
    def test_complex_multiple_joins(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_multiple_remote_rel.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'complex_multiple_joins.yaml')
