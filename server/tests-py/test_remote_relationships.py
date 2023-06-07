#!/usr/bin/env python3

import pytest

from conftest import extract_server_address_from
from remote_server import NodeGraphQL
from validate import check_query_f

@pytest.fixture(scope='class')
@pytest.mark.early
def graphql_service(worker_id: str, hge_fixture_env: dict[str, str]):
    (_, port) = extract_server_address_from('GRAPHQL_SERVICE_HANDLER')
    server = NodeGraphQL(worker_id, 'remote_schemas/nodejs/index.js', port=port)
    server.start()
    print(f'{graphql_service.__name__} server started on {server.url}')
    hge_fixture_env['GRAPHQL_SERVICE_HANDLER'] = server.url
    yield server
    server.stop()

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
    #         hge_ctx.v1q_f(self.dir() + 'setup.yaml')
    #         yield
    #         hge_ctx.v1q_f(self.dir() + 'teardown.yaml')
    #     finally:
    #         graphql_service.stop()

    def test_create_valid(self, hge_ctx):
        files = [
            'setup_remote_rel_basic.yaml',
            'setup_remote_rel_nested_args.yaml',
            'setup_remote_rel_array.yaml',
            'setup_remote_rel_nested_fields.yaml',
            'setup_remote_rel_multiple_fields.yaml',
            'setup_remote_rel_joining_singleton_with_array.yaml',
            'setup_remote_rel_with_interface.yaml',
            'setup_remote_rel_with_union.yaml',
            'setup_remote_rel_with_enum.yaml',
            'setup_remote_rel_computed_fields.yaml',
        ]

        for f in files:
            hge_ctx.v1q_f(self.dir() + f)

    def test_create_invalid(self, hge_ctx):
        files = [
            'setup_invalid_remote_rel_hasura_field.yaml',
            'setup_invalid_remote_rel_literal.yaml',
            'setup_invalid_remote_rel_variable.yaml',
            'setup_invalid_remote_rel_remote_args.yaml',
            'setup_invalid_remote_rel_remote_schema.yaml',
            'setup_invalid_remote_rel_remote_field.yaml',
            'setup_invalid_remote_rel_nested_args.yaml',
            'setup_invalid_remote_rel_array.yaml',
            'setup_invalid_remote_rel_computed_field.yaml',
        ]

        for f in files:
            hge_ctx.v1q_f(self.dir() + f, expected_status_code = 400)

    def test_generation(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic.yaml')
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_nested_args.yaml')

        check_query_f(hge_ctx, self.dir() + 'select_remote_fields.yaml')

@use_test_fixtures
class TestDeleteRemoteRelationship:
    @classmethod
    def dir(cls):
        return "queries/remote_schemas/remote_relationships/"

    def test_delete(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic.yaml')
        hge_ctx.v1q_f(self.dir() + 'delete_remote_rel.yaml')

    def test_delete_dependencies(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic.yaml')
        hge_ctx.v1q_f(self.dir() + 'remove_remote_schema.yaml', expected_status_code = 400)
        hge_ctx.v1q_f(self.dir() + 'delete_remote_rel.yaml')

    def test_deleting_column_with_remote_relationship_dependency(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'drop_col_with_remote_rel_dependency.yaml')
        self._check_no_remote_relationships(hge_ctx, 'profiles')

    def test_deleting_table_with_remote_relationship_dependency(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'drop_table_with_remote_rel_dependency.yaml')
        self._check_no_remote_relationships(hge_ctx, 'profiles')

    def _check_no_remote_relationships(self, hge_ctx, table):
        export_metadata_q = {
            'type': 'export_metadata',
            'args': {}
        }
        resp = hge_ctx.v1q(export_metadata_q)
        tables = resp['sources'][0]['tables']
        for t in tables:
            if t['table']['name'] == table:
                assert 'event_triggers' not in t

@use_test_fixtures
class TestUpdateRemoteRelationship:
    @classmethod
    def dir(cls):
        return "queries/remote_schemas/remote_relationships/"

    def test_update(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic.yaml')
        check_query_f(hge_ctx, self.dir() + 'basic_relationship.yaml')
        hge_ctx.v1q_f(self.dir() + 'update_remote_rel_basic.yaml')
        check_query_f(hge_ctx, self.dir() + 'update_basic_query.yaml')

@use_test_fixtures
class TestExecution:

    @classmethod
    def dir(cls):
        return "queries/remote_schemas/remote_relationships/"

    # def test_basic_mixed(self, hge_ctx):
    #     check_query_f(hge_ctx, self.dir() + 'basic_mixed.yaml')

    def test_basic_relationship(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic.yaml')
        check_query_f(hge_ctx, self.dir() + 'basic_relationship.yaml')

    def test_basic_relationship_on_object(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic.yaml')
        check_query_f(hge_ctx, self.dir() + 'query_with_object_rel.yaml')

        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_nested_args.yaml')
        check_query_f(hge_ctx, self.dir() + 'query_with_arr_rel.yaml')

    def test_regression_7172(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_regression_7172.yaml')
        check_query_f(hge_ctx, self.dir() + 'regression_7172.yaml')

    def test_basic_relationship_joining_singleton_to_array(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_joining_singleton_with_array.yaml')
        check_query_f(hge_ctx, self.dir() + 'basic_relationship_joining_singleton_with_array.yaml')

    def test_basic_array(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_array.yaml')
        check_query_f(hge_ctx, self.dir() + 'basic_array.yaml')

    def test_basic_array_without_join_key(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_array.yaml')
        check_query_f(hge_ctx, self.dir() + 'basic_array_without_join_key.yaml')

    def test_multiple_fields(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_multiple_fields.yaml')
        check_query_f(hge_ctx, self.dir() + 'basic_multiple_fields.yaml')

    # https://github.com/hasura/graphql-engine/issues/5448
    def test_remote_join_fields_with_null_joining_fields(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_null_joining_fields.yaml')
        check_query_f(hge_ctx, self.dir() + 'remote_rel_with_null_joining_fields.yaml')

    def test_nested_fields(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_nested_fields.yaml')
        check_query_f(hge_ctx, self.dir() + 'basic_nested_fields.yaml')

    def test_arguments(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_nested_args.yaml')
        check_query_f(hge_ctx, self.dir() + 'query_with_arguments.yaml')

    def test_with_variables(self, hge_ctx):
   #    check_query_f(hge_ctx, self.dir() + 'mixed_variables.yaml')  -- uses heterogenous execution, due to which this assert fails
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_nested_args.yaml')
        check_query_f(hge_ctx, self.dir() + 'remote_rel_variables.yaml')

    def test_with_fragments(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'mixed_fragments.yaml')
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic.yaml')
        check_query_f(hge_ctx, self.dir() + 'remote_rel_fragments.yaml')

    def test_with_interface(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_with_interface.yaml')
        check_query_f(hge_ctx, self.dir() + 'mixed_interface.yaml')
        check_query_f(hge_ctx, self.dir() + 'remote_rel_interface.yaml')

    def test_with_union(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_with_union.yaml')
        check_query_f(hge_ctx, self.dir() + 'remote_rel_union.yaml')

    def test_with_enum(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_with_enum.yaml')
        check_query_f(hge_ctx, self.dir() + 'remote_rel_enum.yaml')

    def test_with_errors(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic.yaml')
        check_query_f(hge_ctx, self.dir() + 'query_with_errors_obj.yaml')
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_nested_args.yaml')
        check_query_f(hge_ctx, self.dir() + 'query_with_errors_arr.yaml')

    def test_with_aliased_remote_join_keys(self, hge_ctx):
        """
        Regression test for https://github.com/hasura/graphql-engine/issues/7180.
        """
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic.yaml')
        check_query_f(hge_ctx, self.dir() + 'basic_relationship_alias.yaml')

    def test_with_scalar_relationship(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_scalar.yaml')
        check_query_f(hge_ctx, self.dir() + 'query_with_scalar_rel.yaml')

    def test_renaming_column_with_remote_relationship_dependency(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_nested_args.yaml')
        check_query_f(hge_ctx, self.dir() + 'rename_col_with_remote_rel_dependency.yaml')

    def test_renaming_table_with_remote_relationship_dependency(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_nested_args.yaml')
        check_query_f(hge_ctx, self.dir() + 'rename_table_with_remote_rel_dependency.yaml')

    # The check for the presence of the remote relationships is deferred to later stage
    # in the server source code. To run this test we need to use proper websocket client
    # instead of HTTP.
    # def test_remote_joins_with_subscription_should_throw_error(self, hge_ctx):
    #     hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic.yaml')
    #     check_query_f(hge_ctx, self.dir() + 'subscription_with_remote_join_fields.yaml')

    def test_remote_joins_in_mutation_response(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic_with_authors.yaml')
        check_query_f(hge_ctx, self.dir() + 'mutation_output_with_remote_join_fields.yaml')

class TestDeepExecution:

    @classmethod
    def dir(cls):
        return "queries/remote_schemas/remote_relationships/"

    @pytest.fixture(autouse=True)
    def transact(self, hge_ctx, graphql_service):
        print("In setup method")
        hge_ctx.v1q_f(self.dir() + 'setup.yaml')
        hge_ctx.v1q_f(self.dir() + 'setup_address.yaml')
        yield
        hge_ctx.v1q_f(self.dir() + 'teardown_address.yaml')
        hge_ctx.v1q_f(self.dir() + 'teardown.yaml')

    def test_with_deep_object(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic.yaml')
        check_query_f(hge_ctx, self.dir() + 'query_with_deep_nesting_obj.yaml')

    def test_with_deep_array(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_nested_args.yaml')
        check_query_f(hge_ctx, self.dir() + 'query_with_deep_nesting_arr.yaml')

    def test_with_complex_path_object(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic.yaml')
        check_query_f(hge_ctx, self.dir() + 'query_with_deep_nesting_complex_path_obj.yaml')
        check_query_f(hge_ctx, self.dir() + 'query_with_deep_nesting_complex_path_obj2.yaml')

    def test_with_complex_path_array(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic.yaml')
        check_query_f(hge_ctx, self.dir() + 'query_with_deep_nesting_complex_path_arr.yaml')
        check_query_f(hge_ctx, self.dir() + 'query_with_deep_nesting_complex_path_arr2.yaml')


class TestExecutionWithPermissions:

    @classmethod
    def dir(cls):
        return "queries/remote_schemas/remote_relationships/"

    @pytest.fixture(autouse=True)
    def transact(self, hge_ctx, graphql_service):
        print("In setup method")
        hge_ctx.v1q_f(self.dir() + 'setup_with_permissions.yaml')
        yield
        hge_ctx.v1q_f(self.dir() + 'teardown.yaml')

    def test_basic_relationship(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic.yaml')
        check_query_f(hge_ctx, self.dir() + 'basic_relationship_with_permissions1.yaml')
        check_query_f(hge_ctx, self.dir() + 'basic_relationship_with_permissions2.yaml')

    # Test queries that combine several remote relationships, nested in
    # different ways, variously filtering different bits using permissions.
    def test_complex_multiple_joins(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_multiple_remote_rel.yaml')
        check_query_f(hge_ctx, self.dir() + 'complex_multiple_joins.yaml')

@use_test_fixtures
class TestWithRelay:

    @classmethod
    def dir(cls):
        return "queries/remote_schemas/remote_relationships/"

    def test_with_relay_fail(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic.yaml')
        check_query_f(hge_ctx, self.dir() + "with_relay.yaml")

@use_test_fixtures
class TestExecutionWithCustomization:

    @classmethod
    def dir(cls):
        return "queries/remote_schemas/remote_relationships/schema_customization/"

    def test_basic_relationship(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_basic.yaml')
        check_query_f(hge_ctx, self.dir() + 'basic_relationship.yaml')

    def test_nested_fields(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_nested_fields.yaml')
        check_query_f(hge_ctx, self.dir() + 'basic_nested_fields.yaml')


class TestComputedFieldsInRemoteRelationship:

    @classmethod
    def dir(cls):
        return "queries/remote_schemas/remote_relationships/"

    @pytest.fixture(autouse=True)
    def transact(self, hge_ctx, graphql_service):
        print("In setup method")
        hge_ctx.v1q_f(self.dir() + 'setup.yaml')
        hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_computed_fields.yaml')
        yield
        hge_ctx.v1q_f(self.dir() + 'teardown.yaml')

    def test_remote_join_with_computed_field(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'remote_join_with_computed_field.yaml')

    def test_remote_join_with_computed_field_session(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'remote_join_with_computed_field_session.yaml')

@use_test_fixtures
class TestRemoteRelationshipFieldType:

    @classmethod
    def dir(cls):
        return "queries/remote_schemas/remote_relationships"

    def test_remote_relationship_field_type(self, hge_ctx):
        hge_ctx.v1q_f(self.dir() + '/setup_remote_rel_nested_args.yaml')
        check_query_f(hge_ctx, self.dir() + '/remote_relationship_field_type.yaml')
