#!/usr/bin/env python3

import pytest
import yaml

from validate import check_query_f
from test_schema_stitching import add_remote, delete_remote


@pytest.fixture(scope='class')
def remote_get_url(remote_gql_server):
    def remote_url(path):
        return remote_gql_server.root_url + path
    return remote_url

def run_sql(hge_ctx, sql):
    query = {
        'type': 'run_sql',
        'args': {
            'sql' : sql
        }
    }
    st_code, resp = hge_ctx.v1q(query)
    assert st_code == 200, resp
    return resp


@pytest.fixture(scope='class')
def validate_v1q_f(hge_ctx, request):
    def cvq(f, exp_code = 200):
        st_code, resp = hge_ctx.v1q_f(request.cls.dir() + f)
        assert st_code == exp_code, {
            'response': resp,
            'file' : f
        }
        return (st_code, resp)
    return cvq


class TestCreateRemoteRelationship:

    @classmethod
    def dir(cls):
        return "queries/remote_schemas/remote_relationships/"

    remote_schemas = {
        'user' : '/user-graphql',
        'prefixer-proxy': '/graphql-prefixer-proxy'
    }

    @pytest.fixture(autouse=True, scope='class')
    def db_state(self, hge_ctx, remote_get_url, validate_v1q_f):
        validate_v1q_f('setup.yaml')
        for (schema, path) in self.remote_schemas.items():
            add_remote(hge_ctx, schema, remote_get_url(path))
        yield
        for schema in self.remote_schemas:
            delete_remote(hge_ctx, schema)
        validate_v1q_f('teardown.yaml')

    def test_create_basic(self, validate_v1q_f):
        validate_v1q_f('setup_remote_rel_basic.yaml')

    def test_create_nested(self, validate_v1q_f):
        validate_v1q_f('setup_remote_rel_nested_args.yaml')

    def test_create_with_array(self, validate_v1q_f):
        validate_v1q_f('setup_remote_rel_array.yaml')

    def test_create_nested_fields(self, validate_v1q_f):
        validate_v1q_f('setup_remote_rel_nested_fields.yaml')

    def test_create_multi_nested_fields(self, validate_v1q_f):
        validate_v1q_f('setup_remote_rel_multi_nested_fields.yaml')

    def test_create_multiple_hasura_fields(self, validate_v1q_f):
        validate_v1q_f('setup_remote_rel_multiple_fields.yaml')

    @pytest.mark.xfail(reason="Refer https://github.com/tirumaraiselvan/graphql-engine/issues/53")
    def test_create_arg_with_arr_struture(self, validate_v1q_f):
        validate_v1q_f('setup_remote_rel_arg_with_arr_structure.yaml')

    @pytest.mark.xfail(reason="Refer https://github.com/tirumaraiselvan/graphql-engine/issues/15")
    def test_create_enum_arg(self, validate_v1q_f):
        validate_v1q_f('setup_remote_rel_enum_arg.yaml')

    # st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_with_interface.yaml')
    # assert st_code == 200, resp

    def test_create_error_non_admin_role(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'setup_remote_rel_err_non_admin_role.yaml')

    def test_create_invalid_hasura_field(self, validate_v1q_f):
        validate_v1q_f('setup_invalid_remote_rel_hasura_field.yaml', 400)

    def test_create_invalid_remote_rel_arg_literal(self, validate_v1q_f):
        """Test with input argument literal not having the required type"""
        validate_v1q_f('setup_invalid_remote_rel_literal.yaml', 400)

    def test_create_invalid_remote_rel_variable(self, validate_v1q_f):
        validate_v1q_f('setup_invalid_remote_rel_variable.yaml', 400)

    def test_create_invalid_remote_args(self, validate_v1q_f):
        validate_v1q_f('setup_invalid_remote_rel_remote_args.yaml', 400)

    def test_create_invalid_remote_schema(self, validate_v1q_f):
        validate_v1q_f('setup_invalid_remote_rel_remote_schema.yaml', 400)

    def test_create_invalid_remote_field(self, validate_v1q_f):
        validate_v1q_f('setup_invalid_remote_rel_remote_field.yaml', 400)

    def test_create_invalid_remote_rel_type(self, validate_v1q_f):
        validate_v1q_f('setup_invalid_remote_rel_type.yaml', 400)

    def test_create_invalid_remote_rel_nested_args(self, validate_v1q_f):
        validate_v1q_f('setup_invalid_remote_rel_nested_args.yaml', 400)

    def test_create_invalid_remote_rel_array(self, validate_v1q_f):
        validate_v1q_f('setup_invalid_remote_rel_array.yaml', 400)
