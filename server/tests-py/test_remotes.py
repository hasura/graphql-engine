#!/usr/bin/env python3

import pytest
import subprocess
import time
from validate import check_query_f

from validate import check_query_f, check_query

class NodeGraphQL():

    def __init__(self, cmd):
        self.cmd = cmd
        self.proc = None

    def start(self):
        proc = subprocess.Popen(self.cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        time.sleep(1)
        self.proc = proc

    def stop(self):
        self.proc.terminate()

@pytest.fixture(scope="module")
def graphql_service():
    svc = NodeGraphQL(["node", "remote_schemas/nodejs/index.js"])
    return svc

class TestCreateRemoteRelationship:
    @classmethod
    def dir(cls):
        return "queries/remote_schemas/remote_relationships/"

    @pytest.fixture(autouse=True)
    def transact(self, hge_ctx, graphql_service):
        print("In setup method")
        graphql_service.start()
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'teardown.yaml')
        assert st_code == 200, resp
        graphql_service.stop()

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

        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_remote_rel_with_interface.yaml')
        assert st_code == 200, resp

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


class TestDeleteRemoteRelationship:
    @classmethod
    def dir(cls):
        return "queries/remote_schemas/remote_relationships/"

    @pytest.fixture(autouse=True)
    def transact(self, hge_ctx, graphql_service):
        print("In setup method")
        graphql_service.start()
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'teardown.yaml')
        assert st_code == 200, resp
        graphql_service.stop()

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

@pytest.mark.parametrize("transport", ['http', 'websocket'])
class TestExecution:

    @classmethod
    def dir(cls):
        return "queries/remote_schemas/remote_relationships/"

    @pytest.fixture(autouse=True)
    def transact(self, hge_ctx, graphql_service):
        print("In setup method")
        graphql_service.start()
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup.yaml')
        assert st_code == 200, resp
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'setup_all_relationships.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir() + 'teardown.yaml')
        assert st_code == 200, resp
        graphql_service.stop()

    def test_basic_mixed(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + 'basic_mixed.yaml', transport)

    def test_basic_relationship(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + 'basic_relationship.yaml', transport)

    def test_basic_array(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + 'basic_array.yaml', transport)

    def test_multiple_fields(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + 'basic_multiple_fields.yaml', transport)

    def test_with_variables(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + 'mixed_variables.yaml', transport)
        check_query_f(hge_ctx, self.dir() + 'remote_rel_variables.yaml', transport)

    def test_with_fragments(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + 'mixed_fragments.yaml', transport)
        check_query_f(hge_ctx, self.dir() + 'remote_rel_fragments.yaml', transport)

    def test_with_interface(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + 'mixed_interface.yaml', transport)
        check_query_f(hge_ctx, self.dir() + 'remote_rel_interface.yaml', transport)
