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

class TestTopLevelMixedFields:

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



# class TestRemoteRelationships:

#     @pytest.fixture(autouse=True)
#     def transact(self, request, hge_ctx):
#         print("In setup method")
#         st_code, resp = hge_ctx.v1q_f('queries/remote_schemas/setup_relationship.yaml')
#         assert st_code == 200, resp
#         yield
#         st_code, resp = hge_ctx.v1q_f('queries/remote_schemas/teardown_relationship.yaml')
#         assert st_code == 200, resp

#     def test_basic(self, hge_ctx):
#         check_query_f(hge_ctx, 'queries/remote_schemas/basic_remote_relationship.yaml')
