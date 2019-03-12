import time
from urllib.parse import urlparse
import pytest
import yaml

from ws_graphql_client import GraphQLClient
from test_schema_stitching import mk_add_remote_q

if not pytest.config.getoption("--test-remote-subs"):
    pytest.skip("--test-remote-subs flag is missing, skipping tests", allow_module_level=True)

#@pytest.mark.skip(reason='does not have proper env')
class TestRemoteSchemaSubscriptions():
    dir = 'queries/remote_schemas'
    teardown = {"type": "clear_metadata", "args": {}}

    @pytest.fixture(autouse=True, scope='class')
    def transact(self, hge_ctx):
        self._setup_remote_schema_tbls(hge_ctx)
        url = hge_ctx.test_remote_hge + '/v1alpha1/graphql'
        q = mk_add_remote_q('subs-test', url)
        st_code, resp = hge_ctx.v1q(q)
        assert st_code == 200, resp
        yield
        # teardown
        self._drop_remote_schema_tbls(hge_ctx)
        st_code, resp = hge_ctx.v1q(self.teardown)
        assert st_code == 200, resp

    def _setup_remote_schema_tbls(self, hge_ctx):
        fp = open(self.dir + '/hge_remote_setup.yaml')
        q = yaml.load(fp)
        resp = hge_ctx.http.post(
            hge_ctx.test_remote_hge + "/v1/query",
            json=q,
        )
        assert resp.status_code == 200

    def _drop_remote_schema_tbls(self, hge_ctx):
        fp = open(self.dir + '/hge_remote_teardown.yaml')
        q = yaml.load(fp)
        resp = hge_ctx.http.post(
            hge_ctx.test_remote_hge + "/v1/query",
            json=q,
        )
        assert resp.status_code == 200

    def _run_subscription(self, hge_ctx, query):
        self.wsc = GraphQLClient(hge_ctx)
        headers = {}
        if hge_ctx.hge_key:
            headers = {'x-hasura-admin-secret': hge_ctx.hge_key}

        self.wsc.conn_init(headers)
        ev = hge_ctx.get_ws_event(3)
        assert ev['type'] == 'connection_ack', ev
        id = self.wsc.start(query)
        return id

    def test_simple_subscription(self, hge_ctx):
        query = "subscription { animals {id common_name} }"
        id = self._run_subscription(hge_ctx, query)
        hge_ctx.get_ws_event(3)
        res = hge_ctx.get_ws_event(3)
        assert len(res['payload']['data']['animals']) == 3
        self.wsc.stop(id)

    def test_remote_then_local_subscription(self, hge_ctx):
        q1 = "subscription { animals {id common_name} }"
        q2 = "subscription { hello { code name } }"

        id1 = self._run_subscription(hge_ctx, q1)
        hge_ctx.get_ws_event(3)
        res = hge_ctx.get_ws_event(3)
        assert len(res['payload']['data']['animals']) == 3
        self.wsc.stop(id1)

        id2 = self._run_subscription(hge_ctx, q2)
        hge_ctx.get_ws_event(3)
        res = hge_ctx.get_ws_event(3)
        assert len(res['payload']['data']['hello']) == 2
        self.wsc.stop(id2)

    def test_local_then_remote_subscription(self, hge_ctx):
        q1 = "subscription { hello { code name } }"
        q2 = "subscription { animals {id common_name} }"

        id1 = self._run_subscription(hge_ctx, q1)
        hge_ctx.get_ws_event(3)
        res = hge_ctx.get_ws_event(3)
        assert len(res['payload']['data']['hello']) == 2
        self.wsc.stop(id1)

        id2 = self._run_subscription(hge_ctx, q2)
        hge_ctx.get_ws_event(3)
        res = hge_ctx.get_ws_event(3)
        assert len(res['payload']['data']['animals']) == 3
        self.wsc.stop(id2)

    def test_consecutive_remote_subscription(self, hge_ctx):
        q = "subscription { animals { id common_name } }"

        id1 = self._run_subscription(hge_ctx, q)
        hge_ctx.get_ws_event(3)
        res = hge_ctx.get_ws_event(3)
        assert len(res['payload']['data']['animals']) == 3
        self.wsc.stop(id1)

        id2 = self._run_subscription(hge_ctx, q)
        hge_ctx.get_ws_event(3)
        res = hge_ctx.get_ws_event(3)
        self.wsc.stop(id2)

    def test_interleaved_remote_subscription(self, hge_ctx):
        q1 = "subscription { animals { id common_name } }"
        q2 = "subscription { hello { code name } }"

        id1 = self._run_subscription(hge_ctx, q1)
        hge_ctx.get_ws_event(3)
        res = hge_ctx.get_ws_event(3)
        assert len(res['payload']['data']['animals']) == 3

        id2 = self._run_subscription(hge_ctx, q2)
        hge_ctx.get_ws_event(3)
        res = hge_ctx.get_ws_event(3)
        assert len(res['payload']['data']['hello']) == 2

        self.wsc.stop(id1)
        self.wsc.stop(id2)
