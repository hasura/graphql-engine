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

    def _mk_ws_url(self, url):
        ws_url = urlparse(url)._replace(scheme='ws', path='/v1alpha1/graphql')
        return ws_url.geturl()

    def _run_subscription(self, hge_ctx, ws, query, cb, subalive=3):
        headers = {}
        if hge_ctx.hge_key:
            headers = {'x-hasura-admin-secret': hge_ctx.hge_key}

        id = ws.subscribe(query, callback=cb)
        time.sleep(subalive)
        ws.stop_subscribe(id)

    def test_simple_subscription(self, hge_ctx):
        query = "subscription { animals {id common_name} }"
        def cb(_id, data):
            assert len(data['payload']['data']['data']['animals']) == 3

        ws = GraphQLClient(self._mk_ws_url(hge_ctx.hge_url))
        self._run_subscription(hge_ctx, ws, query, cb)
        ws.close()

    def test_remote_then_local_subscription(self, hge_ctx):
        q1 = "subscription { animals {id common_name} }"
        q2 = "subscription { hello { code name } }"
        def animals_sub(_id, data):
            assert len(data['payload']['data']['data']['animals']) == 3

        def hello_sub(_id, data):
            assert len(data['payload']['data']['data']['code']) == 2

        ws = GraphQLClient(self._mk_ws_url(hge_ctx.hge_url))
        self._run_subscription(hge_ctx, ws, q1, animals_sub)
        time.sleep(1)
        self._run_subscription(hge_ctx, ws, q2, hello_sub)
        ws.close()

    def test_local_then_remote_subscription(self, hge_ctx):
        q1 = "subscription { hello { code name } }"
        q2 = "subscription { animals {id common_name} }"
        def animals_sub(_id, data):
            assert len(data['payload']['data']['data']['animals']) == 3

        def hello_sub(_id, data):
            assert len(data['payload']['data']['data']['code']) == 2

        ws = GraphQLClient(self._mk_ws_url(hge_ctx.hge_url))
        self._run_subscription(hge_ctx, ws, q1, hello_sub)
        time.sleep(1)
        self._run_subscription(hge_ctx, ws, q2, animals_sub)
        ws.close()

    def test_consecutive_remote_subscription(self, hge_ctx):
        q = "subscription { animals { id common_name } }"
        def animals_sub(_id, data):
            assert len(data['payload']['data']['data']['animals']) == 3

        ws = GraphQLClient(self._mk_ws_url(hge_ctx.hge_url))
        self._run_subscription(hge_ctx, ws, q, animals_sub)
        time.sleep(1)
        self._run_subscription(hge_ctx, ws, q, animals_sub)
        ws.close()

    def test_interleaved_remote_subscription(self, hge_ctx):
        q1 = "subscription { animals { id common_name } }"
        q2 = "subscription { languages { name type } }"
        def animals_sub(_id, data):
            assert len(data['payload']['data']['data']['animals']) == 3

        def lang_sub(_id, data):
            assert len(data['payload']['data']['data']['languages']) == 4

        ws = GraphQLClient(self._mk_ws_url(hge_ctx.hge_url))
        id1 = ws.subscribe(q1, callback=animals_sub)
        time.sleep(1)
        id2 = ws.subscribe(q2, callback=lang_sub)
        time.sleep(2)
        ws.stop_subscribe(id1)
        ws.stop_subscribe(id2)
        time.sleep(1)
        ws.close()
