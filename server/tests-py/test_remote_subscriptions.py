import time
from urllib.parse import urlparse
import pytest
import yaml

from context import GQLWsClient
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
        print(resp.json())
        assert resp.status_code == 200

    def _drop_remote_schema_tbls(self, hge_ctx):
        fp = open(self.dir + '/hge_remote_teardown.yaml')
        q = yaml.load(fp)
        resp = hge_ctx.http.post(
            hge_ctx.test_remote_hge + "/v1/query",
            json=q,
        )
        print(resp.json())
        assert resp.status_code == 200

    def _run_subscription(self, hge_ctx, q, session_vars={}):
        ws = hge_ctx.ws_client
        req = {'query': q}
        ws.init_as_admin()
        #if hge_ctx.hge_key:
        #    ws.init_as_admin()
        #else:
        #    headers = get_auth(hge_ctx, session_vars)
        #    ws.init(headers=headers)
        resp = ws.send_query(req)
        res = next(resp)
        return res

    def test_simple_subscription(self, hge_ctx):
        query = "subscription { animals {id common_name} }"
        res = self._run_subscription(hge_ctx, query)
        assert len(res['payload']['data']['animals']) == 3

    def test_remote_then_local_subscription(self, hge_ctx):
        q1 = "subscription { animals {id common_name} }"
        q2 = "subscription { hello { code name } }"

        res = self._run_subscription(hge_ctx, q1)
        assert len(res['payload']['data']['animals']) == 3

        res = self._run_subscription(hge_ctx, q2)
        assert len(res['payload']['data']['hello']) == 2

    def test_local_then_remote_subscription(self, hge_ctx):
        q1 = "subscription { hello { code name } }"
        q2 = "subscription { animals {id common_name} }"

        res = self._run_subscription(hge_ctx, q1)
        assert len(res['payload']['data']['hello']) == 2

        res = self._run_subscription(hge_ctx, q2)
        assert len(res['payload']['data']['animals']) == 3

    def test_consecutive_remote_subscription(self, hge_ctx):
        q = "subscription { animals { id common_name } }"

        res = self._run_subscription(hge_ctx, q)
        assert len(res['payload']['data']['animals']) == 3

        res = self._run_subscription(hge_ctx, q)
        assert len(res['payload']['data']['animals']) == 3

    def test_interleaved_remote_subscription(self, hge_ctx):
        q1 = "subscription { animals { id common_name } }"
        q2 = "subscription { hello { code name } }"

        res = self._run_subscription(hge_ctx, q1)
        assert len(res['payload']['data']['animals']) == 3

        res = self._run_subscription(hge_ctx, q2)
        assert len(res['payload']['data']['hello']) == 2
