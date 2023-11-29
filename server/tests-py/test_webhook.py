import base64
from datetime import datetime, timedelta
import json
import pytest
import time

from test_subscriptions import init_ws_conn
from validate import authorize_for_webhook, check_query_f


# The tests below would ideally use parameterization rather than subclassing,
# but that doesn't work because of `hge_fixture_env`, which creates a "soft"
# dependency between `webhook_server` and `hge_server`. Paramterizing the former
# *should* force the latter to be recreated for each new parameter set, but
# `hge_server` isn't actually aware there's a dependency. See
# `TestParameterizedFixtures` in test_tests.py for more information.

# Note that `validate.py` will verify that the webhook rejects invalid
# authentication tokens, so we don't need to do this explicitly.

@pytest.mark.usefixtures('webhook_server', 'per_class_tests_db_state')
@pytest.mark.admin_secret
class AbstractTestWebhookV2Query(object):
    @classmethod
    def dir(cls):
        return 'queries/v2/basic'

    def test_v2_query(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_article.yaml')

@pytest.mark.hge_env('HASURA_GRAPHQL_AUTH_HOOK_MODE', 'GET')
@pytest.mark.no_tls_webhook_server
class TestWebhookV2QueryInGETModeWithoutTLS(AbstractTestWebhookV2Query): pass

@pytest.mark.hge_env('HASURA_GRAPHQL_AUTH_HOOK_MODE', 'GET')
@pytest.mark.tls_webhook_server
class TestWebhookV2QueryInGETModeWithTLS(AbstractTestWebhookV2Query): pass

@pytest.mark.hge_env('HASURA_GRAPHQL_AUTH_HOOK_MODE', 'POST')
@pytest.mark.no_tls_webhook_server
class TestWebhookV2QueryInPOSTModeWithoutTLS(AbstractTestWebhookV2Query): pass

@pytest.mark.hge_env('HASURA_GRAPHQL_AUTH_HOOK_MODE', 'POST')
@pytest.mark.tls_webhook_server
class TestWebhookV2QueryInPOSTModeWithTLS(AbstractTestWebhookV2Query): pass


@pytest.mark.usefixtures('webhook_server')
@pytest.mark.admin_secret
class AbstractTestWebhookMetadata(object):
    def test_v1_metadata(self, hge_ctx):
        resp = hge_ctx.v1metadataq(
            q={"type": "export_metadata", "args": {}},
        )
        assert 'sources' in resp
        assert len(resp['sources']) > 0

    def test_v1_metadata_without_secret_is_unauthorized(self, hge_ctx):
        hge_ctx.v1metadataq(
            q={"type": "export_metadata", "args": {}},
            headers={
                'X-Hasura-Admin-Secret': None, # don't provide a secret
            },
            expected_status_code=401,
        )

    def test_v1_metadata_with_bearer_is_unauthorized(self, hge_ctx):
        hge_ctx.v1metadataq(
            q={"type": "export_metadata", "args": {}},
            headers={
                **authorize_for_webhook({}),
                'X-Hasura-Admin-Secret': None, # don't provide a secret
            },
            expected_status_code=400,
        )

@pytest.mark.hge_env('HASURA_GRAPHQL_AUTH_HOOK_MODE', 'GET')
@pytest.mark.no_tls_webhook_server
class TestWebhookMetadataInGETModeWithoutTLS(AbstractTestWebhookMetadata): pass

@pytest.mark.hge_env('HASURA_GRAPHQL_AUTH_HOOK_MODE', 'GET')
@pytest.mark.tls_webhook_server
class TestWebhookMetadataInGETModeWithTLS(AbstractTestWebhookMetadata): pass

@pytest.mark.hge_env('HASURA_GRAPHQL_AUTH_HOOK_MODE', 'POST')
@pytest.mark.no_tls_webhook_server
class TestWebhookMetadataInPOSTModeWithoutTLS(AbstractTestWebhookMetadata): pass

@pytest.mark.hge_env('HASURA_GRAPHQL_AUTH_HOOK_MODE', 'POST')
@pytest.mark.tls_webhook_server
class TestWebhookMetadataInPOSTModeWithTLS(AbstractTestWebhookMetadata): pass


@pytest.mark.usefixtures('webhook_server')
@pytest.mark.admin_secret
class TestWebhookSubscriptionExpiry(object):
    EXPIRE_TIME_FORMAT = '%a, %d %b %Y %T GMT'

    @pytest.fixture(scope='function', autouse=True)
    def ws_conn_recreate(self, ws_client):
        ws_client.recreate_conn()

    def test_expiry_with_no_header(self, ws_client):
        # no expiry time => the connextion will remain alive
        self.connect_with(ws_client, {})
        time.sleep(5)
        assert ws_client.remote_closed == False, ws_client.remote_closed

    def test_expiry_with_expires_header(self, ws_client):
        exp = datetime.utcnow() + timedelta(seconds=6)
        self.connect_with(ws_client, {
            'Expires': exp.strftime(self.EXPIRE_TIME_FORMAT),
        })
        time.sleep(4)
        assert ws_client.remote_closed == False, ws_client.remote_closed
        time.sleep(4)
        assert ws_client.remote_closed == True, ws_client.remote_closed

    def test_expiry_with_cache_control(self, ws_client):
        self.connect_with(ws_client, {
            'Cache-Control': 'max-age=6',
        })
        time.sleep(4)
        assert ws_client.remote_closed == False, ws_client.remote_closed
        time.sleep(4)
        assert ws_client.remote_closed == True, ws_client.remote_closed

    def test_expiry_with_both(self, ws_client):
        exp = datetime.utcnow() + timedelta(seconds=6)
        self.connect_with(ws_client, {
            'Expires': exp.strftime(self.EXPIRE_TIME_FORMAT),
            'Cache-Control': 'max-age=10',
        })
        # cache-control has precedence, so the expiry time will be 10 seconds
        time.sleep(4)
        assert ws_client.remote_closed == False, ws_client.remote_closed
        time.sleep(4)
        assert ws_client.remote_closed == False, ws_client.remote_closed
        time.sleep(4)
        assert ws_client.remote_closed == True, ws_client.remote_closed

    def test_expiry_with_parse_error(self, ws_client):
        exp = datetime.utcnow() + timedelta(seconds=3)
        self.connect_with(ws_client, {
            'Expires': exp.strftime('%a, %d %m %Y %T UTC'),
            'Cache-Control': 'maxage=3',
        })
        # neither will parse, the connection will remain alive
        time.sleep(5)
        assert ws_client.remote_closed == False, ws_client.remote_closed

    @classmethod
    def connect_with(cls, ws_client, headers):
        headers['X-Hasura-Role']      = 'user'
        headers['X-Hasura-User-Id']   = '1234321'
        headers['X-Hasura-Auth-Mode'] = 'webhook'

        token = base64.b64encode(json.dumps(headers).encode('utf-8')).decode('utf-8')
        headers['Authorization'] = 'Bearer ' + token
        payload = {'headers': headers}
        init_ws_conn(None, ws_client, payload)
