from datetime import datetime, timedelta
import math
import json
import time
import base64

import ruamel.yaml as yaml
import pytest
from test_subscriptions import init_ws_conn
from context import PytestConf


if not PytestConf.config.getoption('--hge-webhook'):
    pytest.skip('--hge-webhook is missing, skipping webhook expiration tests', allow_module_level=True)

usefixtures = pytest.mark.usefixtures

@pytest.fixture(scope='function')
def ws_conn_recreate(ws_client):
    ws_client.recreate_conn()

def connect_with(hge_ctx, ws_client, headers):
    headers['X-Hasura-Role']      = 'user'
    headers['X-Hasura-User-Id']   = '1234321'
    headers['X-Hasura-Auth-Mode'] = 'webhook'

    token = base64.b64encode(json.dumps(headers).encode('utf-8')).decode('utf-8')
    headers['Authorization'] = 'Bearer ' + token
    payload = {'headers': headers}
    init_ws_conn(hge_ctx, ws_client, payload)

EXPIRE_TIME_FORMAT = '%a, %d %b %Y %T GMT'


@usefixtures('ws_conn_recreate')
class TestWebhookSubscriptionExpiry(object):
    def test_expiry_with_no_header(self, hge_ctx, ws_client):
        # no expiry time => the connextion will remain alive
        connect_with(hge_ctx, ws_client, {})
        time.sleep(5)
        assert ws_client.remote_closed == False, ws_client.remote_closed

    def test_expiry_with_expires_header(self, hge_ctx, ws_client):
        exp = datetime.utcnow() + timedelta(seconds=6)
        connect_with(hge_ctx, ws_client, {
            'Expires': exp.strftime(EXPIRE_TIME_FORMAT)
        })
        time.sleep(4)
        assert ws_client.remote_closed == False, ws_client.remote_closed
        time.sleep(4)
        assert ws_client.remote_closed == True, ws_client.remote_closed

    def test_expiry_with_cache_control(self, hge_ctx, ws_client):
        connect_with(hge_ctx, ws_client, {
            'Cache-Control': 'max-age=6'
        })
        time.sleep(4)
        assert ws_client.remote_closed == False, ws_client.remote_closed
        time.sleep(4)
        assert ws_client.remote_closed == True, ws_client.remote_closed

    def test_expiry_with_both(self, hge_ctx, ws_client):
        exp = datetime.utcnow() + timedelta(seconds=6)
        connect_with(hge_ctx, ws_client, {
            'Expires': exp.strftime(EXPIRE_TIME_FORMAT),
            'Cache-Control': 'max-age=10',
        })
        # cache-control has precedence, so the expiry time will be five seconds
        time.sleep(4)
        assert ws_client.remote_closed == False, ws_client.remote_closed
        time.sleep(4)
        assert ws_client.remote_closed == False, ws_client.remote_closed
        time.sleep(4)
        assert ws_client.remote_closed == True, ws_client.remote_closed

    def test_expiry_with_parse_error(self, hge_ctx, ws_client):
        exp = datetime.utcnow() + timedelta(seconds=3)
        connect_with(hge_ctx, ws_client, {
            'Expires': exp.strftime('%a, %d %m %Y %T UTC'),
            'Cache-Control': 'maxage=3',
        })
        # neither will parse, the connection will remain alive
        time.sleep(5)
        assert ws_client.remote_closed == False, ws_client.remote_closed
