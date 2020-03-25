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

class TestSubscriptionWebhookExpiry(object):

    def test_webhook_expiry_with_expires_header(self, hge_ctx, ws_client):
        exp = datetime.now() + timedelta(seconds=3)
        headers = {
            'Expires': exp.strftime('%a, %d %b %Y %T GMT'),
            'X-Hasura-Role': 'user',
            'X-Hasura-User-Id': '1234321',
            'X-Hasura-Auth-Mode': 'webhook',
        }
        token = base64.b64encode(json.dumps(headers).encode('utf-8')).decode('utf-8')
        headers['Authorization'] = 'Bearer ' + token
        init_ws_conn(hge_ctx, ws_client, {'headers': headers})
        time.sleep(2)
        assert ws_client.remote_closed == False, ws_client.remote_closed
        time.sleep(2)
        assert ws_client.remote_closed == True, ws_client.remote_closed

class TestSubscriptionWebhookExpiry2(object):

    def test_webhook_expiry_with_cache_control_header(self, hge_ctx, ws_client):
        headers = {
            'Cache-Control': 'max-age=3',
            'X-Hasura-Role': 'user',
            'X-Hasura-User-Id': '1234321',
            'X-Hasura-Auth-Mode': 'webhook',
        }
        token = base64.b64encode(json.dumps(headers).encode('utf-8')).decode('utf-8')
        headers['Authorization'] = 'Bearer ' + token
        init_ws_conn(hge_ctx, ws_client, {'headers': headers})
        time.sleep(2)
        assert ws_client.remote_closed == False, ws_client.remote_closed
        time.sleep(2)
        assert ws_client.remote_closed == True, ws_client.remote_closed
