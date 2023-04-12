import json
from urllib.parse import urlparse

import websocket
import pytest

def url(hge_ctx):
    ws_url = urlparse(hge_ctx.hge_url)._replace(scheme='ws', path='/v1alpha1/graphql')
    return ws_url.geturl()

# The tests below would ideally use parameterization rather than subclassing,
# but that doesn't work because of `hge_fixture_env`, which creates a "soft"
# dependency between the environment variables and `hge_server`. Parameterizing
# the former *should* force the latter to be recreated for each new set of
# environment variables, but `hge_server` isn't actually aware there's a
# dependency. See `TestParameterizedFixtures` in test_tests.py for more
# information.

class AbstractTestWebsocketInitCookie:
    """
    test if cookie is sent when initing the websocket connection, is our auth
    webhook receiving the cookie
    """
    dir = 'queries/remote_schemas'

    @pytest.fixture(autouse=True)
    def transact(self, hge_ctx):
        hge_ctx.v1q_f(self.dir + '/person_table.yaml')
        yield
        hge_ctx.v1q_f(self.dir + '/drop_person_table.yaml')

    def _send_query(self, hge_ctx):
        ws_url = url(hge_ctx)
        headers = {'Cookie': 'foo=bar;'}
        ws = websocket.create_connection(ws_url, header=headers)
        init_payload = {
            'type': 'connection_init',
            'payload': {'headers': {}}
        }
        ws.send(json.dumps(init_payload))
        payload = {
            'type': 'start',
            'id': '1',
            'payload': {'query': 'query { person {name}}'}
        }
        ws.send(json.dumps(payload))
        return ws

    def _test_received_data(self, hge_ctx):
        ws = self._send_query(hge_ctx)
        it = 0
        while True:
            raw = ws.recv()
            frame = json.loads(raw)
            if frame['type'] == 'data':
                assert 'person' in frame['payload']['data']
                break
            elif it == 10:
                assert False, f'max try over: {raw}'
            elif frame['type'] == 'connection_error' or frame['type'] == 'error':
                assert False, f'connection error: {raw}'
            it = it + 1

    def _test_received_connection_error(self, hge_ctx):
        ws = self._send_query(hge_ctx)
        it = 0
        while True:
            raw = ws.recv()
            frame = json.loads(raw)
            if frame['type'] ==  'data':
                assert False, f'got data: {raw}'
            elif it == 10:
                assert False, f'max try over: {raw}'
            elif frame['type'] == 'connection_error':
                assert frame['payload'] == 'Authentication hook unauthorized this request'
                break
            elif frame['type'] == 'error':
                assert False, f'error: {raw}'
            it = it + 1

@pytest.mark.admin_secret
@pytest.mark.usefixtures('auth_hook')
class TestWebsocketInitCookieReadWithCorsEnabled(AbstractTestWebsocketInitCookie):
    def test_websocket_init_cookie_used(self, hge_ctx):
        self._test_received_data(hge_ctx)

@pytest.mark.admin_secret
@pytest.mark.usefixtures('auth_hook')
@pytest.mark.hge_env('HASURA_GRAPHQL_DISABLE_CORS', 'true')
class TestWebsocketInitCookieNotReadWithCorsDisabled(AbstractTestWebsocketInitCookie):
    def test_websocket_init_cookie_not_used(self, hge_ctx):
        self._test_received_connection_error(hge_ctx)

@pytest.mark.admin_secret
@pytest.mark.usefixtures('auth_hook')
@pytest.mark.hge_env('HASURA_GRAPHQL_DISABLE_CORS', 'true')
@pytest.mark.hge_env('HASURA_GRAPHQL_WS_READ_COOKIE', 'true')
class TestWebsocketInitCookieReadWithCorsDisabledWhenRequired(AbstractTestWebsocketInitCookie):
    def test_websocket_init_cookie_not_used(self, hge_ctx):
        self._test_received_data(hge_ctx)
