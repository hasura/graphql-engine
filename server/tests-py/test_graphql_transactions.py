import ruamel.yaml as yaml
import time

from validate import assert_graphql_resp_expected
from super_classes import DefaultTestSelectQueries

'''
TODO:- Add tests for
- abort
- commit and check if changes are committed in database by querying the table
- Subscriptions - Not allowed
- Remote schemas - Not allowed
- Permissions
- Webhook
- JWT
- Actions Guard
'''

class TestTransactionBasic(DefaultTestSelectQueries):
    @classmethod
    def dir(cls):
        return 'queries/graphql_transaction/basic'

    def _mk_init_msg(self, hge_ctx, payload={}):
        headers = {}
        if hge_ctx.hge_key:
            headers['x-hasura-admin-secret'] = hge_ctx.hge_key
        payload['headers'] = headers
        return {
            'type': 'init',
            'payload': payload
        }

    def _mk_exec_msg(self, payload):
        return {
            'type': 'execute',
            'payload': payload
        }

    def _test_execute(self, ws_tx_client, file_path):
        with open(self.dir() + file_path) as c:
            conf = yaml.safe_load(c)
            query = conf['query']
            request_id = 'id123'
            execute_msg = self._mk_exec_msg({
                'request_id': request_id,
                'query': query
            })
            ws_tx_client.send(execute_msg)
            res = ws_tx_client.receive(3)
            assert res['type'] == 'data', res
            assert res['request_id'] == request_id, res
            assert_graphql_resp_expected(res['payload'], conf['response'], query)

    def test_execute_without_init(self, hge_ctx, ws_tx_client):
        with open(self.dir() + '/insert_test.yaml') as c:
            conf = yaml.safe_load(c)
            query = conf['query']
            execute_msg = self._mk_exec_msg({
                'query': query
            })
            ws_tx_client.send(execute_msg)
            res = ws_tx_client.receive(3)
            assert res['type'] == 'error'
            assert res['payload'] == 'query received without transaction init'

    def test_init(self, hge_ctx, ws_tx_client):
        ws_tx_client.send(self._mk_init_msg(hge_ctx))
        res = ws_tx_client.receive(3)
        assert res['type'] == 'initialised', res

    def test_again_init(self, hge_ctx, ws_tx_client):
        ws_tx_client.send(self._mk_init_msg(hge_ctx))
        res = ws_tx_client.receive(3)
        assert res['type'] == 'init_error', res
        assert res['payload'] == 'transaction cannot be initialised more than once in a single WebSocket session', res

    def test_execute_insert(self, hge_ctx, ws_tx_client):
        self._test_execute(ws_tx_client, '/insert_test.yaml')

    def test_execute_select(self, hge_ctx, ws_tx_client):
        self._test_execute(ws_tx_client, '/query_test.yaml')

    def test_commit(self, hge_ctx, ws_tx_client):
        ws_tx_client.send({'type': 'commit'})
        time.sleep(2)
        assert ws_tx_client.remote_closed == True
