#!/usr/bin/env python3

import pytest
import json
import queue
import yaml

'''
    Refer: https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_connection_init
'''

def init_ws_conn(hge_ctx, ws_client):
    payload = {}
    if hge_ctx.hge_key is not None:
        payload = {
            'headers' : {
                'X-Hasura-Admin-Secret': hge_ctx.hge_key
            }
        }
    obj = {
        'type': 'connection_init',
        'payload': payload,
    }
    ws_client.ws.send(json.dumps(obj))
    ev = ws_client.get_ws_event(3)
    assert ev['type'] == 'connection_ack', ev

class TestSubscriptionCtrl(object):

    def test_init_without_payload(self, hge_ctx, ws_client):
        if hge_ctx.hge_key is not None:
            pytest.skip("Payload is needed when admin secret is set")
        obj = {
            'type': 'connection_init'
        }
        ws_client.ws.send(json.dumps(obj))
        ev = ws_client.get_ws_event(3)
        assert ev['type'] == 'connection_ack', ev
    
    
    '''
        Refer: https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_connection_init
    '''

    def test_init(self, hge_ctx, ws_client):
        init_ws_conn(hge_ctx, ws_client)

    '''
        Refer: https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_connection_terminate
    '''

    def test_connection_terminate(self, hge_ctx, ws_client):
        obj = {
            'type': 'connection_terminate'
        }
        ws_client.ws.send(json.dumps(obj))
        with pytest.raises(queue.Empty):
            ev = ws_client.get_ws_event(3)

class TestSubscriptionBasic(object):

    @pytest.fixture(scope='class', autouse=True)
    def transact(self, request, hge_ctx, ws_client):
        self.dir = 'queries/subscriptions/basic'
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        init_ws_conn(hge_ctx, ws_client)
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp

    '''
        Refer: https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_connection_error
    '''

    def test_connection_error(self, ws_client):
        ws_client.ws.send("test")
        ev = ws_client.get_ws_event(3)
        assert ev['type'] == 'connection_error', ev

    '''
        Refer: https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_start
    '''

    def test_start(self, ws_client):
        query = """
        subscription {
        hge_tests_test_t1(order_by: {c1: desc}, limit: 1) {
            c1,
            c2
          }
        }
        """
        obj = {
            'id': '1',
            'payload': {
                'query': query
            },
            'type': 'start'
        }
        ws_client.ws.send(json.dumps(obj))
        '''
            Refer: https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_data
        '''
        ev = ws_client.get_ws_event(3)
        assert ev['type'] == 'data' and ev['id'] == '1', ev

    '''
        Refer https://github.com/apollographql/subscriptions-transport-ws/blob/01e0b2b65df07c52f5831cce5c858966ba095993/src/server.ts#L306
    '''

    @pytest.mark.skip(reason="refer https://github.com/hasura/graphql-engine/pull/387#issuecomment-421343098")
    def test_start_duplicate(self, ws_client):
        self.test_start(ws_client)

    def test_stop_without_id(self, ws_client):
        obj = {
            'type': 'stop'
        }
        ws_client.ws.send(json.dumps(obj))
        ev = ws_client.get_ws_event(3)
        assert ev['type'] == 'connection_error', ev

    '''
        Refer https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_stop
    '''

    def test_stop(self, ws_client):
        obj = {
            'type': 'stop',
            'id': '1'
        }
        ws_client.ws.send(json.dumps(obj))
        with pytest.raises(queue.Empty):
            ev = ws_client.get_ws_event(3)

    def test_start_after_stop(self, ws_client):
        self.test_start(ws_client)
        self.test_stop(ws_client)

    '''
        Refer: https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_complete
    '''

    def test_complete(self, hge_ctx, ws_client):
        query = """
        query {
          hge_tests_test_t1(order_by: {c1: desc}, limit: 1) {
            c1,
            c2
          }
        }
        """
        obj = {
            'id': '2',
            'payload': {
                'query': query
            },
            'type': 'start'
        }
        ws_client.ws.send(json.dumps(obj))
        ev = ws_client.get_ws_event(3)
        assert ev['type'] == 'data' and ev['id'] == '2', ev
        # Check for complete type
        ev = ws_client.get_ws_event(3)
        assert ev['type'] == 'complete' and ev['id'] == '2', ev


class TestSubscriptionLiveQueries(object):

    @pytest.fixture(scope='class', autouse=True)
    def transact(self, request, hge_ctx, ws_client):
        st_code, resp = hge_ctx.v1q_f(self.dir() + '/setup.yaml')
        assert st_code == 200, resp
        init_ws_conn(hge_ctx, ws_client)
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir() + '/teardown.yaml')
        assert st_code == 200, resp

    def test_live_queries(self, hge_ctx, ws_client):
        '''
            Create connection using connection_init
        '''
        payload = {}
        if hge_ctx.hge_key is not None:
            payload = {
                'headers' : {
                    'X-Hasura-Admin-Secret': hge_ctx.hge_key
                }
            }
        obj = {
            'type': 'connection_init',
            'payload' : payload
        }
        ws_client.ws.send(json.dumps(obj))
        ev = ws_client.get_ws_event(3)
        assert ev['type'] == 'connection_ack', ev

        with open(self.dir() + "/steps.yaml") as c:
            conf = yaml.load(c)

        query = """
        subscription {
          hge_tests_test_t2(order_by: {c1: desc}, limit: 1) {
            c1,
            c2
          }
        }
        """
        obj = {
            'id': 'live',
            'payload': {
                'query': query
            },
            'type': 'start'
        }
        ws_client.ws.send(json.dumps(obj))
        ev = ws_client.get_ws_event(3)
        assert ev['type'] == 'data' and ev['id'] == obj['id'], ev
        assert ev['payload']['data'] == {'hge_tests_test_t2': []}, ev['payload']['data']

        assert isinstance(conf, list) == True, 'Not an list'
        for index, step in enumerate(conf):
            obj = {
                'id': '{}'.format(index + 1),
                'payload': {
                    'query': step['query']
                },
                'type': 'start'
            }
            if 'variables' in step and step['variables']:
                obj['payload']['variables'] = json.loads(step['variables'])

            expected_resp = json.loads(step['response'])

            ws_client.ws.send(json.dumps(obj))
            ev = ws_client.get_ws_event(3)
            assert ev['type'] == 'data' and ev['id'] == obj['id'], ev
            assert ev['payload']['data'] == expected_resp, ev['payload']['data']

            ev = ws_client.get_ws_event(3)
            assert ev['type'] == 'complete' and ev['id'] == obj['id'], ev

            ev = ws_client.get_ws_event(3)
            assert ev['type'] == 'data' and ev['id'] == 'live', ev
            assert ev['payload']['data'] == {
                'hge_tests_test_t2': expected_resp[step['name']]['returning'] if 'returning' in expected_resp[
                    step['name']] else []
            }, ev['payload']['data']

        # stop live operation
        obj = {
            'id': 'live',
            'type': 'stop'
        }
        ws_client.ws.send(json.dumps(obj))
        with pytest.raises(queue.Empty):
            ev = ws_client.get_ws_event(3)

    @classmethod
    def dir(cls):
        return 'queries/subscriptions/live_queries'

