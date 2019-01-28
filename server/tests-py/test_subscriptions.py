#!/usr/bin/env python3

import pytest
import json
import queue
import yaml

'''
    Refer: https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_connection_init
'''


def test_init_without_payload(hge_ctx):
    if hge_ctx.hge_key is not None:
        pytest.skip("Payload is needed when access key is set")
    obj = {
        'type': 'connection_init'
    }
    hge_ctx.ws.send(json.dumps(obj))
    ev = hge_ctx.get_ws_event(3)
    assert ev['type'] == 'connection_ack', ev


'''
    Refer: https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_connection_init
'''


def test_init(hge_ctx):
    payload = {}
    if hge_ctx.hge_key is not None:
        payload = {
            'headers' : {
                'X-Hasura-Access-Key': hge_ctx.hge_key
            }
        }
    obj = {
        'type': 'connection_init',
        'payload': payload,
    }
    hge_ctx.ws.send(json.dumps(obj))
    ev = hge_ctx.get_ws_event(3)
    assert ev['type'] == 'connection_ack', ev


class TestSubscriptionBasic(object):

    @pytest.fixture(scope='class', autouse=True)
    def transact(self, request, hge_ctx):
        self.dir = 'queries/subscriptions/basic'
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp

    '''
        Refer: https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_connection_error
    '''

    def test_connection_error(self, hge_ctx):
        hge_ctx.ws.send("test")
        ev = hge_ctx.get_ws_event(3)
        assert ev['type'] == 'connection_error', ev

    '''
        Refer: https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_start
    '''

    def test_start(self, hge_ctx):
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
        hge_ctx.ws.send(json.dumps(obj))
        '''
            Refer: https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_data
        '''
        ev = hge_ctx.get_ws_event(3)
        assert ev['type'] == 'data' and ev['id'] == '1', ev

    '''
        Refer https://github.com/apollographql/subscriptions-transport-ws/blob/01e0b2b65df07c52f5831cce5c858966ba095993/src/server.ts#L306
    '''

    @pytest.mark.skip(reason="refer https://github.com/hasura/graphql-engine/pull/387#issuecomment-421343098")
    def test_start_duplicate(self, hge_ctx):
        self.test_start(hge_ctx)

    def test_stop_without_id(self, hge_ctx):
        obj = {
            'type': 'stop'
        }
        hge_ctx.ws.send(json.dumps(obj))
        ev = hge_ctx.get_ws_event(3)
        assert ev['type'] == 'connection_error', ev

    '''
        Refer https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_stop
    '''

    def test_stop(self, hge_ctx):
        obj = {
            'type': 'stop',
            'id': '1'
        }
        hge_ctx.ws.send(json.dumps(obj))
        with pytest.raises(queue.Empty):
            ev = hge_ctx.get_ws_event(3)

    def test_start_after_stop(self, hge_ctx):
        self.test_start(hge_ctx)
        self.test_stop(hge_ctx)

    '''
        Refer: https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_complete
    '''

    def test_complete(self, hge_ctx):
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
        hge_ctx.ws.send(json.dumps(obj))
        ev = hge_ctx.get_ws_event(3)
        assert ev['type'] == 'data' and ev['id'] == '2', ev
        # Check for complete type
        ev = hge_ctx.get_ws_event(3)
        assert ev['type'] == 'complete' and ev['id'] == '2', ev


class TestSubscriptionLiveQueries(object):

    @pytest.fixture(scope='class', autouse=True)
    def transact(self, request, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir() + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir() + '/teardown.yaml')
        assert st_code == 200, resp

    def test_live_queries(self, hge_ctx):
        '''
            Create connection using connection_init
        '''
        payload = {}
        if hge_ctx.hge_key is not None:
            payload = {
                'headers' : {
                    'X-Hasura-Access-Key': hge_ctx.hge_key
                }
            }
        obj = {
            'type': 'connection_init',
            'payload' : payload
        }
        hge_ctx.ws.send(json.dumps(obj))
        ev = hge_ctx.get_ws_event(3)
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
        hge_ctx.ws.send(json.dumps(obj))
        ev = hge_ctx.get_ws_event(3)
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

            hge_ctx.ws.send(json.dumps(obj))
            ev = hge_ctx.get_ws_event(3)
            assert ev['type'] == 'data' and ev['id'] == obj['id'], ev
            assert ev['payload']['data'] == expected_resp, ev['payload']['data']

            ev = hge_ctx.get_ws_event(3)
            assert ev['type'] == 'complete' and ev['id'] == obj['id'], ev

            ev = hge_ctx.get_ws_event(3)
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
        hge_ctx.ws.send(json.dumps(obj))
        with pytest.raises(queue.Empty):
            ev = hge_ctx.get_ws_event(3)

    @classmethod
    def dir(cls):
        return 'queries/subscriptions/live_queries'

'''
    Refer: https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_connection_terminate
'''


def test_connection_terminate(hge_ctx):
    obj = {
        'type': 'connection_terminate'
    }
    hge_ctx.ws.send(json.dumps(obj))
    with pytest.raises(queue.Empty):
        ev = hge_ctx.get_ws_event(3)
