#!/usr/bin/env python3

import pytest
import json
import queue

class TestSubscriptionBasic(object):

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        print ("In setup method")
        st_code, resp = hge_ctx.v1q_f('queries/basic/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f('queries/basic/teardown.yaml')
        assert st_code == 200, resp

    '''
        Refer: https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_connection_init
    '''
    def test_init_without_payload(self, hge_ctx):
        obj = {
            'type': 'connection_init'
        }
        hge_ctx.ws.send(json.dumps(obj))
        ev = hge_ctx.get_ws_event(3)
        assert ev['type'] == 'connection_ack', ev

    '''
        Refer: https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_connection_init
    '''
    def test_init(self, hge_ctx):
        obj = {
            'type': 'connection_init',
            'payload': {},
        }
        hge_ctx.ws.send(json.dumps(obj))
        ev = hge_ctx.get_ws_event(3)
        assert ev['type'] == 'connection_ack', ev

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
          hge_tests_test_t1(order_by: c1_desc, limit: 1) {
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
        '''
            Refer: https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_complete
        '''
        ev = hge_ctx.get_ws_event(3)
        assert ev['type'] == 'complete', ev

    '''
        Refer: https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_connection_terminate
    '''
    def test_connection_terminate(self, hge_ctx):
        obj = {
            'type': 'connection_terminate'
        }
        hge_ctx.ws.send(json.dumps(obj))
        with pytest.raises(queue.Empty):
            ev = hge_ctx.get_ws_event(3)

    '''
        Refer: https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_error
    '''
    def test_error(self, hge_ctx):
        assert 0 == 0
