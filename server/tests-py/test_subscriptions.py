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
    init_msg = {
        'type': 'connection_init',
        'payload': payload,
    }
    ws_client.send(init_msg)
    ev = ws_client.get_ws_event(3)
    assert ev['type'] == 'connection_ack', ev

class TestSubscriptionCtrl(object):

    def test_init_without_payload(self, hge_ctx, ws_client):
        if hge_ctx.hge_key is not None:
            pytest.skip("Payload is needed when admin secret is set")
        init_msg = {
            'type': 'connection_init'
        }
        ws_client.send(init_msg)
        ev = ws_client.get_ws_event(15)
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
        ws_client.send(obj)
        with pytest.raises(queue.Empty):
            ev = ws_client.get_ws_event(3)

class TestSubscriptionBasic(object):

    @pytest.fixture(scope='class')
    def transact(self, request, hge_ctx):
        self.dir = 'queries/subscriptions/basic'
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp

    @pytest.fixture(autouse=True)
    def ws_conn_init(self, transact, hge_ctx, ws_client):
        init_ws_conn(hge_ctx, ws_client)

    '''
        Refer: https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_connection_error
    '''

    def test_connection_error(self, ws_client):
        ws_client.send({'type': 'test'})
        ev = ws_client.get_ws_event(15)
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
        ws_client.send(obj)
        '''
            Refer: https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_data
        '''
        ev = ws_client.get_ws_query_event('1',15)
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
        ws_client.send(obj)
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
        ws_client.send(obj)
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
        ws_client.send(obj)
        ev = ws_client.get_ws_query_event('2',3)
        assert ev['type'] == 'data' and ev['id'] == '2', ev
        # Check for complete type
        ev = ws_client.get_ws_query_event('2',3)
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
        ws_client.init_as_admin()

        with open(self.dir() + "/steps.yaml") as c:
            conf = yaml.safe_load(c)

        queryTmplt = """
        subscription {
          hge_tests_live_query_{0}: hge_tests_test_t2(order_by: {c1: desc}, limit: 1) {
            c1,
            c2
          }
        }
        """

        liveQs = []
        for i in [0,1,2]:
            query = queryTmplt.replace('{0}',str(i))
            headers={}
            if hge_ctx.hge_key is not None:
                headers['X-Hasura-Admin-Secret'] = hge_ctx.hge_key
            subscrPayload = { 'query': query }
            respLive = ws_client.send_query(subscrPayload, query_id='live_'+str(i), headers=headers, timeout=15)
            liveQs.append(respLive)
            ev = next(respLive)
            assert ev['type'] == 'data', ev
            assert ev['id'] == 'live_' + str(i), ev
            assert ev['payload']['data'] == {'hge_tests_live_query_'+str(i): []}, ev['payload']['data']

        assert isinstance(conf, list) == True, 'Not an list'
        for index, step in enumerate(conf):
            mutationPayload = { 'query': step['query'] }
            if 'variables' in step and step['variables']:
                mutationPayload['variables'] = json.loads(step['variables'])

            expected_resp = json.loads(step['response'])

            mutResp = ws_client.send_query(mutationPayload,'mutation_'+str(index),timeout=15)
            ev = next(mutResp)
            assert ev['type'] == 'data' and ev['id'] == 'mutation_'+str(index), ev
            assert ev['payload']['data'] == expected_resp, ev['payload']['data']

            ev = next(mutResp)
            assert ev['type'] == 'complete' and ev['id'] == 'mutation_'+str(index), ev

            for i, respLive in enumerate(liveQs):
                ev = next(respLive)
                assert ev['type'] == 'data', ev
                assert ev['id'] == 'live_' + str(i), ev
                assert ev['payload']['data'] == {
                    'hge_tests_live_query_'+str(i): expected_resp[step['name']]['returning'] if 'returning' in expected_resp[
                        step['name']] else []
                }, ev['payload']['data']

        for i in [0,1,2]:
            # stop live operation
            frame = {
                'id': 'live_'+str(i),
                'type': 'stop'
            }
            ws_client.send(frame)

        with pytest.raises(queue.Empty):
            ev = ws_client.get_ws_event(3)

    @classmethod
    def dir(cls):
        return 'queries/subscriptions/live_queries'

