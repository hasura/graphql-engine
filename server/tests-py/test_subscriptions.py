#!/usr/bin/env python3

import pytest
import json
import queue
import ruamel.yaml as yaml

usefixtures = pytest.mark.usefixtures

@pytest.fixture(scope='class')
def ws_conn_init(hge_ctx, ws_client):
        init_ws_conn(hge_ctx, ws_client)

'''
    Refer: https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_connection_init
'''

def init_ws_conn(hge_ctx, ws_client, payload = None):
    if payload is None:
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

@usefixtures('per_method_tests_db_state', 'ws_conn_init')
class TestSubscriptionBasic:
    @classmethod
    def dir(cls):
        return 'queries/subscriptions/basic'

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


@usefixtures('per_method_tests_db_state','ws_conn_init')
class TestSubscriptionLiveQueries:

    @classmethod
    def dir(cls):
        return 'queries/subscriptions/live_queries'

    def test_live_queries(self, hge_ctx, ws_client):
        '''
            Create connection using connection_init
        '''
        ws_client.init_as_admin()

        with open(self.dir() + "/steps.yaml") as c:
            conf = yaml.safe_load(c)

        queryTmplt = """
        subscription ($result_limit: Int!) {
          hge_tests_live_query_{0}: hge_tests_test_t2(order_by: {c1: asc}, limit: $result_limit) {
            c1,
            c2
          }
        }
        """

        queries = [(0, 1), (1, 2), (2, 2)]
        liveQs = []
        for i, resultLimit in queries:
            query = queryTmplt.replace('{0}',str(i))
            headers={}
            if hge_ctx.hge_key is not None:
                headers['X-Hasura-Admin-Secret'] = hge_ctx.hge_key
            subscrPayload = { 'query': query, 'variables': { 'result_limit': resultLimit } }
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

            for (i, resultLimit), respLive in zip(queries, liveQs):
                ev = next(respLive)
                assert ev['type'] == 'data', ev
                assert ev['id'] == 'live_' + str(i), ev

                expectedReturnedResponse = []
                if 'live_response' in step:
                    expectedReturnedResponse = json.loads(step['live_response'])
                elif 'returning' in expected_resp[step['name']]:
                    expectedReturnedResponse = expected_resp[step['name']]['returning']
                expectedLimitedResponse = expectedReturnedResponse[:resultLimit]
                expectedLiveResponse = { 'hge_tests_live_query_'+str(i): expectedLimitedResponse }

                assert ev['payload']['data'] == expectedLiveResponse, ev['payload']['data']

        for i, _ in queries:
            # stop live operation
            frame = {
                'id': 'live_'+str(i),
                'type': 'stop'
            }
            ws_client.send(frame)

        with pytest.raises(queue.Empty):
            ev = ws_client.get_ws_event(3)

@usefixtures('per_method_tests_db_state')
class TestSubscriptionMultiplexing:

    @classmethod
    def dir(cls):
        return 'queries/subscriptions/multiplexing'

    def test_query_parameterization(self, hge_ctx):
        with open(self.dir() + '/query.yaml') as c:
            config = yaml.safe_load(c)

        query = config['query']
        representative_sql = self.get_parameterized_sql(hge_ctx, query, config['variables_representative'])

        for vars in config['variables_same']:
            same_sql = self.get_parameterized_sql(hge_ctx, query, vars)
            assert same_sql == representative_sql, (representative_sql, same_sql)

        for vars in config['variables_different']:
            different_sql = self.get_parameterized_sql(hge_ctx, query, vars)
            assert different_sql != representative_sql, (representative_sql, different_sql)

    def get_parameterized_sql(self, hge_ctx, query, variables):
        admin_secret = hge_ctx.hge_key
        headers = {}
        if admin_secret is not None:
            headers['X-Hasura-Admin-Secret'] = admin_secret

        request = { 'query': { 'query': query, 'variables': variables }, 'user': {} }
        status_code, response, _ = hge_ctx.anyq('/v1/graphql/explain', request, headers)
        assert status_code == 200, (request, status_code, response)

        sql = response['sql']
        assert isinstance(sql, str), response
        return sql
