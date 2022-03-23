#!/usr/bin/env python3

import time
import pytest
import json
import queue
from validate import check_query_f
from collections import OrderedDict
from ruamel.yaml import YAML

usefixtures = pytest.mark.usefixtures
yaml=YAML(typ='safe', pure=True)

@pytest.fixture(scope='class')
def ws_conn_init(hge_ctx, ws_client):
    init_ws_conn(hge_ctx, ws_client)

@pytest.fixture(scope='class')
def ws_conn_init_graphql_ws(hge_ctx, ws_client_graphql_ws):
    init_graphql_ws_conn(hge_ctx, ws_client_graphql_ws)

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

def init_graphql_ws_conn(hge_ctx, ws_client_graphql_ws, payload = None):
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
    ws_client_graphql_ws.send(init_msg)
    ev = ws_client_graphql_ws.get_ws_event(3)
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

@pytest.mark.parametrize("backend", ['mssql', 'postgres'])
@usefixtures('per_class_tests_db_state', 'ws_conn_init')
class TestSubscriptionBasic:
    @classmethod
    def dir(cls):
        return 'queries/subscriptions/basic'

    @pytest.mark.parametrize("transport", ['http', 'websocket', 'subscription'])
    def test_negative(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/negative_test.yaml', transport)

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

## NOTE: The same tests as in TestSubscriptionBasic but with
##       the subscription transport being used is `graphql-ws`
## FIXME: There's an issue with the tests being parametrized with both
##        postgres and mssql data sources enabled(See issue #2084).
@usefixtures('per_method_tests_db_state', 'ws_conn_init_graphql_ws')
class TestSubscriptionBasicGraphQLWS:
    @classmethod
    def dir(cls):
        return 'queries/subscriptions/basic'

    @pytest.mark.parametrize("transport", ['http', 'websocket', 'subscription'])
    def test_negative(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/negative_test.yaml', transport, gqlws=True)

    def test_connection_error(self, hge_ctx, ws_client_graphql_ws):
        if ws_client_graphql_ws.get_conn_close_state():
            ws_client_graphql_ws.create_conn()
            if hge_ctx.hge_key == None:
                ws_client_graphql_ws.init()
            else:
                ws_client_graphql_ws.init_as_admin()
        ws_client_graphql_ws.send({'type': 'test'})
        time.sleep(2)
        ev = ws_client_graphql_ws.get_conn_close_state()
        assert ev == True, ev

    def test_start(self, hge_ctx, ws_client_graphql_ws):
        if ws_client_graphql_ws.get_conn_close_state():
            ws_client_graphql_ws.create_conn()
            if hge_ctx.hge_key == None:
                ws_client_graphql_ws.init()
            else:
                ws_client_graphql_ws.init_as_admin()
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
            'type': 'subscribe'
        }
        ws_client_graphql_ws.send(obj)
        ev = ws_client_graphql_ws.get_ws_query_event('1',15)
        assert ev['type'] == 'next' and ev['id'] == '1', ev

    @pytest.mark.skip(reason="refer https://github.com/hasura/graphql-engine/pull/387#issuecomment-421343098")
    def test_start_duplicate(self, hge_ctx, ws_client_graphql_ws):
        if ws_client_graphql_ws.get_conn_close_state():
            ws_client_graphql_ws.create_conn()
            if hge_ctx.hge_key == None:
                ws_client_graphql_ws.init()
            else:
                ws_client_graphql_ws.init_as_admin()
        self.test_start(ws_client_graphql_ws)

    def test_stop_without_id(self, hge_ctx, ws_client_graphql_ws):
        if ws_client_graphql_ws.get_conn_close_state():
            ws_client_graphql_ws.create_conn()
            if hge_ctx.hge_key == None:
                ws_client_graphql_ws.init()
            else:
                ws_client_graphql_ws.init_as_admin()
        obj = {
            'type': 'complete'
        }
        ws_client_graphql_ws.send(obj)
        time.sleep(2)
        ev = ws_client_graphql_ws.get_conn_close_state()
        assert ev == True, ev

    def test_stop(self, hge_ctx, ws_client_graphql_ws):
        if ws_client_graphql_ws.get_conn_close_state():
            ws_client_graphql_ws.create_conn()
            if hge_ctx.hge_key == None:
                ws_client_graphql_ws.init()
            else:
                ws_client_graphql_ws.init_as_admin()
        obj = {
            'type': 'complete',
            'id': '1'
        }
        ws_client_graphql_ws.send(obj)
        time.sleep(2)
        with pytest.raises(queue.Empty):
            ev = ws_client_graphql_ws.get_ws_event(3)

    def test_start_after_stop(self, hge_ctx, ws_client_graphql_ws):
        if ws_client_graphql_ws.get_conn_close_state():
            ws_client_graphql_ws.create_conn()
            if hge_ctx.hge_key == None:
                ws_client_graphql_ws.init()
            else:
                ws_client_graphql_ws.init_as_admin()
        self.test_start(hge_ctx, ws_client_graphql_ws)
        ## NOTE: test_start leaves a message in the queue, hence clearing it
        if len(ws_client_graphql_ws.get_queue()) > 0:
            ws_client_graphql_ws.clear_queue()
        self.test_stop(hge_ctx, ws_client_graphql_ws)

    def test_complete(self, hge_ctx, ws_client_graphql_ws):
        if ws_client_graphql_ws.get_conn_close_state():
            ws_client_graphql_ws.create_conn()
            if hge_ctx.hge_key == None:
                ws_client_graphql_ws.init()
            else:
                ws_client_graphql_ws.init_as_admin()
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
            'type': 'subscribe'
        }
        ws_client_graphql_ws.send(obj)
        ev = ws_client_graphql_ws.get_ws_query_event('2',3)
        assert ev['type'] == 'next' and ev['id'] == '2', ev
        # Check for complete type
        ev = ws_client_graphql_ws.get_ws_query_event('2',3)
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
            conf = yaml.load(c)

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

@usefixtures('per_method_tests_db_state','ws_conn_init_graphql_ws')
class TestSubscriptionLiveQueriesForGraphQLWS:

    @classmethod
    def dir(cls):
        return 'queries/subscriptions/live_queries'

    def test_live_queries(self, hge_ctx, ws_client_graphql_ws):
        '''
            Create connection using connection_init
        '''
        ws_client_graphql_ws.init_as_admin()

        with open(self.dir() + "/steps.yaml") as c:
            conf = yaml.load(c)

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
            respLive = ws_client_graphql_ws.send_query(subscrPayload, query_id='live_'+str(i), headers=headers, timeout=15)
            liveQs.append(respLive)
            ev = next(respLive)
            assert ev['type'] == 'next', ev
            assert ev['id'] == 'live_' + str(i), ev
            assert ev['payload']['data'] == {'hge_tests_live_query_'+str(i): []}, ev['payload']['data']

        assert isinstance(conf, list) == True, 'Not an list'
        for index, step in enumerate(conf):
            mutationPayload = { 'query': step['query'] }
            if 'variables' in step and step['variables']:
                mutationPayload['variables'] = json.loads(step['variables'])

            expected_resp = json.loads(step['response'])

            mutResp = ws_client_graphql_ws.send_query(mutationPayload,'mutation_'+str(index),timeout=15)
            ev = next(mutResp)
            assert ev['type'] == 'next' and ev['id'] == 'mutation_'+str(index), ev
            assert ev['payload']['data'] == expected_resp, ev['payload']['data']

            ev = next(mutResp)
            assert ev['type'] == 'complete' and ev['id'] == 'mutation_'+str(index), ev

            for (i, resultLimit), respLive in zip(queries, liveQs):
                ev = next(respLive)
                assert ev['type'] == 'next', ev
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
                'type': 'complete'
            }
            ws_client_graphql_ws.send(frame)
            ws_client_graphql_ws.clear_queue()

@pytest.mark.parametrize("backend", ['mssql', 'postgres'])
@usefixtures('per_class_tests_db_state')
class TestSubscriptionMultiplexing:

    @classmethod
    def dir(cls):
        return 'queries/subscriptions/multiplexing'

    def test_extraneous_session_variables_are_discarded_from_query(self, hge_ctx):
        with open(self.dir() + '/articles_query.yaml') as c:
            config = yaml.load(c)

        query = config['query']
        session_variables = {
                "X-Hasura-Role":"public",
                "X-Hasura-User-Id":"1"      # extraneous session variable
        }
        response = self.get_explain_graphql_query_response(hge_ctx, query, {}, session_variables)
        # The input session variables should be ignored because the only check for the role is
        # if `is_public` is `true`
        assert response["variables"]["session"] == {}, response["variables"]

        session_variables = {
                "X-Hasura-Role":"user",
                "X-Hasura-User-Id":"1",
                "X-Hasura-Allowed-Ids":"{1,3,4}" # extraneous session variable
        }
        response = self.get_explain_graphql_query_response(hge_ctx, query, {}, session_variables)
        # The input session variable should not be ignored because the `user` role can only
        # select those roles where `user_id = X-Hasura-User-Id`
        assert response["variables"]["session"] == {'x-hasura-user-id':"1"}, response["variables"]

    def get_explain_graphql_query_response(self, hge_ctx, query, variables, user_headers = {}):
        admin_secret = hge_ctx.hge_key
        headers = {}
        if admin_secret is not None:
            headers['X-Hasura-Admin-Secret'] = admin_secret

        request = { 'query': { 'query': query, 'variables': variables }, 'user': user_headers }
        status_code, response, _ = hge_ctx.anyq('/v1/graphql/explain', request, headers)
        assert status_code == 200, (request, status_code, response)
        return response


@pytest.mark.parametrize("backend", ['postgres'])
@usefixtures('per_class_tests_db_state', 'ws_conn_init')
class TestSubscriptionUDFWithSessionArg:
    """
    Test a user-defined function which uses the entire session variables as argument
    """

    query = """
      subscription {
        me {
          id
          name
        }
      }
    """

    @classmethod
    def dir(cls):
        return 'queries/subscriptions/udf_session_args'

    def test_user_defined_function_with_session_argument(self, hge_ctx, ws_client):
        ws_client.init_as_admin()
        headers = {'x-hasura-role': 'user', 'x-hasura-user-id': '42'}
        if hge_ctx.hge_key is not None:
            headers['X-Hasura-Admin-Secret'] = hge_ctx.hge_key
        payload = {'query': self.query}
        resp = ws_client.send_query(payload, headers=headers, timeout=15)
        ev = next(resp)
        assert ev['type'] == 'data', ev
        assert ev['payload']['data'] == {'me': [{'id': '42', 'name': 'Charlie'}]}, ev['payload']['data']

@pytest.mark.parametrize("backend", ['mssql', 'postgres'])
@usefixtures('per_class_tests_db_state', 'ws_conn_init')
class TestSubscriptionCustomizedSourceCommon:
    @classmethod
    def dir(cls):
        return 'queries/subscriptions/customized_source'

    setup_metadata_api_version = "v2"

    '''
        Refer: https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_complete
    '''

    def test_complete(self, hge_ctx, ws_client):
        query = """
        subscription MySubscription {
            a: my_source {
                b: fpref_author_fsuff {
                    id
                    name
                }
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
        assert ev['payload']['data']['a'] == OrderedDict([('b', [OrderedDict([('id', 1), ('name', 'Author 1')]), OrderedDict([('id', 2), ('name', 'Author 2')])])]), ev

    def test_double_alias(self, hge_ctx, ws_client):
        '''
        This should give an error even though @_multiple_top_level_fields is specified.
        The two different aliases for `my_source` mean that we would have to wrap different
        parts of the DB response in different namespace fields, which is not currently possible.
        '''
        query = """
        subscription MySubscription @_multiple_top_level_fields {
            alias1: my_source {
                fpref_author_fsuff {
                    id
                    name
                }
            }
            alias2: my_source {
                fpref_author_fsuff {
                    id
                    name
                }
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
        '''
            Refer: https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_data
        '''
        ev = ws_client.get_ws_query_event('2',15)
        assert ev['type'] == 'error' and ev['id'] == '2', ev
        assert ev['payload']['errors'] == [OrderedDict([('extensions', OrderedDict([('path', '$'), ('code', 'validation-failed')])), ('message', 'subscriptions must select one top level field')])], ev

@pytest.mark.parametrize("backend", ['mssql'])
@usefixtures('per_class_tests_db_state', 'ws_conn_init')
class TestSubscriptionMSSQLChunkedResults:
    @classmethod
    def dir(cls):
        return 'queries/subscriptions/mssql'

    query = """
     subscription {
       hge_tests_test_subscriptions {
         field1
       }
     }
    """

    def test_chunked_results(self, ws_client):
        obj = {
            'id': '1',
            'payload': {
                'query': self.query
            },
            'type': 'start'
        }
        ws_client.send(obj)
        ev = ws_client.get_ws_query_event('1',15)
        assert ev['type'] == 'data' and ev['id'] == '1', ev
        assert not "errors" in ev['payload'], ev
