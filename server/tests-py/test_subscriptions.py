from collections import OrderedDict
import json
import pytest
import queue
from ruamel.yaml import YAML
import time
import uuid

from validate import check_query_f
from utils import insert_many

yaml=YAML(typ='safe', pure=True)

usefixtures = pytest.mark.usefixtures

@pytest.fixture(scope='class')
def ws_conn_init(hge_key, ws_client):
    init_ws_conn(hge_key, ws_client)

@pytest.fixture(scope='class')
def ws_conn_init_graphql_ws(hge_key, ws_client_graphql_ws):
    init_graphql_ws_conn(hge_key, ws_client_graphql_ws)

'''
    Refer: https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_connection_init
'''

# This is used in other test files! Be careful when modifying it.
def init_ws_conn(hge_key, ws_client, payload = None):
    init_msg = {
        'type': 'connection_init',
        'payload': payload or ws_payload(hge_key),
    }
    ws_client.send(init_msg)
    ev = ws_client.get_ws_event(3)
    assert ev['type'] == 'connection_ack', ev

def init_graphql_ws_conn(hge_key, ws_client_graphql_ws):
    init_msg = {
        'type': 'connection_init',
        'payload': ws_payload(hge_key),
    }
    ws_client_graphql_ws.send(init_msg)
    ev = ws_client_graphql_ws.get_ws_event(3)
    assert ev['type'] == 'connection_ack', ev

def ws_payload(hge_key):
    if hge_key is not None:
        return {
            'headers': {
                'X-Hasura-Admin-Secret': hge_key,
            }
        }
    else:
        return {}

def get_explain_graphql_query_response(hge_ctx, hge_key, query, variables, user_headers = {}):
    headers = {}
    if hge_key is not None:
        headers['X-Hasura-Admin-Secret'] = hge_key

    request = { 'query': { 'query': query, 'variables': variables }, 'user': user_headers }
    status_code, response, _ = hge_ctx.anyq('/v1/graphql/explain', request, headers)
    assert status_code == 200, (request, status_code, response)
    return response

@pytest.mark.no_admin_secret
class TestSubscriptionCtrlWithoutSecret(object):
    def test_connection(self, ws_client):
        ws_client.recreate_conn()
        init_ws_conn(None, ws_client)

        obj = {
            'type': 'connection_terminate'
        }
        ws_client.send(obj)
        with pytest.raises(queue.Empty):
            ws_client.get_ws_event(3)

@pytest.mark.admin_secret
class TestSubscriptionCtrl(object):
    '''
    References:
    https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_connection_init
    https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_connection_terminate
    '''

    def test_connection(self, hge_key, ws_client):
        ws_client.recreate_conn()
        init_ws_conn(hge_key, ws_client)

        obj = {
            'type': 'connection_terminate'
        }
        ws_client.send(obj)
        with pytest.raises(queue.Empty):
            ws_client.get_ws_event(3)

@pytest.mark.admin_secret
# TODO: remove once parallelization work is completed
#       only used when running HGE outside the test suite
@pytest.mark.requires_an_admin_secret
class TestSubscriptionBasicNoAuth:

    def test_closed_connection_apollo(self, ws_client):
        # sends empty header so that there is not authentication present in the test
        init_msg = {
            'type': 'connection_init',
            'payload':{'headers':{}}
        }
        ws_client.send(init_msg)
        time.sleep(2)
        ev = ws_client.get_conn_close_state()
        assert ev == True, ev

    def test_closed_connection_graphql_ws(self, ws_client_graphql_ws):
        # sends empty header so that there is not authentication present in the test
        init_msg = {
            'type': 'connection_init',
            'payload':{'headers':{}}
        }
        ws_client_graphql_ws.send(init_msg)
        time.sleep(2)
        ev = ws_client_graphql_ws.get_conn_close_state()
        assert ev == True, ev

@pytest.mark.backend('mssql', 'postgres')
@usefixtures('per_class_tests_db_state', 'ws_conn_init')
@pytest.mark.admin_secret
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
        id = str(uuid.uuid4())
        query = """
        subscription {
        hge_tests_test_t1(order_by: {c1: desc}, limit: 1) {
            c1,
            c2
          }
        }
        """
        obj = {
            'id': id,
            'payload': {
                'query': query
            },
            'type': 'start'
        }
        ws_client.send(obj)
        '''
            Refer: https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_data
        '''
        ev = ws_client.get_ws_query_event(id, 15)
        assert ev['type'] == 'data' and ev['id'] == id, ev

    '''
        Refer https://github.com/apollographql/subscriptions-transport-ws/blob/01e0b2b65df07c52f5831cce5c858966ba095993/src/server.ts#L306
    '''
    @pytest.mark.skip(reason="refer to https://github.com/hasura/graphql-engine/pull/387#issuecomment-421343098")
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

    def test_complete(self, ws_client):
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
@pytest.mark.admin_secret
class TestSubscriptionBasicGraphQLWS:
    @classmethod
    def dir(cls):
        return 'queries/subscriptions/basic'

    @pytest.mark.parametrize("transport", ['http', 'websocket', 'subscription'])
    def test_negative(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/negative_test.yaml', transport, gqlws=True)

    def test_connection_error(self, hge_key, ws_client_graphql_ws):
        if ws_client_graphql_ws.get_conn_close_state():
            ws_client_graphql_ws.create_conn()
            if hge_key == None:
                ws_client_graphql_ws.init()
            else:
                ws_client_graphql_ws.init_as_admin()
        ws_client_graphql_ws.send({'type': 'test'})
        time.sleep(2)
        ev = ws_client_graphql_ws.get_conn_close_state()
        assert ev == True, ev

    def test_start(self, hge_key, ws_client_graphql_ws):
        if ws_client_graphql_ws.get_conn_close_state():
            ws_client_graphql_ws.create_conn()
            if hge_key == None:
                ws_client_graphql_ws.init()
            else:
                ws_client_graphql_ws.init_as_admin()

        id = str(uuid.uuid4())
        query = """
        subscription {
        hge_tests_test_t1(order_by: {c1: desc}, limit: 1) {
            c1,
            c2
          }
        }
        """
        obj = {
            'id': id,
            'payload': {
                'query': query
            },
            'type': 'subscribe'
        }
        ws_client_graphql_ws.send(obj)
        ev = ws_client_graphql_ws.get_ws_query_event(id, 15)
        assert ev['type'] == 'next' and ev['id'] == id, ev

    @pytest.mark.skip(reason="refer to https://github.com/hasura/graphql-engine/pull/387#issuecomment-421343098")
    def test_start_duplicate(self, hge_key, ws_client_graphql_ws):
        if ws_client_graphql_ws.get_conn_close_state():
            ws_client_graphql_ws.create_conn()
            if hge_key == None:
                ws_client_graphql_ws.init()
            else:
                ws_client_graphql_ws.init_as_admin()
        self.test_start(hge_key, ws_client_graphql_ws)

    def test_stop_without_id(self, hge_key, ws_client_graphql_ws):
        if ws_client_graphql_ws.get_conn_close_state():
            ws_client_graphql_ws.create_conn()
            if hge_key == None:
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

    def test_stop(self, hge_key, ws_client_graphql_ws):
        if ws_client_graphql_ws.get_conn_close_state():
            ws_client_graphql_ws.create_conn()
            if hge_key == None:
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

    def test_start_after_stop(self, hge_key, hge_ctx, ws_client_graphql_ws):
        if ws_client_graphql_ws.get_conn_close_state():
            ws_client_graphql_ws.create_conn()
            if hge_key == None:
                ws_client_graphql_ws.init()
            else:
                ws_client_graphql_ws.init_as_admin()
        self.test_start(hge_ctx, ws_client_graphql_ws)
        ## NOTE: test_start leaves a message in the queue, hence clearing it
        if len(ws_client_graphql_ws.get_queue()) > 0:
            ws_client_graphql_ws.clear_queue()
        self.test_stop(hge_ctx, ws_client_graphql_ws)

    def test_complete(self, hge_key, ws_client_graphql_ws):
        id = str(uuid.uuid4())
        if ws_client_graphql_ws.get_conn_close_state():
            ws_client_graphql_ws.create_conn()
            if hge_key == None:
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
            'id': id,
            'payload': {
                'query': query
            },
            'type': 'subscribe'
        }
        ws_client_graphql_ws.send(obj)
        ev = ws_client_graphql_ws.get_ws_query_event(id, 3)
        assert ev['type'] == 'next' and ev['id'] == id, ev
        # Check for complete type
        ev = ws_client_graphql_ws.get_ws_query_event(id, 3)
        assert ev['type'] == 'complete' and ev['id'] == id, ev

@usefixtures('per_method_tests_db_state','ws_conn_init')
@pytest.mark.admin_secret
class TestSubscriptionLiveQueries:

    @classmethod
    def dir(cls):
        return 'queries/subscriptions/live_queries'

    def test_live_queries(self, hge_key, ws_client):
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
            if hge_key is not None:
                headers['X-Hasura-Admin-Secret'] = hge_key
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

@usefixtures('per_method_tests_db_state', 'ws_conn_init')
@pytest.mark.hge_env('HASURA_GRAPHQL_EXPERIMENTAL_FEATURES', 'streaming_subscriptions')
class TestStreamingSubscription:

    @classmethod
    def dir(cls):
        return 'queries/subscriptions/streaming'

    def test_basic_streaming_subscription_existing_static_data(self, hge_key, hge_ctx, ws_client):
        '''
            Create connection using connection_init
        '''
        ws_client.init_as_admin()

        query = """
        subscription ($batch_size: Int!) {
          hge_tests_stream_query: hge_tests_articles_stream(cursor: {initial_value: {id: 0}}, batch_size: $batch_size) {
             id
             title
          }
        }
        """

        liveQs = []
        headers={}
        articles_to_insert = []
        for i in range(10):
            articles_to_insert.append({"id": i + 1, "title": "Article title {}".format(i + 1)})
        insert_many(hge_ctx, {"schema": "hge_tests", "name": "articles"}, articles_to_insert)
        if hge_key is not None:
            headers['X-Hasura-Admin-Secret'] = hge_key
        subscrPayload = { 'query': query, 'variables': { 'batch_size': 2 } }
        respLive = ws_client.send_query(subscrPayload, query_id='stream_1', headers=headers, timeout=15)
        liveQs.append(respLive)
        for idx in range(5):
          ev = next(respLive)
          assert ev['type'] == 'data', ev
          assert ev['id'] == 'stream_1', ev
          # fetching two rows per batch
          expected_payload = [ {"id": 2*idx+1, "title": "Article title {}".format(2*idx+1)}, {"id": 2*idx+2, "title": "Article title {}".format(2*idx+2)}]
          assert ev['payload']['data'] == {'hge_tests_stream_query': expected_payload}, ev['payload']['data']

        # stop the streaming subscription
        frame = {
            'id': 'stream_1',
            'type': 'stop'
        }
        ws_client.send(frame)

        with pytest.raises(queue.Empty):
            ev = ws_client.get_ws_event(3)

    def test_streaming_subscriptions_with_concurrent_data_inserts(self, ws_client):
        '''
            Create connection using connection_init
        '''
        ws_client.init_as_admin()
        headers={}
        query = """
        subscription ($batch_size: Int!, $initial_created_at: timestamptz!) {
          hge_tests_stream_query: hge_tests_test_t2_stream(cursor: [{initial_value: {created_at: $initial_created_at}, ordering: ASC}], batch_size: $batch_size) {
             c1
             c2
          }
        }
        """

        with open(self.dir() + "/steps.yaml") as c:
            conf = yaml.load(c)

        subscrPayload = { 'query': query, 'variables': { 'batch_size': 2, 'initial_created_at': "2020-01-01" } }
        respLive = ws_client.send_query(subscrPayload, query_id='stream_1', headers=headers, timeout=15)

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

            ev = next(respLive)
            assert ev['type'] == 'data', ev
            assert ev['id'] == 'stream_1', ev

            expectedReturnedResponse = json.loads(step['stream_response'])
            expectedLiveResponse = { 'hge_tests_stream_query' : expectedReturnedResponse }

            assert ev['payload']['data'] == expectedLiveResponse, ev['payload']['data']

        # stop the streaming subscription
        frame = {
            'id': 'stream_1',
            'type': 'stop'
        }
        ws_client.send(frame)

        with pytest.raises(queue.Empty):
            ev = ws_client.get_ws_event(3)

    def test_streaming_subscription_with_custom_name_set_for_cursor(self, hge_key, ws_client):
        '''
            Create connection using connection_init
        '''
        ws_client.init_as_admin()

        query = """
        subscription ($batch_size: Int!) {
          hge_tests_stream_query: hge_tests_users_stream(cursor: {initial_value: {userId: 0}}, batch_size: $batch_size) {
             userId
             name
          }
        }
        """

        liveQs = []
        headers={}
        if hge_key is not None:
            headers['X-Hasura-Admin-Secret'] = hge_key
        subscrPayload = { 'query': query, 'variables': { 'batch_size': 1 } }
        respLive = ws_client.send_query(subscrPayload, query_id='stream_1', headers=headers, timeout=15)
        liveQs.append(respLive)
        for idx in range(2):
          ev = next(respLive)
          assert ev['type'] == 'data', ev
          assert ev['id'] == 'stream_1', ev
          # fetching two rows per batch
          expected_payload = [ {"userId": idx + 1, "name": "Name {}".format(idx+1)}]
          assert ev['payload']['data'] == {'hge_tests_stream_query': expected_payload}, ev['payload']['data']

        # stop the streaming subscription
        frame = {
            'id': 'stream_1',
            'type': 'stop'
        }
        ws_client.send(frame)

        with pytest.raises(queue.Empty):
            ev = ws_client.get_ws_event(3)

@usefixtures('per_method_tests_db_state', 'ws_conn_init_graphql_ws')
@pytest.mark.admin_secret
class TestSubscriptionLiveQueriesForGraphQLWS:

    @classmethod
    def dir(cls):
        return 'queries/subscriptions/live_queries'

    def test_live_queries(self, hge_key, ws_client_graphql_ws):
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
            if hge_key is not None:
                headers['X-Hasura-Admin-Secret'] = hge_key
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

@pytest.mark.backend('mssql', 'postgres')
@usefixtures('per_class_tests_db_state')
@pytest.mark.admin_secret
class TestSubscriptionMultiplexingPostgresMSSQL:

    @classmethod
    def dir(cls):
        return 'queries/subscriptions/multiplexing'

    def test_extraneous_session_variables_are_discarded_from_query(self, hge_key, hge_ctx):
        with open(self.dir() + '/articles_query.yaml') as c:
            config = yaml.load(c)

        query = config['query']
        session_variables = {
                "X-Hasura-Role":"public",
                "X-Hasura-User-Id":"1"      # extraneous session variable
        }
        response = get_explain_graphql_query_response(hge_ctx, hge_key, query, {}, session_variables)
        # The input session variables should be ignored because the only check for the role is
        # if `is_public` is `true`
        assert response["variables"]["session"] == {}, response["variables"]

        session_variables = {
                "X-Hasura-Role":"user",
                "X-Hasura-User-Id":"1",
                "X-Hasura-Allowed-Ids":"{1,3,4}" # extraneous session variable
        }
        response = get_explain_graphql_query_response(hge_ctx, hge_key, query, {}, session_variables)
        # The input session variable should not be ignored because the `user` role can only
        # select those roles where `user_id = X-Hasura-User-Id`
        assert response["variables"]["session"] == {'x-hasura-user-id':"1"}, response["variables"]

# test case for https://github.com/hasura/graphql-engine-mono/issues/3689
@usefixtures('per_class_tests_db_state')
@pytest.mark.admin_secret
class TestSubscriptionMultiplexingPostgres:

    @classmethod
    def dir(cls):
        return 'queries/subscriptions/multiplexing'

    def test_simple_variables_are_parameterized(self, hge_key, hge_ctx):
        with open(self.dir() + '/articles_query_simple_variable.yaml') as c:
            config = yaml.load(c)

        response = get_explain_graphql_query_response(hge_ctx, hge_key, config['query'], config['variables'], {})
        assert response["variables"]["synthetic"] == ['1'], response["variables"]

    def test_array_variables_are_parameterized(self, hge_key, hge_ctx):
        with open(self.dir() + '/articles_query_array_variable.yaml') as c:
            config = yaml.load(c)

        response = get_explain_graphql_query_response(hge_ctx, hge_key, config['query'], config['variables'], {})
        assert response["variables"]["synthetic"] == ['{1,2,3}'], response["variables"]

@pytest.mark.backend('postgres')
@usefixtures('per_class_tests_db_state', 'ws_conn_init')
@pytest.mark.admin_secret
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

    def test_user_defined_function_with_session_argument(self, hge_key, ws_client):
        ws_client.init_as_admin()
        headers = {'x-hasura-role': 'user', 'x-hasura-user-id': '42'}
        if hge_key is not None:
            headers['X-Hasura-Admin-Secret'] = hge_key
        payload = {'query': self.query}
        resp = ws_client.send_query(payload, headers=headers, timeout=15)
        ev = next(resp)
        assert ev['type'] == 'data', ev
        assert ev['payload']['data'] == {'me': [{'id': '42', 'name': 'Charlie'}]}, ev['payload']['data']

@pytest.fixture(scope='class')
def add_customized_source(current_backend, add_source, hge_ctx):
    customization = {
        'root_fields': {
            'namespace': 'my_source',
            'prefix': 'fpref_',
            'suffix': '_fsuff',
        },
        'type_names': {
            'prefix': 'tpref_',
            'suffix': '_tsuff',
        }
    }
    if current_backend == 'mssql':
        hge_ctx.v1metadataq({
            'type': 'mssql_add_source',
            'args': {
                'name': 'mssql1',
                'configuration': {
                    'connection_info': {
                        'database_url': {
                            'from_env': 'HASURA_GRAPHQL_MSSQL_SOURCE_URL',
                        },
                    },
                },
                'customization': customization,
            },
        })
    else:
        add_source('pg1', customization=customization)

@pytest.mark.backend('mssql', 'postgres')
@usefixtures('add_customized_source', 'per_class_tests_db_state', 'ws_conn_init')
@pytest.mark.admin_secret
class TestSubscriptionCustomizedSourceMSSQLPostgres:
    @classmethod
    def dir(cls):
        return 'queries/subscriptions/customized_source'

    setup_metadata_api_version = "v2"

    '''
        Refer: https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_complete
    '''

    def test_complete(self, ws_client):
        id = str(uuid.uuid4())
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
            'id': id,
            'payload': {
                'query': query
            },
            'type': 'start'
        }
        ws_client.send(obj)
        '''
            Refer: https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md#gql_data
        '''
        ev = ws_client.get_ws_query_event(id, 15)
        assert ev['type'] == 'data' and ev['id'] == id, ev
        assert ev['payload']['data']['a'] == OrderedDict([('b', [OrderedDict([('id', 1), ('name', 'Author 1')]), OrderedDict([('id', 2), ('name', 'Author 2')])])]), ev

    def test_double_alias(self, ws_client):
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

@pytest.mark.backend('mssql')
@usefixtures('per_class_tests_db_state', 'ws_conn_init')
@pytest.mark.admin_secret
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
