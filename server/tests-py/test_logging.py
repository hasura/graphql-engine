#!/usr/bin/env python3

import json
import threading
import time

import pytest


def parse_logs(stream):
    logs = []

    def aggregate_logs():
        for line in stream:
            try:
                logs.append(json.loads(line.strip()))
            except:
                print('Failed to parse log line:')
                print(line)
                raise

    # We aggregate the logs in a separate thread because otherwise, this will
    # block forever. The thread will finish after the test finishes and HGE
    # is stopped.
    threading.Thread(target = aggregate_logs).start()
    time.sleep(1)

    return logs


pytestmark = [
    pytest.mark.capture_hge_logs,
    pytest.mark.admin_secret,
    pytest.mark.hge_env('HASURA_GRAPHQL_LOG_LEVEL', 'debug'),
]


@pytest.mark.hge_env('HASURA_GRAPHQL_ENABLED_LOG_TYPES', 'startup,http-log,query-log')
class TestLogging:
    dir = 'queries/logging'
    success_query = {'query': 'query { hello {code name} }'}

    @pytest.fixture(scope='class', autouse=True)
    def make_requests(self, hge_ctx):
        # setup some tables
        hge_ctx.v1q_f(self.dir + '/setup.yaml')

        # make a successful query
        q = self.success_query
        headers = {'x-request-id': 'successful-query-log-test'}
        if hge_ctx.hge_key:
            headers['x-hasura-admin-secret'] = hge_ctx.hge_key
        resp = hge_ctx.http.post(hge_ctx.hge_url + '/v1/graphql', json=q, headers=headers)
        assert resp.status_code == 200 and 'data' in resp.json()

        # make a query where JSON body parsing fails
        q = {'quer': 'query { hello {code name} }'}
        headers = {'x-request-id': 'json-parse-fail-log-test'}
        if hge_ctx.hge_key:
            headers['x-hasura-admin-secret'] = hge_ctx.hge_key
        resp = hge_ctx.http.post(hge_ctx.hge_url + '/v1/graphql', json=q, headers=headers)
        assert resp.status_code == 200 and 'errors' in resp.json()

        # make an unauthorized query where admin secret/access token is empty
        q = {'query': 'query { hello {code name} }'}
        headers = {'x-request-id': 'unauthorized-query-log-test'}
        resp = hge_ctx.http.post(hge_ctx.hge_url + '/v1/graphql', json=q, headers=headers)
        assert resp.status_code == 200 and 'errors' in resp.json()

        # make a successful "run SQL" query
        q = {'type': 'run_sql', 'args': {'source': 'default', 'sql': 'SELECT 1 AS one'}}
        headers = {'x-request-id': 'successful-run-sql-log-test'}
        if hge_ctx.hge_key:
            headers['x-hasura-admin-secret'] = hge_ctx.hge_key
        resp = hge_ctx.http.post(hge_ctx.hge_url + '/v2/query', json=q, headers=headers)
        assert resp.status_code == 200 and 'result' in resp.json()

        # make a failed "run SQL" query
        q = {'type': 'run_sql', 'args': {'source': 'default', 'sql': 'SELECT x FROM non_existent_table'}}
        headers = {'x-request-id': 'failed-run-sql-log-test'}
        if hge_ctx.hge_key:
            headers['x-hasura-admin-secret'] = hge_ctx.hge_key
        resp = hge_ctx.http.post(hge_ctx.hge_url + '/v2/query', json=q, headers=headers)
        assert resp.status_code == 400

        # make an unauthorized metadata request where admin secret/access token is empty
        q = {
        'query': {
            'type': 'select',
            'args': {
                    "table": {
                        "name": "hdb_function",
                        "schema": "hdb_catalog"
                    },
                    "columns": ["function_name", "function_schema", "is_system_defined"],
                    "where": { "function_schema": "public" }
                }
            }
        }
        headers = {'x-request-id': 'unauthorized-metadata-log-test'}
        resp = hge_ctx.http.post(hge_ctx.hge_url + '/v1/query', json=q,
                                 headers=headers)
        assert resp.status_code == 401 and 'error' in resp.json()

    @pytest.fixture(scope='class')
    def logs_from_requests(self, hge_server):
        return parse_logs(hge_server.stdout)

    def test_startup_logs(self, hge_ctx, logs_from_requests):
        def _get_server_config(x):
            return x['type'] == 'startup' and \
                'kind' in x['detail'] and \
                x['detail']['kind'] == 'server_configuration'

        config_logs = [l for l in logs_from_requests if _get_server_config(l)]
        print('config logs:', config_logs)
        assert len(config_logs) == 1
        config_log = config_logs[0]
        info = config_log['detail']['info']

        # we can improve this later by checking the actual value and
        # cross-checking with current config of the server
        assert 'enable_console' in info
        assert 'port' in info
        assert 'server_host' in info
        #assert 'transaction_isolation' in info
        assert 'admin_secret_set' in info
        if hge_ctx.hge_key:
            assert info['admin_secret_set'] == True
        assert 'auth_hook' in info
        assert 'auth_hook_mode' in info
        assert 'jwt_secret' in info
        assert 'unauth_role' in info
        assert 'cors_config' in info
        assert 'enable_console' in info
        assert 'console_assets_dir' in info
        assert 'enable_telemetry' in info
        assert 'use_prepared_statements' in info
        assert 'stringify_numeric_types' in info
        assert 'enabled_apis' in info
        assert 'live_query_options' in info
        assert 'enable_allowlist' in info
        assert 'enabled_log_types' in info

    def test_http_log(self, logs_from_requests):
        def _get_http_logs(x):
            return x['type'] == 'http-log'

        print('logs:', logs_from_requests)
        http_logs = [l for l in logs_from_requests if _get_http_logs(l)]
        print('http logs:', http_logs)
        assert len(http_logs) > 0
        for http_log in http_logs:
            print('log:', http_log)

            http_info = http_log['detail']['http_info']
            assert 'url' in http_info
            assert 'status' in http_info
            assert 'ip' in http_info

            operation = http_log['detail']['operation']
            assert 'request_id' in operation
            if operation['request_id'] in ['successful-query-log-test', 'successful-run-sql-log-test', 'failed-run-sql-log-test']:
                assert 'query_execution_time' in operation
            if operation['request_id'] == 'successful-query-log-test':
                assert 'user_vars' in operation
                # we should see the `query` field in successful operations
                assert 'query' in operation
                assert operation['query'] == self.success_query
                # there shouldn't be any raw_query in success
                assert operation.get('raw_query') is None

    def test_query_log(self, logs_from_requests):
        def _get_query_logs(x):
            return x['type'] == 'query-log'

        query_logs = [l for l in logs_from_requests if _get_query_logs(l)]
        assert len(query_logs) > 0
        onelog = query_logs[0]['detail']
        assert 'request_id' in onelog
        assert 'query' in onelog
        assert 'query' in onelog['query']
        assert 'generated_sql' in onelog

    def test_http_parse_failed_log(self, logs_from_requests):
        def _get_logs(x):
            return x['type'] == 'http-log' and \
                x['detail']['operation']['request_id'] == 'json-parse-fail-log-test'

        http_logs = [l for l in logs_from_requests if _get_logs(l)]
        print('parse failed logs:', http_logs)
        assert len(http_logs) > 0
        assert 'error' in http_logs[0]['detail']['operation']
        assert http_logs[0]['detail']['operation']['error']['code'] == 'parse-failed'

    def test_http_unauthorized_query(self, logs_from_requests):
        def _get_logs(x):
            return x['type'] == 'http-log' and \
                x['detail']['operation']['request_id'] == 'unauthorized-query-log-test'

        http_logs = [l for l in logs_from_requests if _get_logs(l)]
        print('unauthorized failed logs:', http_logs)
        assert len(http_logs) > 0
        assert 'error' in http_logs[0]['detail']['operation']
        assert http_logs[0]['detail']['operation']['error']['code'] == 'access-denied'
        assert http_logs[0]['detail']['operation'].get('query') is None
        assert http_logs[0]['detail']['operation']['raw_query'] is not None

    def test_successful_run_sql(self, logs_from_requests):
        def _get_logs(x):
            return x['type'] == 'http-log' and \
                x['detail']['operation']['request_id'] == 'successful-run-sql-log-test'

        http_logs = [l for l in logs_from_requests if _get_logs(l)]
        print('successful run SQL logs:', http_logs)
        assert len(http_logs) > 0
        assert http_logs[0]['detail']['operation']['query']['type'] == 'run_sql'

    def test_failed_run_sql(self, logs_from_requests):
        def _get_logs(x):
            return x['type'] == 'http-log' and \
                x['detail']['operation']['request_id'] == 'failed-run-sql-log-test'

        http_logs = [l for l in logs_from_requests if _get_logs(l)]
        print('failed run SQL logs:', http_logs)
        assert len(http_logs) > 0
        assert http_logs[0]['detail']['operation']['error']['code'] == 'postgres-error'
        assert http_logs[0]['detail']['operation']['query']['type'] == 'run_sql'

    def test_http_unauthorized_metadata(self, logs_from_requests):
        def _get_logs(x):
            return x['type'] == 'http-log' and \
                x['detail']['operation']['request_id'] == 'unauthorized-metadata-log-test'

        http_logs = [l for l in logs_from_requests if _get_logs(l)]
        print('unauthorized failed logs:', http_logs)
        assert len(http_logs) > 0
        assert 'error' in http_logs[0]['detail']['operation']
        assert http_logs[0]['detail']['operation']['error']['code'] == 'access-denied'
        assert "type" in http_logs[0]['detail']['operation'].get('query')
        # By default, 'raw_query' field is ignored for metadata queries. To allow
        # logging this field use the flag HASURA_GRAPHQL_ENABLE_METADATA_QUERY_LOGGING
        assert http_logs[0]['detail']['operation'].get('raw_query') is None


@pytest.mark.hge_env('HASURA_GRAPHQL_ENABLED_LOG_TYPES', 'websocket-log,query-log')
class TestWebsocketLogging:
    """
    Test logs emitted on websocket transport
    1. websocket-log
    2. ws-server
    """
    dir = 'queries/logging'
    query = {
        'query': 'query GetHello { hello {code name} }',
        'operationName': 'GetHello'
    }
    query_id = 'successful-ws-log-test'

    @pytest.fixture(scope='class', autouse=True)
    def make_requests(self, hge_ctx, ws_client):
        '''
            Create connection using connection_init
        '''
        headers = {'x-request-id': self.query_id}
        if hge_ctx.hge_key:
            headers['x-hasura-admin-secret'] = hge_ctx.hge_key
        ws_client.init(headers=headers)
        # setup some tables
        hge_ctx.v1q_f(self.dir + '/setup.yaml')

        # make a successful websocket query
        resp = ws_client.send_query(self.query, query_id=self.query_id, timeout=5)
        try:
            ev = next(resp)
            assert ev['type'] == 'data' and ev['id'] == self.query_id, ev
        finally:
            ws_client.stop(self.query_id)

    @pytest.fixture(scope='class')
    def logs_from_requests(self, hge_server):
        return parse_logs(hge_server.stdout)

    def test_websocket_log(self, logs_from_requests):
        """
        tests for the `websocket-log` type. currently tests presence of operation_name
        """
        def _get_websocket_operation_logs(x):
            return x['type'] == 'websocket-log' and x['detail']['event']['type'] == 'operation'

        ws_logs = list(filter(_get_websocket_operation_logs, logs_from_requests))
        assert len(ws_logs) > 0
        onelog = ws_logs[0]['detail']['event']['detail']
        assert 'request_id' in onelog
        assert 'operation_name' in onelog
        assert 'query' in onelog
        assert 'query' in onelog['query']

    def test_ws_server_log(self, logs_from_requests):
        """
        tests for the `websocket-log` type. currently tests presence of operation_name
        """
        def _get_ws_server_logs(x):
            return x['type'] == 'ws-server' and 'metadata' in x['detail'] and type(x['detail']) != str

        ws_logs = list(filter(_get_ws_server_logs, logs_from_requests))
        assert len(ws_logs) > 0
        onelog = ws_logs[0]['detail']
        assert 'operation_id' in onelog['metadata']
        assert 'operation_name' in onelog['metadata']


@pytest.mark.jwk_path('/jwk-cache-control?no-cache=true')
class AbstractTestJwkRefreshLog:
    dir = 'queries/logging'
    success_query = {'query': 'query { hello {code name} }'}

    @pytest.fixture(scope='class', autouse=True)
    def make_requests(self, hge_ctx, jwk_server_url):
        # setup some tables
        hge_ctx.v1q_f(self.dir + '/setup.yaml')

        # make a successful query
        q = self.success_query
        headers = {'x-request-id': 'successful-query-log-test'}
        if hge_ctx.hge_key:
            headers['x-hasura-admin-secret'] = hge_ctx.hge_key
        resp = hge_ctx.http.post(hge_ctx.hge_url + '/v1/graphql', json=q,
                                 headers=headers)
        assert resp.status_code == 200 and 'data' in resp.json()

    @pytest.fixture(scope='class')
    def logs_from_requests(self, hge_server):
        return parse_logs(hge_server.stdout)


# Test that the JWK refresh log can be enabled
@pytest.mark.hge_env('HASURA_GRAPHQL_ENABLED_LOG_TYPES', 'http-log,jwk-refresh-log')
class TestJwkRefreshLog(AbstractTestJwkRefreshLog):
    def test_jwk_refresh_log(self, logs_from_requests):
        def _get_jwk_refresh_log(x):
            return x['type'] == 'jwk-refresh-log'
        jwk_refresh_logs = list(filter(_get_jwk_refresh_log, logs_from_requests))
        assert len(jwk_refresh_logs) > 0


# Test that the JWK refresh log can be disabled
@pytest.mark.hge_env('HASURA_GRAPHQL_ENABLED_LOG_TYPES', 'http-log')
class TestNoJwkRefreshLog(AbstractTestJwkRefreshLog):
    def test_jwk_refresh_log(self, logs_from_requests):
        def _get_jwk_refresh_log(x):
            return x['type'] == 'jwk-refresh-log'
        jwk_refresh_logs = list(filter(_get_jwk_refresh_log, logs_from_requests))
        assert len(jwk_refresh_logs) == 0
