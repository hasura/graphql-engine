#!/usr/bin/env python3

import json
import os
import time

import pytest
from context import PytestConf

if not PytestConf.config.getoption("--test-logging"):
    pytest.skip("--test-logging missing, skipping tests", allow_module_level=True)

def parse_logs():
    # parse the log file into a json list
    log_file = os.getenv('LOGGING_TEST_LOGFILE_PATH', None)
    if not log_file:
        print('Could not determine log file path to test logging!')
        assert False
    loglines = []
    with open(log_file, 'r') as f:
        loglines = f.readlines()
    logs = list(map(lambda x: json.loads(x.strip()), loglines))
    assert len(logs) > 0
    return logs

class TestLogging():
    dir = 'queries/logging'
    success_query = {'query': 'query { hello {code name} }'}

    def _teardown(self, hge_ctx):
        hge_ctx.v1q_f(self.dir + '/teardown.yaml')

    @pytest.fixture(autouse=True)
    def transact(self, hge_ctx):
        # setup some tables
        hge_ctx.v1q_f(self.dir + '/setup.yaml')

        try:
            # make a successful query
            q = self.success_query
            headers = {'x-request-id': 'successful-query-log-test'}
            if hge_ctx.hge_key:
                headers['x-hasura-admin-secret'] = hge_ctx.hge_key
            resp = hge_ctx.http.post(hge_ctx.hge_url + '/v1/graphql', json=q,
                                     headers=headers)
            assert resp.status_code == 200 and 'data' in resp.json()

            # make a query where JSON body parsing fails
            q = {'quer': 'query { hello {code name} }'}
            headers = {'x-request-id': 'json-parse-fail-log-test'}
            if hge_ctx.hge_key:
                headers['x-hasura-admin-secret'] = hge_ctx.hge_key
            resp = hge_ctx.http.post(hge_ctx.hge_url + '/v1/graphql', json=q,
                                     headers=headers)
            assert resp.status_code == 200 and 'errors' in resp.json()

            # make an unthorized query where admin secret/access token is empty
            q = {'query': 'query { hello {code name} }'}
            headers = {'x-request-id': 'unauthorized-query-test'}
            resp = hge_ctx.http.post(hge_ctx.hge_url + '/v1/graphql', json=q,
                                     headers=headers)
            assert resp.status_code == 200 and 'errors' in resp.json()

            # make an unthorized metadata request where admin secret/access token is empty
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
            headers = {'x-request-id': 'unauthorized-metadata-test'}
            resp = hge_ctx.http.post(hge_ctx.hge_url + '/v1/query', json=q,
                                     headers=headers)
            assert resp.status_code == 401 and 'error' in resp.json()

            # gather and parse the logs now
            self.logs = parse_logs()
            # sometimes the log might take time to buffer
            time.sleep(2)
            yield
        finally:
            self._teardown(hge_ctx)

    def test_startup_logs(self, hge_ctx):
        def _get_server_config(x):
            return x['type'] == 'startup' and \
                'kind' in x['detail'] and \
                x['detail']['kind'] == 'server_configuration'

        config_logs = list(filter(_get_server_config, self.logs))
        print(config_logs)
        assert len(config_logs) == 1
        config_log = config_logs[0]
        print(config_log)
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

    def test_http_log(self, hge_ctx):
        def _get_http_logs(x):
            return x['type'] == 'http-log'

        print('all logs gathered', self.logs)
        http_logs = list(filter(_get_http_logs, self.logs))
        print('http logs', http_logs)
        assert len(http_logs) > 0
        for http_log in http_logs:
            print(http_log)

            http_info = http_log['detail']['http_info']
            assert 'url' in http_info
            assert 'status' in http_info
            assert 'ip' in http_info

            operation = http_log['detail']['operation']
            assert 'request_id' in operation
            if operation['request_id'] == 'successful-query-log-test':
                assert 'query_execution_time' in operation
                assert 'user_vars' in operation
                # we should see the `query` field in successful operations
                assert 'query' in operation
                assert operation['query'] == self.success_query
                # there shouldn't be any raw_query in success
                assert operation.get('raw_query') is None

    def test_query_log(self, hge_ctx):
        def _get_query_logs(x):
            return x['type'] == 'query-log'

        query_logs = list(filter(_get_query_logs, self.logs))
        assert len(query_logs) > 0
        onelog = query_logs[0]['detail']
        assert 'request_id' in onelog
        assert 'query' in onelog
        assert 'query' in onelog['query']
        assert 'generated_sql' in onelog

    def test_http_parse_failed_log(self, hge_ctx):
        def _get_parse_failed_logs(x):
            return x['type'] == 'http-log' and \
                x['detail']['operation']['request_id'] == 'json-parse-fail-log-test'

        http_logs = list(filter(_get_parse_failed_logs, self.logs))
        print('parse failed logs', http_logs)
        assert len(http_logs) > 0
        print(http_logs[0])
        assert 'error' in http_logs[0]['detail']['operation']
        assert http_logs[0]['detail']['operation']['error']['code'] == 'parse-failed'

    def test_http_unthorized_query(self, hge_ctx):
        def _get_failed_logs(x):
            return x['type'] == 'http-log' and \
                x['detail']['operation']['request_id'] == 'unauthorized-query-test'

        http_logs = list(filter(_get_failed_logs, self.logs))
        print('unauthorized failed logs', http_logs)
        assert len(http_logs) > 0
        print(http_logs[0])
        assert 'error' in http_logs[0]['detail']['operation']
        assert http_logs[0]['detail']['operation']['error']['code'] == 'access-denied'
        assert http_logs[0]['detail']['operation'].get('query') is None
        assert http_logs[0]['detail']['operation']['raw_query'] is not None

    def test_http_unthorized_metadata(self, hge_ctx):
        def _get_failed_logs(x):
            return x['type'] == 'http-log' and \
                x['detail']['operation']['request_id'] == 'unauthorized-metadata-test'

        http_logs = list(filter(_get_failed_logs, self.logs))
        print('unauthorized failed logs', http_logs)
        assert len(http_logs) > 0
        print(http_logs[0])
        assert 'error' in http_logs[0]['detail']['operation']
        assert http_logs[0]['detail']['operation']['error']['code'] == 'access-denied'
        assert http_logs[0]['detail']['operation'].get('query') is None
        # By default, 'raw_query' field is ignored for metadata queries. To allow
        # logging this field use the flag HASURA_GRAPHQL_ENABLE_METADATA_QUERY_LOGGING
        assert http_logs[0]['detail']['operation'].get('raw_query') is None
    
class TestWebsocketLogging():
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

    def _teardown(self, hge_ctx):
        hge_ctx.v1q_f(self.dir + '/teardown.yaml')

    @pytest.fixture(autouse=True)
    def transact(self, hge_ctx):
        # setup some tables
        hge_ctx.v1q_f(self.dir + '/setup.yaml')

        try:
            # make a successful websocket query
            headers = {'x-request-id': self.query_id}
            if hge_ctx.hge_key:
                headers['x-hasura-admin-secret'] = hge_ctx.hge_key

            resp = hge_ctx.ws_client.send_query(self.query, headers=headers,
                                                query_id=self.query_id,
                                                timeout=5)
            try:
                ev = next(resp)
                assert ev['type'] == 'data' and ev['id'] == self.query_id, ev
            finally:
                hge_ctx.ws_client.stop(self.query_id)

            # sometimes the log might take time to buffer
            time.sleep(2)
            # gather and parse the logs now
            self.logs = parse_logs()
            yield
        finally:
            self._teardown(hge_ctx)

    def test_websocket_log(self, hge_ctx):
        """
        tests for the `websocket-log` type. currently tests presence of operation_name
        """
        def _get_websocket_operation_logs(x):
            return x['type'] == 'websocket-log' and x['detail']['event']['type'] == 'operation'

        ws_logs = list(filter(_get_websocket_operation_logs, self.logs))
        assert len(ws_logs) > 0
        onelog = ws_logs[0]['detail']['event']['detail']
        assert 'request_id' in onelog
        assert 'operation_name' in onelog
        assert 'query' in onelog
        assert 'query' in onelog['query']

    def test_ws_server_log(self, hge_ctx):
        """
        tests for the `websocket-log` type. currently tests presence of operation_name
        """
        def _get_ws_server_logs(x):
            return x['type'] == 'ws-server' and 'metadata' in x['detail']

        ws_logs = list(filter(_get_ws_server_logs, self.logs))
        assert len(ws_logs) > 0
        onelog = ws_logs[0]['detail']
        assert 'operation_id' in onelog['metadata']
        assert 'operation_name' in onelog['metadata']

class TestConfiguragbleLogs():
    dir = 'queries/logging'
    success_query = {'query': 'query { hello {code name} }'}

    def _teardown(self, hge_ctx):
        hge_ctx.v1q_f(self.dir + '/teardown.yaml')

    @pytest.fixture(autouse=True)
    def transact(self, hge_ctx):
        # setup some tables
        hge_ctx.v1q_f(self.dir + '/setup.yaml')

        try:
            # make a successful query
            q = self.success_query
            headers = {'x-request-id': 'successful-query-log-test'}
            if hge_ctx.hge_key:
                headers['x-hasura-admin-secret'] = hge_ctx.hge_key
            resp = hge_ctx.http.post(hge_ctx.hge_url + '/v1/graphql', json=q,
                                     headers=headers)
            assert resp.status_code == 200 and 'data' in resp.json()

            # gather and parse the logs now
            self.logs = parse_logs()
            # sometimes the log might take time to buffer
            time.sleep(2)
            yield
        finally:
            self._teardown(hge_ctx)

    @pytest.mark.jwk_path('/jwk-cache-control?no-cache=true')
    def test_jwk_refresh_log(self, hge_ctx):
        def _get_jwk_refresh_log(x):
            return x['type'] == 'jwk-refresh-log'
        jwk_refresh_logs = list(filter(_get_jwk_refresh_log, self.logs))
        env_var = os.getenv('HASURA_GRAPHQL_ENABLED_LOG_TYPES')
        if env_var:
            if "jwk-refresh-log" in env_var:
                assert len(jwk_refresh_logs) > 0
            else:
                assert len(jwk_refresh_logs) == 0
