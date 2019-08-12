#!/usr/bin/env python3

import json
import os
import time

import pytest

if not pytest.config.getoption("--test-logging"):
    pytest.skip("--test-logging missing, skipping tests", allow_module_level=True)

class TestLogging():
    dir = 'queries/logging'

    def _teardown(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp

    @pytest.fixture(autouse=True)
    def transact(self, hge_ctx):
        # setup some tables
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp

        # make a successful query
        q = {'query': 'query { hello {code name} }'}
        headers = {}
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

        # gather and parse the logs now
        self.logs = self._parse_logs(hge_ctx)
        # sometimes the log might take time to buffer
        time.sleep(2)
        yield
        self._teardown(hge_ctx)

    def _parse_logs(self, hge_ctx):
        # parse the log file into a json list
        log_file = os.getenv('LOGGING_TEST_LOGFILE_PATH', None)
        if not log_file:
            print('Could not determine log file path to test logging!')
            # force teardown if setup fails
            self._teardown(hge_ctx)
            assert False
        loglines = []
        with open(log_file, 'r') as f:
            loglines = f.readlines()
        logs = list(map(lambda x: json.loads(x.strip()), loglines))
        assert len(logs) > 0
        return logs

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
        http_info = http_logs[0]['detail']['http_info']
        assert 'url' in http_info
        assert 'status' in http_info
        assert 'ip' in http_info
        print(http_logs)
        operation = http_logs[0]['detail']['operation']
        assert 'query_execution_time' in operation
        assert 'user_vars' in operation
        assert 'request_id' in operation

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
