#!/usr/bin/env python3

import json
import os
import time

import pytest
from context import PytestConf

if not PytestConf.config.getoption("--test-startup-db-calls"):
    pytest.skip("--test-startup-db-calls missing, skipping tests", allow_module_level=True)

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

class TestStartupDBLogging():

    @pytest.fixture(autouse=True)
    def transact(self):
        try:
            # gather and parse the logs
            self.logs = parse_logs()
            # sometimes the log might take time to buffer
            time.sleep(2)
            yield
        except:
            assert False 

    def test_startup_db_calls_logs(self):
        def _get_source_resolve(x):
            return x['type'] == 'startup' and \
                'kind' in x['detail'] and \
                x['detail']['kind'] == 'resolve_source'

        source_resolve_logs = list(filter(_get_source_resolve, self.logs))
        print(source_resolve_logs)
        assert len(source_resolve_logs) == 1
