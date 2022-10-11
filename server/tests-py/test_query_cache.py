import pytest
import os
import redis

from validate import check_query_f, check_query, get_conf_f
from context import PytestConf

# Mark that all tests in this module can be run as server upgrade tests
pytestmark = pytest.mark.allow_server_upgrade_test

usefixtures = pytest.mark.usefixtures

@pytest.mark.skipif(not PytestConf.config.getoption('--redis-url'), reason="Must enable redis")
@pytest.mark.parametrize("transport", ['http'])
@usefixtures('per_class_tests_db_state')
class TestQueryCache:
    def flushRedis(self):
        # TODO: Move this into setup/teardown
        r = redis.from_url(PytestConf.config.getoption('--redis-url'))
        r.flushall()

    @classmethod
    def dir(cls):
        return 'queries/query_cache'

    def test_simple_cached_endpoint(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/simple_cached.yaml', transport)
        self.flushRedis()

    def test_metrics_one_endpoint(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/test_metrics_one.yaml', transport)
        self.flushRedis()

    def test_metrics_two_endpoint(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/test_metrics_two.yaml', transport)
        self.flushRedis()

    def test_cache_clear_endpoint(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/test_cache_clear.yaml', transport)
        self.flushRedis()

    def test_cache_clear_endpoint_key(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/test_cache_clear_key.yaml', transport)
        self.flushRedis()

    def test_cache_clear_endpoint_family(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/test_cache_clear_family.yaml', transport)
        self.flushRedis()

    def test_no_variables_in_query_key(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/test_no_variables_in_query_key.yaml', transport)
        self.flushRedis()

    def test_action_query_with_forward_client_headers_set(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/test_action_query_with_forward_client_headers_set.yaml', transport)
        self.flushRedis()

    def test_action_query_with_forward_client_header_unset(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/test_action_query_with_forward_client_headers_unset.yaml', transport)
        self.flushRedis()
