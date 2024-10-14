import pytest

from validate import check_query_f

@pytest.mark.admin_secret
@pytest.mark.parametrize('transport', ['http'])
@pytest.mark.usefixtures('pro_tests_fixtures', 'enterprise_edition', 'per_class_tests_db_state', 'actions_fixture', 'flush_redis')
class TestQueryCache:
    @classmethod
    def dir(cls):
        return 'queries/query_cache'

    def test_simple_cached_endpoint(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/simple_cached.yaml', transport)

    def test_metrics_one_endpoint(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/test_metrics_one.yaml', transport)

    def test_metrics_two_endpoint(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/test_metrics_two.yaml', transport)

    def test_cache_clear_endpoint(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/test_cache_clear.yaml', transport)

    def test_cache_clear_endpoint_key(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/test_cache_clear_key.yaml', transport)

    def test_cache_clear_endpoint_family(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/test_cache_clear_family.yaml', transport)

    def test_no_variables_in_query_key(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/test_no_variables_in_query_key.yaml', transport)

    def test_action_query(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/test_action_query.yaml', transport)
