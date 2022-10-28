import pytest
import os
from validate import check_query_f, check_query, get_conf_f
from context import PytestConf

import redis

@pytest.mark.parametrize("transport", ['http'])
@pytest.mark.usefixtures('postgis', 'per_class_tests_db_state')
class TestCustomEndpoints:
    def flushRedis(self):
        # TODO: Move this into setup/teardown
        r = redis.from_url(PytestConf.config.getoption('--redis-url'))
        r.flushall()

    @classmethod
    def dir(cls):
        return 'queries/endpoints'

    def test_missing_endpoint(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/endpoint_missing.yaml', transport)

    def test_endpoint_as_user_err(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/endpoint_as_user_err.yaml', transport)

    def test_simple_endpoint(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/endpoint_simple.yaml', transport)

    def test_simple_endpoint_wrong_method(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/endpoint_simple_wrong_method.yaml', transport)

    @pytest.mark.skipif(not PytestConf.config.getoption('--redis-url'), reason="Must enable redis")
    def test_simple_cached_endpoint(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/endpoint_simple_cached.yaml', transport)
        self.flushRedis()

    def test_endpoint_with_query_arg(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/endpoint_with_query_arg.yaml', transport)

    def test_endpoint_with_body_arg(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/endpoint_with_body_arg.yaml', transport)

    def test_endpoint_with_body_list_arg(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/endpoint_with_list_arg.yaml', transport)

    def test_endpoint_with_query_arg_url_encoded(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/endpoint_with_query_arg_url_encoded.yaml', transport)

    def test_endpoint_with_query_arg_url_encoded_2(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/endpoint_with_query_arg_url_encoded_2.yaml', transport)

    def test_endpoint_with_query_args_missing_arg(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/endpoint_with_query_arg_missing_arg.yaml', transport)

    def test_endpoint_with_multiple_query_args(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/endpoint_with_multiple_query_args.yaml', transport)

    def test_endpoint_with_template(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/endpoint_with_template.yaml', transport)

    def test_endpoint_dropped(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/endpoint_dropped.yaml', transport)

    def test_endpoint_conflicting(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/endpoint_conflicting.yaml', transport)

    def test_endpoint_duplicate_param(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/endpoint_duplicate_param.yaml', transport)

    def test_endpoint_empty_path(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/endpoint_empty_path.yaml', transport)

    def test_endpoint_trailing_slash(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/endpoint_trailing_slash.yaml', transport)

    def test_endpoint_empty_path_segment(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/endpoint_empty_path_segment.yaml', transport)

    def test_endpoint_empty_path_param(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/endpoint_empty_path_param.yaml', transport)

    def test_endpoint_uuid_arg(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/endpoint_with_uuid_arg.yaml', transport)

    def test_endpoint_subscription(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/endpoint_subscription.yaml', transport)

    def test_query_validation(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/endpoint_query_validation.yaml', transport)

    def test_endpoint_with_pg_date_url_variable(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/endpoint_pg_date_url_variable.yaml', transport)
