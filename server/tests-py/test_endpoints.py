import pytest
import os
from validate import check_query_f, check_query, get_conf_f
from context import PytestConf

@pytest.mark.parametrize("transport", ['http'])
@pytest.mark.usefixtures('per_class_tests_db_state')
class TestCustomEndpoints:

    @classmethod
    def dir(cls):
        return 'queries/endpoints'

    def test_missing_endpoint(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/endpoint_missing.yaml', transport)

    def test_simple_endpoint(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/endpoint_simple.yaml', transport)

    def test_simple_endpoint_wrong_method(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/endpoint_simple_wrong_method.yaml', transport)

    @pytest.mark.skipif(os.getenv('MULTITENANT_ENVIRONMENT', default=None) is None, reason="Doesn't work with oss server.")
    def test_simple_cached_endpoint(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/endpoint_simple_cached.yaml', transport)

    def test_endpoint_with_query_arg(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/endpoint_with_query_arg.yaml', transport)

    def test_endpoint_with_body_arg(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/endpoint_with_body_arg.yaml', transport)

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

