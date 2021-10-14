import pytest
import os
from validate import check_query_f, check_query, get_conf_f
from context import PytestConf

@pytest.mark.parametrize("transport", ['http'])
@pytest.mark.usefixtures('per_class_tests_db_state')
class TestOpenAPISpec:

    @classmethod
    def dir(cls):
        return 'queries/openapi'

    def test_empty_openapi_json(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/openapi_empty.yaml', transport)

    def test_endpoint_simple(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/openapi_get_endpoint_test_simple.yaml', transport)

    def test_endpoint_with_args(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/openapi_post_endpoint_test_with_args.yaml', transport)

    def test_endpoint_with_args_url(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/openapi_post_endpoint_test_with_args_url.yaml', transport)

    def test_endpoint_with_default_arg(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/openapi_post_endpoint_test_with_default_arg.yaml', transport)

    def test_endpoint_with_multiple_methods(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/openapi_endpoint_with_multiple_methods.yaml', transport)

    def test_multiple_endpoints(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/openapi_multiple_endpoints_test.yaml', transport)

    def test_multiple_endpoints_same_path(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/openapi_multiple_endpoints_same_path.yaml', transport)

