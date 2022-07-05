import collections
import pytest
import os
from validate import check_query_f, check_query, get_conf_f
from context import PytestConf

@pytest.mark.parametrize("transport", ['http'])
@pytest.mark.usefixtures('per_method_tests_db_state')
class TestOpenAPISpec:

    @classmethod
    def dir(cls):
        return 'queries/openapi'

    def test_empty_openapi_json(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/openapi_empty.yaml', transport)

    def test_endpoint_simple(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/openapi_get_endpoint_test_simple.yaml', transport)

    def test_endpoint_with_aliases(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/openapi_endpoint_with_aliases.yaml', transport)

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

    def test_multiple_endpoints_with_path_segments(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/openapi_multiple_endpoints_with_path_segments.yaml', transport)

    def test_endpoint_with_complex_arg(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/openapi_get_endpoint_test_complex_arg.yaml', transport)

    def test_endpoint_with_complex_args(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/openapi_get_endpoint_test_complex_args.yaml', transport)

    def test_endpoint_with_recursive_arg(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/openapi_post_endpoint_test_recursive_arg.yaml', transport)

    def test_duplicate_field_name(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/openapi_get_endpoint_test_duplicate_field_name.yaml', transport)

    def test_inconsistent_schema_openAPI(self, hge_ctx, transport):
        # export metadata and create a backup
        backup_metadata = hge_ctx.v1q(
            q = {
                "type": "export_metadata",
                "args": {}
            }
        )

        new_metadata = backup_metadata.copy()

        #create inconsistent metadata
        inconsistent_query = [collections.OrderedDict([
            ('name', 'wrong_queries'),
            ('definition', collections.OrderedDict([
                ('queries', [collections.OrderedDict([
                    ('name', 'random_query'),
                    ('query', 'query { random_field_name test_table { random_col_name } }')
                ])])
            ]))
        ])]
        res_endpoint = [collections.OrderedDict([
            ("definition", collections.OrderedDict([
                ("query", collections.OrderedDict([
                    ("collection_name", "wrong_queries"),
                    ("query_name", "random_query")
                ]))
            ])),
            ("url", "some_url"),
            ("methods", ["GET"]),
            ("name", "wrong_endpoint"),
            ("comment", None)
        ])]
        new_metadata["query_collections"] = inconsistent_query
        new_metadata["rest_endpoints"] = res_endpoint

        # apply inconsistent metadata
        hge_ctx.v1q(
            q={
                "type": "replace_metadata",
                "version": 2,
                "args": {
                    "allow_inconsistent_metadata": True,
                    "metadata": new_metadata
                }
            }
        )

        # check openAPI schema
        check_query_f(hge_ctx, self.dir() + '/openapi_inconsistent_schema.yaml', transport)

        # revert to old metadata
        hge_ctx.v1q(
            q={
                "type": "replace_metadata",
                "args": backup_metadata
            }
        )
