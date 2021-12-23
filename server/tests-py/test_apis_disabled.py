#!/usrbin/env python3

import pytest
from validate import check_query, check_query_f
from context import PytestConf

def check_post_404(hge_ctx,url):
   return check_query(hge_ctx, {
     'url': url,
     'status': 404,
     'query': {}
   })[0]

metadata_api_disabled = PytestConf.config.getoption("--test-metadata-disabled")
graphql_api_disabled = PytestConf.config.getoption("--test-graphql-disabled")

@pytest.mark.skipif(not metadata_api_disabled,
                    reason="flag --test-metadata-disabled is not set. Cannot run tests for metadata disabled")
class TestMetadataDisabled:

    def test_metadata_v1_query_disabled(self, hge_ctx):
        check_post_404(hge_ctx,'/v1/query')

    def test_metadata_v1_template_disabled(self, hge_ctx):
        check_post_404(hge_ctx,'/v1/template/foo')

    def test_metadata_api_1_disabled(self, hge_ctx):
        check_post_404(hge_ctx,'/api/1/table/foo/select')

    def test_graphql_explain_disabled(self, hge_ctx):
        check_post_404(hge_ctx, '/v1/graphql/explain')

@pytest.mark.skipif(not graphql_api_disabled,
                    reason="--test-graphql-disabled is not set. Cannot run GraphQL disabled tests")
class TestGraphQLDisabled:

    def test_graphql_endpoint_disabled(self, hge_ctx):
        check_post_404(hge_ctx, '/v1/graphql')

@pytest.mark.skipif(graphql_api_disabled,
                    reason="--test-graphql-disabled is set. Cannot run GraphQL enabled tests")
class TestGraphQLEnabled:

    def test_graphql_introspection(self, hge_ctx):
        check_query_f(hge_ctx, "queries/graphql_introspection/introspection_only_kind_of_queryType.yaml")


@pytest.mark.skipif(metadata_api_disabled,
                    reason="--test-metadata-disabled is set. Cannot run metadata enabled tests")
class TestMetadataEnabled:

    def test_reload_metadata(self, hge_ctx):
        check_query_f(hge_ctx, "queries/v1/metadata/reload_metadata.yaml")

    def test_run_sql(self, hge_ctx):
        check_query_f(hge_ctx, "queries/v1/run_sql/sql_set_timezone.yaml")


