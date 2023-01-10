#!/usrbin/env python3

import pytest
from validate import check_query, check_query_f

pytestmark = [
    pytest.mark.usefixtures('auth_hook'),
    pytest.mark.admin_secret,
    pytest.mark.hge_env('HASURA_GRAPHQL_AUTH_HOOK_MODE', 'POST'),
]

def check_post_404(hge_ctx,url):
   return check_query(hge_ctx, {
     'url': url,
     'status': 404,
     'query': {}
   })[0]

@pytest.mark.hge_env('HASURA_GRAPHQL_ENABLED_APIS', 'graphql')
@pytest.mark.default_source_disabled()
class TestMetadataDisabled:

    def test_metadata_v1_query_disabled(self, hge_ctx):
        check_post_404(hge_ctx,'/v1/query')

    def test_metadata_v1_template_disabled(self, hge_ctx):
        check_post_404(hge_ctx,'/v1/template/foo')

    def test_metadata_api_1_disabled(self, hge_ctx):
        check_post_404(hge_ctx,'/api/1/table/foo/select')

    def test_graphql_explain_disabled(self, hge_ctx):
        check_post_404(hge_ctx, '/v1/graphql/explain')

@pytest.mark.hge_env('HASURA_GRAPHQL_ENABLED_APIS', 'metadata')
class TestGraphQLDisabled:

    def test_graphql_endpoint_disabled(self, hge_ctx):
        check_post_404(hge_ctx, '/v1/graphql')

@pytest.mark.hge_env('HASURA_GRAPHQL_ENABLED_APIS', 'graphql')
@pytest.mark.default_source_disabled()
class TestGraphQLEnabled:

    def test_graphql_introspection(self, hge_ctx):
        check_query_f(hge_ctx, "queries/graphql_introspection/introspection_only_kind_of_queryType.yaml")

@pytest.mark.hge_env('HASURA_GRAPHQL_ENABLED_APIS', 'metadata')
class TestMetadataEnabled:

    def test_reload_metadata(self, hge_ctx):
        check_query_f(hge_ctx, "queries/v1/metadata/reload_metadata.yaml")

    def test_run_sql(self, hge_ctx):
        check_query_f(hge_ctx, "queries/v1/run_sql/sql_set_timezone.yaml")
