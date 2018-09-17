import pytest
import yaml
from validate import check_query_f

class TestGraphQLQueryBasic(object):

    def test_select_query_author(self, hge_ctx):
        check_query_f(hge_ctx, 'queries/graphql_query/basic/select_query_author.yaml')

    def test_select_query_author_pk(self, hge_ctx):
        check_query_f(hge_ctx, "queries/graphql_query/basic/select_query_author_by_pkey.yaml")

    def test_select_query_where(self, hge_ctx):
        check_query_f(hge_ctx, "queries/graphql_query/basic/select_query_author_where.yaml")

    def test_nested_select_query_article_author(self, hge_ctx):
        check_query_f(hge_ctx, "queries/graphql_query/basic/nested_select_query_article_author.yaml")

    def test_nested_select_query_where(self, hge_ctx):
        check_query_f(hge_ctx, "queries/graphql_query/basic/nested_select_where_query_author_article.yaml")

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):  
        st_code, resp = hge_ctx.v1q_f('queries/graphql_query/basic/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f('queries/graphql_query/basic/teardown.yaml')
        assert st_code == 200, resp

class TestGraphQLQueryLimits(object):
    
    def test_limit_offset(self, hge_ctx):
        check_query_f(hge_ctx, "queries/graphql_query/limits/select_query_article_limit_offset.yaml")
    
    def test_err_str_offset(self, hge_ctx):
        check_query_f(hge_ctx, "queries/graphql_query/limits/select_query_article_limit_string_offset_error.yaml")
    
    def test_err_neg_offset(self, hge_ctx):
        check_query_f(hge_ctx, "queries/graphql_query/limits/select_query_article_limit_neg_offset.yaml")

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):  
        st_code, resp = hge_ctx.v1q_f('queries/graphql_query/limits/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f('queries/graphql_query/limits/teardown.yaml')
        assert st_code == 200, resp

