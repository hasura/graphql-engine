import pytest
import yaml
from validate import check_query_f

class TestGraphQLQueryBasic(object):

    def test_select_query_author(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_query_author.yaml')

    def test_select_query_author_pk(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_query_author_by_pkey.yaml')

    def test_select_query_where(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_query_author_where.yaml')

    def test_nested_select_query_article_author(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/nested_select_query_article_author.yaml')

    def test_nested_select_query_where(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/nested_select_where_query_author_article.yaml')

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):  
        self.dir = 'queries/graphql_query/basic'
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp

class TestGraphQLQueryLimits(object):

    def test_limit_1(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_query_article_limit_1.yaml')

    def test_limit_2(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_query_article_limit_2.yaml')

    def test_err_str_limit_error(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_query_article_string_limit_error.yaml')

    def test_err_neg_limit_error(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_query_article_neg_limit_error.yaml')

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):  
        self.dir = 'queries/graphql_query/limits'
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp

class TestGraphQLQueryOffsets(object):

    def test_limit_1(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_query_article_offset_1_limit_2.yaml')

    def test_limit_2(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_query_article_offset_2_limit_1.yaml')

    def test_err_str_offset(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_query_article_string_offset.yaml')

    def test_err_neg_offset_error(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_query_article_neg_offset_error.yaml')

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        self.dir = 'queries/graphql_query/offset'
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp
