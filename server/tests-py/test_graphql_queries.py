import pytest
import yaml
from validate import check_query_f

class TestGraphQLQueryBasic:

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

class TestGraphQLQueryLimits:

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

class TestGraphQLQueryOffsets:

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

class TestGraphQLQueryBoolExpJsonB:

    def test_jsonb_contains_article_latest(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_article_author_jsonb_contains_latest.yaml')

    def test_jsonb_contains_article_beststeller(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_author_article_jsonb_contains_bestseller.yaml')

    def test_jsonb_contained_in_latest(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_article_author_jsonb_contained_in_latest.yaml')

    def test_jsonb_contained_in_bestseller_latest(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_article_author_jsonb_contained_in_bestseller_latest.yaml')

    def test_jsonb_has_key_sim_type(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_product_jsonb_has_key_sim_type.yaml')

    def test_jsonb_has_keys_any_os_operating_system(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_product_jsonb_has_keys_any_os_operating_system.yaml')

    def test_jsonb_has_keys_all_touchscreen_ram(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_product_jsonb_has_keys_all_ram_touchscreen.yaml')

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        self.dir = 'queries/graphql_query/boolexp/jsonb'
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp
