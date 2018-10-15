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

    def test_nested_select_query_deep(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/nested_select_query_deep.yaml')

    def test_nested_select_query_where(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/nested_select_where_query_author_article.yaml')

    def test_select_query_user(self, hge_ctx):
        check_query_f(hge_ctx, "queries/graphql_query/basic/select_query_user.yaml")

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

    def test_offset_1_limit_2(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_query_article_offset_1_limit_2.yaml')

    def test_offset_2_limit_1(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_query_article_offset_2_limit_1.yaml')

    def test_int_as_string_offset(self, hge_ctx):
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


class TestGraphQLQueryBoolExpBasic:

    def test_author_article_where_not_equal(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_author_article_where_neq.yaml')

    def test_author_article_where_greater_than(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_author_article_where_gt.yaml')

    def test_author_article_where_greater_than_or_equal(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_author_article_where_gte.yaml')

    def test_author_article_where_less_than(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_author_article_where_lt.yaml')

    def test_author_article_where_less_than_or_equal(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_author_article_where_lte.yaml')

    def test_author_article_where_in(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_author_article_where_in.yaml')

    def test_author_article_where_nin(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_author_article_where_nin.yaml')

    def test_order_delivered_at_is_null(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_query_order_delivered_at_is_null.yaml')

    def test_order_delivered_at_is_not_null(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_query_order_delivered_at_is_not_null.yaml')

    def test_author_article_where_not_less_than(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_author_article_where_not_lt.yaml')

    def test_article_author_is_published_and_registered(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_article_author_is_published_and_registered.yaml')

    def test_article_author_not_published_nor_registered(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_article_author_not_published_or_not_registered.yaml')

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        self.dir = 'queries/graphql_query/boolexp/basic'
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp


class TestGraphqlQueryPermissions:

    def test_user_select_unpublished_articles(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/user_select_query_unpublished_articles.yaml')

    def test_user_only_other_users_published_articles(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/user_can_query_other_users_published_articles.yaml')

    def test_anonymous_only_published_articles(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/anonymous_can_only_get_published_articles.yaml')

    def test_user_cannot_access_remarks_col(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/user_cannot_access_remarks_col.yaml')

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        self.dir = 'queries/graphql_query/permissions'
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp


class TestGraphQLQueryBoolExpSearch:

    def test_city_where_like(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_city_where_like.yaml')

    def test_city_where_not_like(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_city_where_nlike.yaml')

    def test_city_where_ilike(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_city_where_ilike.yaml')

    def test_city_where_not_ilike(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_city_where_nilike.yaml')

    def test_city_where_similar(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_city_where_similar.yaml')

    def test_city_where_not_similar(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_city_where_not_similar.yaml')

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        self.dir = 'queries/graphql_query/boolexp/search'
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
