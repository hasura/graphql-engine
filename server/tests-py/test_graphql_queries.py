import yaml
from validate import check_query_f
from super_classes import DefaultTestSelectQueries


class TestGraphQLQueryBasic(DefaultTestSelectQueries):

    def test_select_query_author(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_query_author.yaml')

    def test_select_various_postgres_types(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_query_test_types.yaml')

    def test_select_query_author_quoted_col(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_query_author_col_quoted.yaml')

    def test_select_query_author_pk(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_query_author_by_pkey.yaml')

    def test_select_query_where(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_query_author_where.yaml')

    def test_nested_select_query_article_author(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/nested_select_query_article_author.yaml')

    def test_nested_select_query_deep(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/nested_select_query_deep.yaml')

    def test_nested_select_query_where(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/nested_select_where_query_author_article.yaml')

    def test_nested_select_query_where_on_relationship(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/nested_select_query_article_author_where_on_relationship.yaml')

    def test_select_query_user(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/select_query_user.yaml")

    def test_select_query_non_tracked_table(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/select_query_non_tracked_table_err.yaml")

    def test_select_query_col_not_present_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/select_query_author_col_not_present_err.yaml")

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/basic'


class TestGraphQLQueryAgg(DefaultTestSelectQueries):

    def test_article_agg_count_sum_avg_max_min_with_aliases(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/article_agg_count_sum_avg_max_min_with_aliases.yaml')

    def test_article_agg_where(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/article_agg_where.yaml')

    def test_author_agg_with_articles(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/author_agg_with_articles.yaml')

    def test_author_agg_with_articles_where(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/author_agg_with_articles_where.yaml')

    def test_article_deeply_nested_aggregate(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/article_deeply_nested_aggregate.yaml')

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/aggregations'


class TestGraphQLQueryAggPerm(DefaultTestSelectQueries):

    def test_author_agg_articles(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/author_agg_articles.yaml')

    def test_article_agg_fail(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/article_agg_fail.yaml')

    def test_author_articles_agg_fail(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/author_articles_agg_fail.yaml')

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/agg_perm'


class TestGraphQLQueryLimits(DefaultTestSelectQueries):

    def test_limit_1(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_query_article_limit_1.yaml')

    def test_limit_2(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_query_article_limit_2.yaml')

    def test_limit_null(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_query_article_limit_null.yaml')

    def test_err_str_limit_error(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_query_article_string_limit_error.yaml')

    def test_err_neg_limit_error(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_query_article_neg_limit_error.yaml')

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/limits'


class TestGraphQLQueryOffsets(DefaultTestSelectQueries):

    def test_offset_1_limit_2(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_query_article_offset_1_limit_2.yaml')

    def test_offset_2_limit_1(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_query_article_offset_2_limit_1.yaml')

    def test_int_as_string_offset(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_query_article_string_offset.yaml')

    def test_err_neg_offset_error(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_query_article_neg_offset_error.yaml')

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/offset'


class TestGraphQLQueryBoolExpBasic(DefaultTestSelectQueries):

    def test_author_article_where_not_equal(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_author_article_where_neq.yaml')

    def test_author_article_operator_ne_not_found_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_author_article_operator_ne_not_found_err.yaml')

    def test_author_article_where_greater_than(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_author_article_where_gt.yaml')

    def test_author_article_where_greater_than_or_equal(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_author_article_where_gte.yaml')

    def test_author_article_where_less_than(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_author_article_where_lt.yaml')

    def test_author_article_where_less_than_or_equal(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_author_article_where_lte.yaml')

    def test_author_article_where_in(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_author_article_where_in.yaml')

    def test_author_article_where_in_empty_array(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_author_article_where_in_empty_array.yaml')

    def test_author_article_where_nin_empty_array(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_author_article_where_nin_empty_array.yaml')

    def test_author_article_where_nin(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_author_article_where_nin.yaml')

    def test_uuid_test_in_uuid_col(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_uuid_test_in_uuid_col.yaml')

    def test_order_delivered_at_is_null(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_query_order_delivered_at_is_null.yaml')

    def test_order_delivered_at_is_not_null(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_query_order_delivered_at_is_not_null.yaml')

    def test_author_article_where_not_less_than(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_author_article_where_not_lt.yaml')

    def test_article_author_is_published_and_registered(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_article_author_is_published_and_registered.yaml')

    def test_article_author_not_published_nor_registered(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_article_author_not_published_or_not_registered.yaml')

    def test_article_author_unexpected_operator_in_where_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_author_article_unexpected_operator_in_where_err.yaml')

    def test_self_referential_relationships(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/self_referential_relationships.yaml')

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/boolexp/basic'


class TestGraphqlQueryPermissions(DefaultTestSelectQueries):

    def test_user_select_unpublished_articles(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/user_select_query_unpublished_articles.yaml')

    def test_user_only_other_users_published_articles(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/user_can_query_other_users_published_articles.yaml')

    def test_anonymous_only_published_articles(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/anonymous_can_only_get_published_articles.yaml')

    def test_user_cannot_access_remarks_col(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/user_cannot_access_remarks_col.yaml')

    def test_artist_select_query_Track_fail(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/artist_select_query_Track_fail.yaml')

    def test_artist_select_query_Track(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/artist_select_query_Track.yaml')

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/permissions'


class TestGraphQLQueryBoolExpSearch(DefaultTestSelectQueries):

    def test_city_where_like(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_city_where_like.yaml')

    def test_city_where_not_like(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_city_where_nlike.yaml')

    def test_city_where_ilike(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_city_where_ilike.yaml')

    def test_city_where_not_ilike(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_city_where_nilike.yaml')

    def test_city_where_similar(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_city_where_similar.yaml')

    def test_city_where_not_similar(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_city_where_not_similar.yaml')

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/boolexp/search'


class TestGraphQLQueryBoolExpJsonB(DefaultTestSelectQueries):

    def test_jsonb_contains_article_latest(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_article_author_jsonb_contains_latest.yaml')

    def test_jsonb_contains_article_beststeller(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_author_article_jsonb_contains_bestseller.yaml')

    def test_jsonb_contained_in_latest(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_article_author_jsonb_contained_in_latest.yaml')

    def test_jsonb_contained_in_bestseller_latest(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_article_author_jsonb_contained_in_bestseller_latest.yaml')

    def test_jsonb_has_key_sim_type(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_product_jsonb_has_key_sim_type.yaml')

    def test_jsonb_has_keys_any_os_operating_system(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_product_jsonb_has_keys_any_os_operating_system.yaml')

    def test_jsonb_has_keys_all_touchscreen_ram(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_product_jsonb_has_keys_all_ram_touchscreen.yaml')

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/boolexp/jsonb'


class TestGraphQLQueryOrderBy(DefaultTestSelectQueries):
    def test_articles_order_by_without_id(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/articles_order_by_without_id.yaml')

    def test_articles_order_by_rel_author_id(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/articles_order_by_rel_author_id.yaml')

    def test_articles_order_by_rel_author_rel_contact_phone(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/articles_order_by_rel_author_rel_contact_phone.yaml')

    def test_album_order_by_tracks_count(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/album_order_by_tracks_count.yaml')

    def test_album_order_by_tracks_duration_avg(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/album_order_by_tracks_duration_avg.yaml')

    def test_album_order_by_tracks_max_name(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/album_order_by_tracks_max_name.yaml')

    def test_album_order_by_tracks_bytes_stddev(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/album_order_by_tracks_bytes_stddev.yaml')

    def test_employee_distinct_department_order_by_salary_desc(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/employee_distinct_department_order_by_salary_desc.yaml')

    def test_employee_distinct_department_order_by_salary_asc(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/employee_distinct_department_order_by_salary_asc.yaml')

    def test_employee_distinct_fail(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/employee_distinct_fail.yaml')

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/order_by'
