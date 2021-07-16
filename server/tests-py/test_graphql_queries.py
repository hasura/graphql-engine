import pytest
from validate import check_query_f, check_query, get_conf_f
from conftest import use_inherited_roles_fixtures
from context import PytestConf

# Mark that all tests in this module can be run as server upgrade tests
pytestmark = pytest.mark.allow_server_upgrade_test

usefixtures = pytest.mark.usefixtures


@pytest.mark.parametrize("transport", ['http', 'websocket'])
@pytest.mark.parametrize("backend", ['mysql'])
@usefixtures('per_class_tests_db_state')
class TestGraphQLQueryBasicMySQL:

    # initialize the metadata
    def test_replace_metadata(self, hge_ctx, transport):
        if transport == 'http':
            check_query_f(hge_ctx, self.dir() + '/replace_metadata.yaml')

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/mysql'


@pytest.mark.parametrize("transport", ['http', 'websocket'])
@pytest.mark.parametrize("backend", ['bigquery'])
@usefixtures('per_class_tests_db_state')
class TestGraphQLQueryBasicBigquery:

    # initialize the metadata
    def test_replace_metadata(self, hge_ctx, transport):
        if transport == 'http':
            check_query_f(hge_ctx, self.dir() + '/replace_metadata.yaml')

    def test_global_limit(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/global_limit.yaml", transport)

    def test_offset_regression(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/offset_regression.yaml", transport)

    def test_user_perms(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/user_perms.yaml", transport)

    def test_empty_perms(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/empty_perms.yaml", transport)

    def test_timestamp_perm(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/timestamp_perm.yaml", transport)

    def test_exact_article_id(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/exact_article_id.yaml", transport)

    def test_perms_published_articles(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/perms_published_articles.yaml", transport)

    # types
    def test_select_query_all_types(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/select_query_all_types.yaml", transport)

    # relational queries
    def test_select_query_author(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_author.yaml', transport)

    def test_select_query_author_pk(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_author_by_pkey.yaml', transport)

    def test_select_query_author_quoted_col(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_author_col_quoted.yaml', transport)

    def test_select_query_author_with_skip_directive(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_author_skip_directive.yaml', transport)

    def test_select_query_author_with_include_directive(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_author_include_directive.yaml', transport)

    def test_select_query_where(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_author_where.yaml', transport)

    def test_nested_select_query_article_author(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/nested_select_query_article_author.yaml', transport)

    def test_nested_select_query_deep(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/nested_select_query_deep.yaml', transport)

    def test_nested_select_query_where(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/nested_select_where_query_author_article.yaml', transport)

    def test_nested_select_query_where_on_relationship(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/nested_select_query_article_author_where_on_relationship.yaml', transport)

    def test_select_query_non_tracked_table(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/select_query_non_tracked_table_err.yaml", transport)

    def test_select_query_col_not_present_err(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/select_query_author_col_not_present_err.yaml", transport)

    # batching # works only with http, not with websocket
    def test_select_query_batching(self, hge_ctx, transport):
        if transport == 'http':
            check_query_f(hge_ctx, self.dir() + "/select_query_batching.yaml", transport)

    def test_select_query_batching_with_one_error(self, hge_ctx, transport):
        if transport == 'http':
            check_query_f(hge_ctx, self.dir() + "/select_query_batching_with_one_error.yaml", transport)

    # fragments
    def test_select_query_top_level_fragment(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_top_level_fragment.yaml', transport)

    def test_select_query_nested_fragment(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_nested_fragment.yaml', transport)

    def test_select_query_fragment_cycles(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_fragment_cycles.yaml', transport)

    def test_select_query_fragment_with_variable(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_fragment_with_variable.yaml', transport)

    # invalid # this is not applicable to bigquery, it simply returns empty results
    def test_select_query_invalid_escape_sequence(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/select_query_invalid_escape_sequence.yaml", transport)

    # aggregates
    def test_select_join_provenance_queries(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/select_join_provenance.yaml", transport)

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/bigquery'

@pytest.mark.parametrize("transport", ['http', 'websocket'])
@pytest.mark.parametrize("backend", ['citus', 'mssql', 'postgres'])
@usefixtures('per_class_tests_db_state')
class TestGraphQLQueryBasicCommon:
    # This also exercises support for multiple operations in a document:
    def test_select_query_author(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_author.yaml', transport)

    def test_select_query_author(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_author_v1alpha1.yaml', transport)

    def test_select_query_author_quoted_col(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_author_col_quoted.yaml', transport)

    def test_select_query_author_with_skip_directive(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_author_skip_directive.yaml', transport)

    def test_select_query_author_with_include_directive(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_author_include_directive.yaml', transport)

    def test_select_query_author_with_skip_include_directive(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_author_skip_include_directives.yaml', transport)

    def test_select_query_author_with_wrong_directive_err(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_author_wrong_directive_err.yaml', transport)

    def test_select_query_where(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_author_where.yaml', transport)

    def test_nested_select_query_article_author(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/nested_select_query_article_author.yaml', transport)

    def test_nested_select_query_deep(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/nested_select_query_deep.yaml', transport)

    def test_nested_select_query_where(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/nested_select_where_query_author_article.yaml', transport)

    def test_nested_select_query_where_on_relationship(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/nested_select_query_article_author_where_on_relationship.yaml', transport)

    def test_select_query_non_tracked_table(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/select_query_non_tracked_table_err.yaml", transport)

    def test_select_query_col_not_present_err(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/select_query_author_col_not_present_err.yaml", transport)

    def test_select_query_multiple_columns_arr_fkey(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/select_multiple_columns_arr_fkey.yaml", transport)

    def test_select_query_multiple_columns_obj_fkey(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/select_multiple_columns_obj_fkey.yaml", transport)


    @classmethod
    def dir(cls):
        return 'queries/graphql_query/basic'

@pytest.mark.parametrize("transport", ['http', 'websocket'])
@pytest.mark.parametrize("backend", ['mssql'])
@usefixtures('per_class_tests_db_state')
class TestGraphQLQueryBasicMSSQL:

    def test_select_various_mssql_types(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_test_types_mssql.yaml', transport)

    def test_select_query_user(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/select_query_user_mssql.yaml", transport)

    def test_select_query_user_col_change(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/select_query_user_col_change_mssql.yaml")

    def test_nodes_aggregates_mssql(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/nodes_aggregates_mssql.yaml", transport)

    def test_nodes_aggregates_conditions_mssql(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/nodes_aggregates_conditions_mssql.yaml", transport)

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/basic'

@pytest.mark.parametrize("transport", ['http', 'websocket'])
@pytest.mark.parametrize("backend", ['postgres'])
@usefixtures('per_class_tests_db_state')
class TestGraphQLQueryBasicPostgres:
    # Can't run server upgrade tests, as this test has a schema change
    @pytest.mark.skip_server_upgrade_test
    def test_select_various_postgres_types(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_test_types_postgres.yaml', transport)

    # TODO: https://github.com/hasura/graphql-engine-mono/issues/866
    # This test currently fails on MSSQL.
    # Move to TestGraphQLQueryBasicCommon once linked issue is fixed.
    def test_select_query_author_pk(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_author_by_pkey.yaml', transport)

    def test_select_query_invalid_escape_sequence(self, hge_ctx, transport):
        transport = 'http'
        check_query_f(hge_ctx, self.dir() + "/select_query_invalid_escape_sequence.yaml", transport)

    def test_nested_select_with_foreign_key_alter(self, hge_ctx, transport):
        transport = 'http'
        check_query_f(hge_ctx, self.dir() + "/nested_select_with_foreign_key_alter.yaml", transport)

    def test_select_query_user(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/select_query_user_postgres.yaml", transport)

    def test_select_query_user_col_change(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/select_query_user_col_change_postgres.yaml")

    def test_select_query_person_citext(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/select_query_person_citext.yaml", transport)

    def test_select_query_batching(self, hge_ctx, transport):
        transport = 'http'
        check_query_f(hge_ctx, self.dir() + "/select_query_batching.yaml", transport)

    def test_select_query_batching_with_mutation(self, hge_ctx, transport):
        transport = 'http'
        check_query_f(hge_ctx, self.dir() + "/select_query_batching_with_mutation.yaml", transport)

    def test_select_query_batching_with_one_error(self, hge_ctx, transport):
        transport = 'http'
        check_query_f(hge_ctx, self.dir() + "/select_query_batching_with_one_error.yaml", transport)

    def test_create_invalid_fkey_relationship(self, hge_ctx, transport):
        st_code, resp = hge_ctx.v1q_f(self.dir() + '/setup_invalid_fkey_relationship.yaml')
        assert st_code == 400, resp
        assert resp['error'] == "Expecting object { table, columns }."

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/basic'

@pytest.mark.parametrize("transport", ['http', 'websocket'])
@pytest.mark.parametrize("backend", ['citus'])
@usefixtures('per_class_tests_db_state')
class TestGraphQLQueryBasicCitus:
    def test_nested_select_with_foreign_key_alter(self, hge_ctx, transport):
        transport = 'http'
        check_query_f(hge_ctx, self.dir() + "/nested_select_with_foreign_key_alter_citus.yaml", transport)

    @pytest.mark.skip(reason="TODO: https://github.com/hasura/graphql-engine-mono/issues/1224")
    def test_select_query_user_col_change(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/select_query_user_col_change_citus.yaml")

    @pytest.mark.skip(reason="TODO: https://github.com/hasura/graphql-engine-mono/issues/1224")
    def test_select_query_person_citext(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/select_query_person_citext.yaml", transport)

    # relationships test cases described at
    # https://github.com/hasura/graphql-engine-mono/blob/vamshi/rfc/citus-support/rfcs/citus-support.md
    def test_select_relationships_distributed(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/select_query_disaster_relationships_distributed.yaml", transport)

    def test_select_relationships_reference(self, hge_ctx, transport):
        transport = 'http'
        check_query_f(hge_ctx, self.dir() + "/select_query_disaster_relationships_reference.yaml", transport)

    def test_select_functions(self, hge_ctx, transport):
        transport = 'http'
        check_query_f(hge_ctx, self.dir() + "/select_query_disaster_functions.yaml", transport)

    def test_create_invalid_fkey_relationship(self, hge_ctx, transport):
        st_code, resp = hge_ctx.v1metadataq_f(self.dir() + '/setup_invalid_fkey_relationship.yaml')
        assert st_code == 400, resp
        assert resp['error'] == "Expecting object { table, columns }."

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/citus'

@pytest.mark.parametrize("transport", ['http', 'websocket'])
@pytest.mark.parametrize("backend", ['citus', 'postgres'])
@usefixtures('per_class_tests_db_state')
class TestGraphQLQueryFragments:

    def test_select_query_top_level_fragment(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_top_level_fragment.yaml', transport)

    def test_select_query_nested_fragment(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_nested_fragment.yaml', transport)

    def test_select_query_fragment_cycles(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_fragment_cycles.yaml', transport)

    def test_select_query_fragment_with_variable(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_fragment_with_variable.yaml', transport)

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/basic'

@pytest.mark.parametrize("transport", ['http', 'websocket'])
@usefixtures('per_class_tests_db_state')
class TestGraphQLQueryAgg:

    def test_article_agg_count_sum_avg_max_min_with_aliases(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/article_agg_count_sum_avg_max_min_with_aliases.yaml', transport)

    def test_article_agg_where(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/article_agg_where.yaml', transport)

    def test_author_agg_with_articles(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/author_agg_with_articles.yaml', transport)

    def test_author_agg_with_articles_where(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/author_agg_with_articles_where.yaml', transport)

    def test_article_deeply_nested_aggregate(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/article_deeply_nested_aggregate.yaml', transport)

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/aggregations'


@pytest.mark.parametrize("transport", ['http', 'websocket'])
@usefixtures('per_class_tests_db_state')
class TestGraphQLQueryAggPerm:

    def test_author_agg_articles(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/author_agg_articles.yaml', transport)

    def test_article_agg_fail(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/article_agg_fail.yaml', transport)

    def test_author_articles_agg_fail(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/author_articles_agg_fail.yaml', transport)

    def test_author_post_agg_order_by(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/author_post_agg_order_by.yaml', transport)

    def test_article_agg_without_select_access_to_any_col(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/article_agg_with_role_without_select_access.yaml', transport)

    def test_article_agg_with_select_access(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/article_agg_with_role_with_select_access.yaml', transport)

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/agg_perm'


@usefixtures('per_class_tests_db_state')
class TestGraphQLQueryLimits:

    @pytest.mark.parametrize("transport", ['http', 'websocket'])
    def test_limit_1(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_article_limit_1.yaml', transport)

    @pytest.mark.parametrize("transport", ['http', 'websocket'])
    def test_limit_2(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_article_limit_2.yaml', transport)

    def test_limit_null(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_query_article_limit_null.yaml')

    @pytest.mark.parametrize("transport", ['http', 'websocket'])
    def test_err_str_limit_error(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_article_string_limit_error.yaml', transport)

    @pytest.mark.parametrize("transport", ['http', 'websocket'])
    def test_err_neg_limit_error(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_article_neg_limit_error.yaml', transport)

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/limits'


@usefixtures('per_class_tests_db_state')
class TestGraphQLQueryOffsets:

    @pytest.mark.parametrize("transport", ['http', 'websocket'])
    def test_offset_1_limit_2(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_article_offset_1_limit_2.yaml', transport)

    @pytest.mark.parametrize("transport", ['http', 'websocket'])
    def test_offset_2_limit_1(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_article_offset_2_limit_1.yaml', transport)

    @pytest.mark.parametrize("transport", ['http', 'websocket'])
    def test_int_as_string_offset(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_article_string_offset.yaml', transport)

    def test_err_neg_offset_error(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_query_article_neg_offset_error.yaml')

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/offset'

@pytest.mark.parametrize("transport", ['http', 'websocket', 'subscription'])
@pytest.mark.parametrize("backend", ['mssql', 'postgres'])
@usefixtures('per_class_tests_db_state')
class TestGraphQLQueryBoolExpBasicCommon:
    def test_order_delivered_at_is_null(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_order_delivered_at_is_null.yaml', transport)

    def test_order_delivered_at_is_not_null(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_order_delivered_at_is_not_null.yaml', transport)

    def test_author_article_where_not_equal(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_author_article_where_neq.yaml', transport)

    def test_author_article_where_greater_than(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_author_article_where_gt.yaml', transport)

    def test_author_article_where_greater_than_or_equal(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_author_article_where_gte.yaml', transport)

    def test_author_article_where_less_than(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_author_article_where_lt.yaml', transport)

    def test_author_article_where_not_less_than(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_author_article_where_not_lt.yaml', transport)

    def test_author_article_where_less_than_or_equal(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_author_article_where_lte.yaml', transport)

    def test_article_author_is_published_and_registered(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_article_author_is_published_and_registered.yaml', transport)

    def test_article_author_not_published_nor_registered(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_article_author_not_published_or_not_registered.yaml', transport)

    def test_author_article_where_in(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_author_article_where_in.yaml', transport)

    def test_author_article_where_nin(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_author_article_where_nin.yaml', transport)

    def test_author_article_where_permissions(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_author_article_where_permissions.yaml', transport)

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/boolexp/basic'

@pytest.mark.parametrize("transport", ['http', 'websocket'])
@pytest.mark.parametrize("backend", ['postgres'])
@usefixtures('per_class_tests_db_state')
class TestGraphQLQueryBoolExpBasicPostgres:
    def test_author_article_operator_ne_not_found_err(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_author_article_operator_ne_not_found_err_postgres.yaml', transport)

    def test_author_article_where_in_empty_array(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_author_article_where_in_empty_array_postgres.yaml', transport)

    def test_author_article_where_nin_empty_array(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_author_article_where_nin_empty_array_postgres.yaml', transport)

    def test_article_author_unexpected_operator_in_where_err(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_author_article_unexpected_operator_in_where_err_postgres.yaml', transport)

    def test_uuid_test_in_uuid_col(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_uuid_test_in_uuid_col_postgres.yaml', transport)

    def test_self_referential_relationships(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/self_referential_relationships.yaml', transport)

    def test_query_account_permission_success(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_account_permission_success.yaml', transport)

    def test_query_account_permission_fail(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_account_permission_fail.yaml', transport)

    def test_in_sql_identifier_array(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/in_sql_identifier_array.yaml', transport)

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/boolexp/basic'

@pytest.mark.parametrize("transport", ['http', 'websocket'])
@pytest.mark.parametrize("backend", ['mssql'])
@usefixtures('per_class_tests_db_state')
class TestGraphQLQueryBoolExpBasicMSSQL:
    def test_author_article_operator_ne_not_found_err(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_author_article_operator_ne_not_found_err_mssql.yaml', transport)

    def test_article_author_unexpected_operator_in_where_err(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_author_article_unexpected_operator_in_where_err_mssql.yaml', transport)

    def test_uuid_test_in_uuid_col(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_uuid_test_in_uuid_col_mssql.yaml', transport)

    def test_bools(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_bools_mssql.yaml', transport)

    def test_create_invalid_fkey_relationship(self, hge_ctx, transport):
        st_code, resp = hge_ctx.v1metadataq_f(self.dir() + '/setup_invalid_fkey_relationship_mssql.yaml')
        assert st_code == 400, resp
        assert resp['error'] == "Expecting object { table, columns }."

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/boolexp/basic'

@pytest.mark.parametrize("transport", ['http', 'websocket'])
@usefixtures('per_class_tests_db_state')
class TestGraphqlQueryPermissions:

    def test_user_select_unpublished_articles(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/user_select_query_unpublished_articles.yaml', transport)

    def test_user_select_query_article_author(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/user_select_query_article_author.yaml', transport)

    def test_user_only_other_users_published_articles(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/user_can_query_other_users_published_articles.yaml', transport)

    def test_anonymous_only_published_articles(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/anonymous_can_only_get_published_articles.yaml', transport)

    def test_anonymous_only_published_articles_v1alpha1(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/anonymous_can_only_get_published_articles_v1alpha1.yaml', transport)

    def test_user_cannot_access_remarks_col(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/user_cannot_access_remarks_col.yaml', transport)

    def test_user_can_query_geometry_values_filter(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/user_can_query_geometry_values_filter.yaml', transport)

    def test_user_can_query_geometry_values_filter_session_vars(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/user_can_query_geometry_values_filter_session_vars.yaml', transport)

    def test_user_can_query_jsonb_values_filter(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/user_can_query_jsonb_values_filter.yaml', transport)

    def test_user_can_query_jsonb_values_filter_session_vars(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/user_can_query_jsonb_values_filter_session_vars.yaml', transport)

    def test_artist_select_query_Track_fail(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/artist_select_query_Track_fail.yaml', transport)

    def test_artist_select_query_Track(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/artist_select_query_Track.yaml', transport)

    def test_artist_search_tracks(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/artist_search_tracks.yaml', transport)

    def test_artist_search_tracks_aggregate(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/artist_search_tracks_aggregate.yaml', transport)

    def test_staff_passed_students(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/staff_passed_students.yaml', transport)

    def test_user_query_auction(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/user_query_auction.yaml', transport)

    # FIXME: This test fails nondeterministically: strict=false doesn't seem to
    # work on CI, so just disable for now:
    # @pytest.mark.xfail(reason="Refer https://github.com/hasura/graphql-engine-internal/issues/252")
    # def test_jsonb_has_all(self, hge_ctx, transport):
    #     check_query_f(hge_ctx, self.dir() + '/jsonb_has_all.yaml', transport)

    def test_jsonb_has_any(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/jsonb_has_any.yaml', transport)

    def test_in_and_nin(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/in_and_nin.yaml', transport)

    def test_iregex(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/iregex.yaml', transport)

    def test_user_accessing_books_by_pk_should_fail(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/user_should_not_be_able_to_access_books_by_pk.yaml')

    def test_author_articles_without_required_headers_set(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_articles_without_required_headers.yaml', transport)

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/permissions'

# These tests only test the schema specific stuff which will
# be common across all the backends, to add DB specific tests
# look for the  TestGraphQLInheritedRoles<backend> test classes
@pytest.mark.parametrize('transport', ['http', 'websocket'])
@use_inherited_roles_fixtures
class TestGraphQLInheritedRolesSchema:

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/permissions/inherited_roles'

    setup_metadata_api_version = "v2"

    def test_basic_inherited_role(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/basic_inherited_roles.yaml')

    def test_inherited_role_when_some_roles_may_not_have_permission_configured(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/inherited_role_with_some_roles_having_no_permissions.yaml')

    def test_throw_error_when_roles_form_a_cycle(self, hge_ctx, transport):
        export_metadata_query = {
            "type": "export_metadata",
            "args": {}
        }
        st_code, resp = hge_ctx.v1q(export_metadata_query)
        assert st_code == 200, resp
        circular_roles_metadata = [
            {
                "role_name": "intermediate_circular_role_1",
                "role_set": [
                    "manager_employee",
                    "circular_role"
                ]
            },
            {
                "role_name": "intermediate_circular_role_2",
                "role_set": [
                    "intermediate_circular_role_1",
                    "employee"
                ]
            },
            {
                "role_name": "circular_role",
                "role_set": [
                    "intermediate_circular_role_2",
                    "author"
                ]
            }

        ]
        resp["inherited_roles"] = resp["inherited_roles"] + circular_roles_metadata
        import_metadata_query = {
            "type": "replace_metadata",
            "args": {
                "metadata": resp
            }
        }
        st_code, resp = hge_ctx.v1q(import_metadata_query)
        assert st_code == 400, resp
        assert resp['error'] == '''found cycle(s) in roles: ["circular_role","intermediate_circular_role_2","intermediate_circular_role_1","circular_role"]'''

    def test_explicit_metadata_permission_should_override_role_inheritance(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/override_inherited_permission.yaml')

    def test_inherited_role_inherits_from_inherited_role(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/inherited_role_parent_is_another_inherited_role.yaml')

@pytest.mark.parametrize('transport', ['http', 'websocket'])
@use_inherited_roles_fixtures
class TestGraphQLInheritedRolesPostgres:

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/permissions/inherited_roles'

    setup_metadata_api_version = "v2"

    def test_basic_inherited_role(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/basic_inherited_roles.yaml')

    def test_inherited_role_when_some_roles_may_not_have_permission_configured(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/inherited_role_with_some_roles_having_no_permissions.yaml')

@pytest.mark.parametrize('transport', ['http', 'websocket'])
@pytest.mark.parametrize('backend', ['mssql'])
@usefixtures('per_backend_tests', 'inherited_role_fixtures', 'per_class_tests_db_state')
class TestGraphQLInheritedRolesMSSQL:

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/permissions/inherited_roles_mssql'

    def test_basic_inherited_role(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/basic_inherited_roles.yaml')

    def test_inherited_role_when_some_roles_may_not_have_permission_configured(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/inherited_role_with_some_roles_having_no_permissions.yaml')


@pytest.mark.parametrize("transport", ['http', 'websocket', 'subscription'])
@pytest.mark.parametrize("backend", ['postgres', 'mssql'])
@usefixtures('per_class_tests_db_state')
class TestGraphQLQueryBoolExpSearchCommon:

    def test_city_where_like(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_city_where_like.yaml', transport)

    def test_city_where_not_like(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_city_where_nlike.yaml', transport)

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/boolexp/search'

@pytest.mark.parametrize("transport", ['http', 'websocket'])
@pytest.mark.parametrize("backend", ['postgres'])
@usefixtures('per_class_tests_db_state')
class TestGraphQLQueryBoolExpSearchPostgres:

    def test_city_where_ilike(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_city_where_ilike_postgres.yaml', transport)

    def test_city_where_not_ilike(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_city_where_nilike_postgres.yaml', transport)

    def test_city_where_similar(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_city_where_similar_postgres.yaml', transport)

    def test_city_where_not_similar(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_city_where_not_similar_postgres.yaml', transport)

    def test_city_where_regex(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_city_where_regex_postgres.yaml', transport)

    def test_city_where_nregex(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_city_where_nregex_postgres.yaml', transport)

    def test_city_where_iregex(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_city_where_iregex_postgres.yaml', transport)

    def test_city_where_niregex(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_city_where_niregex_postgres.yaml', transport)

    def test_project_where_ilike(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_project_where_ilike_postgres.yaml', transport)

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/boolexp/search'

@pytest.mark.parametrize("transport", ['http', 'websocket'])
@pytest.mark.parametrize("backend", ['mssql'])
@usefixtures('per_class_tests_db_state')
class TestGraphQLQueryBoolExpSearchMSSQL:

    def test_city_where_like(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_city_where_like_mssql.yaml', transport)

    def test_city_where_not_like(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_city_where_nlike_mssql.yaml', transport)

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/boolexp/search'

@pytest.mark.parametrize("transport", ['http', 'websocket'])
@usefixtures('per_class_tests_db_state')
class TestGraphQLQueryBoolExpJsonB:

    def test_jsonb_contains_article_latest(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_article_author_jsonb_contains_latest.yaml', transport)

    def test_jsonb_contains_article_beststeller(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_author_article_jsonb_contains_bestseller.yaml', transport)

    def test_jsonb_contained_in_latest(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_article_author_jsonb_contained_in_latest.yaml', transport)

    def test_jsonb_contained_in_bestseller_latest(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_article_author_jsonb_contained_in_bestseller_latest.yaml', transport)

    def test_jsonb_has_key_sim_type(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_product_jsonb_has_key_sim_type.yaml', transport)

    def test_jsonb_has_keys_any_os_operating_system(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_product_jsonb_has_keys_any_os_operating_system.yaml', transport)

    def test_jsonb_has_keys_all_touchscreen_ram(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_product_jsonb_has_keys_all_ram_touchscreen.yaml', transport)

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/boolexp/jsonb'

@pytest.mark.parametrize("transport", ['http', 'websocket', 'subscription'])
@usefixtures('per_class_tests_db_state')
class TestGraphQLQueryBoolExpPostGIS:

    def test_query_using_point(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_using_point.yaml', transport)

    def test_query_using_line(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_using_line.yaml', transport)

    def test_query_using_polygon(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_using_polygon.yaml', transport)

    def test_query_geography_spatial_ops(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_geography_spatial_ops.yaml', transport)

    def test_query_geometry_3d_spatial_ops(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_geometry_3d_spatial_ops.yaml', transport)

    def test_query_cast_geometry_to_geography(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_cast_geometry_to_geography.yaml', transport)

    def test_query_cast_geography_to_geometry(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_cast_geography_to_geometry.yaml', transport)

    def test_query_illegal_cast_is_not_allowed(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_illegal_cast_is_not_allowed.yaml', transport)

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/boolexp/postgis'

@pytest.mark.parametrize("transport", ['http', 'websocket'])
@usefixtures('per_class_tests_db_state')
class TestGraphQLQueryBoolExpRaster:

    def test_query_st_intersects_geom_nband(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_st_intersects_geom_nband.yaml', transport)

    def test_query_st_intersects_geom_nband_no_rows(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_st_intersects_geom_nband_no_rows.yaml', transport)

    def test_query_st_intersects_rast(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_st_intersects_rast.yaml', transport)

    def test_query_st_intersects_rast_no_rows(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_st_intersects_rast_no_rows.yaml', transport)

    def test_query_st_intersects_rast_fail(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_st_intersects_rast_fail.yaml', transport)

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/boolexp/raster'

@pytest.mark.parametrize("transport", ['http', 'websocket'])
@usefixtures('per_class_tests_db_state')
class TestGraphQLQueryOrderBy:
    def test_articles_order_by_without_id(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/articles_order_by_without_id.yaml', transport)

    def test_articles_order_by_rel_author_id(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/articles_order_by_rel_author_id.yaml', transport)

    def test_articles_order_by_rel_author_rel_contact_phone(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/articles_order_by_rel_author_rel_contact_phone.yaml', transport)

    def test_articles_order_by_null(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/articles_order_by_null.yaml', transport)

    def test_album_order_by_tracks_count(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/album_order_by_tracks_count.yaml', transport)

    def test_album_order_by_tracks_duration_avg(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/album_order_by_tracks_duration_avg.yaml', transport)

    def test_album_order_by_tracks_max_name(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/album_order_by_tracks_max_name.yaml', transport)

    def test_album_order_by_tracks_bytes_stddev(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/album_order_by_tracks_bytes_stddev.yaml', transport)

    def test_employee_distinct_department_order_by_salary_desc(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/employee_distinct_department_order_by_salary_desc.yaml', transport)

    def test_employee_distinct_department_order_by_salary_asc(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/employee_distinct_department_order_by_salary_asc.yaml', transport)

    def test_employee_distinct_fail(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/employee_distinct_fail.yaml', transport)

    def test_album_order_by_tracks_tags(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/album_order_by_tracks_tags.yaml', transport)

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/order_by'

@usefixtures('per_class_tests_db_state')
class TestGraphQLQueryFunctions:

    @pytest.mark.parametrize("transport", ['http', 'websocket'])
    def test_search_posts(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/query_search_posts.yaml")

    @pytest.mark.parametrize("transport", ['http', 'websocket'])
    def test_search_posts_aggregate(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/query_search_posts_aggregate.yaml")

    @pytest.mark.parametrize("transport", ['http', 'websocket'])
    def test_query_get_users(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/query_get_users.yaml", transport)

    @pytest.mark.parametrize("transport", ['http', 'websocket'])
    def test_query_get_users_arguments_error(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/query_get_users_arguments_error.yaml", transport)

    @pytest.mark.parametrize("transport", ['http', 'websocket'])
    def test_query_get_users_default_arguments_error(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/query_get_users_default_arguments_error.yaml", transport)

    def test_alter_function_error(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/alter_function_error.yaml')

    def test_overloading_function_error(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/overloading_function_error.yaml')

    def test_query_get_test_uuid(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/query_get_test_uuid.yaml')

    def test_query_my_add(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/query_my_add.yaml')

    @pytest.mark.parametrize("transport", ['http', 'websocket'])
    def test_query_get_session_var(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_get_session_var.yaml', transport)

    def test_track_function_v2_errors(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/track_function_v2_errors.yaml')

    @pytest.mark.parametrize("transport", ['http', 'websocket'])
    def test_query_get_test_session_id(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_get_test_session_id.yaml')

    @pytest.mark.parametrize("transport", ['http', 'websocket'])
    def test_query_search_author_mview(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_search_author_mview.yaml')

    def test_tracking_function_with_composite_type_argument(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/track_non_base_function_arg_type.yaml')

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/functions'

@pytest.mark.parametrize("transport", ['http', 'websocket'])
@usefixtures('per_class_tests_db_state')
class TestGraphQLQueryCustomSchema:

    def test_author(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/author.yaml', transport)

    def test_article(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/article.yaml', transport)

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/custom_schema'

@pytest.mark.parametrize("transport", ['http', 'websocket'])
@usefixtures('per_class_tests_db_state')
class TestGraphQLQueryCustomTableName:

    def test_author(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + 'author.yaml', transport)

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/custom_schema/custom_table_name/'

@pytest.mark.parametrize('transport', ['http', 'websocket'])
@usefixtures('per_class_tests_db_state')
class TestGraphQLQueryEnums:
    @classmethod
    def dir(cls):
        return 'queries/graphql_query/enums'

    def test_introspect(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/introspect.yaml', transport)

    def test_introspect_user_role(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/introspect_user_role.yaml', transport)

    def test_select_enum_field(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_enum_field.yaml', transport)

    def test_select_where_enum_eq(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_where_enum_eq.yaml', transport)

    def test_select_where_enum_eq_bad_value(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_where_enum_eq_bad_value.yaml', transport)

    def test_select_where_enum_eq_string(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_where_enum_eq_string.yaml', transport)

    def test_select_where_enum_eq_variable(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_where_enum_eq_variable.yaml', transport)

    def test_select_where_enum_eq_variable_bad_value(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_where_enum_eq_variable_bad_value.yaml', transport)

    def test_select_where_enum_eq_without_enum_table_visibility(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_where_enum_eq_without_enum_table_visibility.yaml', transport)

@pytest.mark.parametrize('transport', ['http', 'websocket'])
@usefixtures('per_class_tests_db_state')
class TestGraphQLQueryComputedFields:
    @classmethod
    def dir(cls):
        return 'queries/graphql_query/computed_fields'

    def test_computed_fields(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/computed_fields.yaml', transport)

    def test_computed_fields_permission(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/computed_fields_permission.yaml', transport)

    def test_locations(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/locations.yaml', transport)

    def test_float_test(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/float_test.yaml', transport)

    def test_tracked_function_as_computed_field(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/tracked_function_as_comp_field.yaml')

    def test_scalar_computed_field_filter(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/scalar_computed_field_filter.yaml')

    def test_table_computed_field_filter(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/table_computed_field_filter.yaml')

    def test_table_computed_field_filter_fail(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/table_computed_field_filter_fail.yaml')

@pytest.mark.parametrize('transport', ['http', 'websocket'])
@usefixtures('per_class_tests_db_state')
class TestGraphQLQueryCaching:
    @classmethod
    def dir(cls):
        return 'queries/graphql_query/caching'

    def test_include_directive(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/include_directive.yaml', transport)

    def test_introspection(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/introspection.yaml', transport)

@pytest.mark.skipif(
    not PytestConf.config.getoption("--test-unauthorized-role"),
    reason="--test-unauthorized-role missing"
)
@pytest.mark.parametrize('transport', ['http', 'websocket'])
@usefixtures('per_class_tests_db_state')
class TestUnauthorizedRolePermission:
    @classmethod
    def dir(cls):
        return 'queries/unauthorized_role'

    def test_unauth_role(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/unauthorized_role.yaml', transport, False)

@pytest.mark.parametrize("backend", ['postgres', 'mssql'])
@usefixtures('per_class_tests_db_state')
class TestGraphQLExplain:
    @classmethod
    def dir(cls):
        return 'queries/explain'

    def test_simple_query_as_admin(self, hge_ctx, backend):
        q = {"query": {"query": "query abc { __typename }", "operationName": "abc"}}
        st_code, resp = hge_ctx.v1GraphqlExplain(q)
        assert st_code == 200, resp

    def test_simple_query_as_user(self, hge_ctx, backend):
        q = {"query": {"query": "query abc { __typename }", "operationName": "abc"}}
        st_code, resp = hge_ctx.v1GraphqlExplain(q, {"x-hasura-role": "random_user"})
        assert st_code == 400, resp

    def test_simple_query_as_admin_with_user_role(self, hge_ctx, backend):
        self.with_admin_secret(hge_ctx, self.dir() + hge_ctx.backend_suffix('/permissions_query') + ".yaml")

    def test_simple_query(self, hge_ctx, backend):
        self.with_admin_secret(hge_ctx, self.dir() + hge_ctx.backend_suffix('/simple_query') + ".yaml")

    def test_permissions_query(self, hge_ctx, backend):
        self.with_admin_secret(hge_ctx, self.dir() + hge_ctx.backend_suffix('/permissions_query') + ".yaml")

    def with_admin_secret(self, hge_ctx, f, hdrs=None, req_st=200):
        conf = get_conf_f(f)
        admin_secret = hge_ctx.hge_key
        headers = {}
        if hdrs != None:
            headers = hdrs
        elif admin_secret and hdrs == None:
            headers['X-Hasura-Admin-Secret'] = admin_secret
        status_code, resp_json, _ = hge_ctx.anyq(conf['url'], conf['query'], headers)
        assert status_code == req_st, resp_json

        if req_st != 200:
            # return early in case we're testing for failures
            return
        
        # Comparing only with generated 'sql' since the 'plan' may differ
        resp_sql = resp_json[0]['sql']
        exp_sql = conf['response'][0]['sql']
        assert resp_sql == exp_sql, resp_json

@pytest.mark.parametrize('transport', ['http', 'websocket'])
@usefixtures('per_class_tests_db_state')
class TestRelayQueriesBasic:

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/relay/basic'

    def test_article_connection(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/article_connection.yaml', transport)

    def test_author_connection(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/author_connection.yaml', transport)

    def test_author_with_articles_view_connection(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/author_with_articles_view_connection.yaml', transport)

    def test_search_articles_connection(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/search_articles_connection.yaml', transport)

    def test_node(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/node.yaml', transport)

    def test_only_pageinfo(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/only_pageinfo.yaml', transport)

    # Articles forward pagination
    def test_article_no_orderby_forward_pagination(self, hge_ctx, transport):
        _test_relay_pagination(hge_ctx, transport, self.dir() + "/article_pagination_no_orderby/forward", 3)

    # Articles backward pagination
    def test_article_no_orderby_backward_pagination(self, hge_ctx, transport):
        _test_relay_pagination(hge_ctx, transport, self.dir() + "/article_pagination_no_orderby/backward", 3)

    # Authors forward pagination
    def test_author_orderby_articles_aggregate_orderby_forward_pagination(self, hge_ctx, transport):
        _test_relay_pagination(hge_ctx, transport, self.dir() + "/author_pagination_articles_aggregate_orderby/forward", 2)

    # Authors backward pagination
    def test_author_orderby_articles_aggregate_orderby_backward_pagination(self, hge_ctx, transport):
        _test_relay_pagination(hge_ctx, transport, self.dir() + "/author_pagination_articles_aggregate_orderby/backward", 3)

    # Pagination errors
    def test_first_and_last_fail(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/pagination_errors/first_and_last.yaml", transport)

    def test_after_and_before_fail(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/pagination_errors/after_and_before.yaml", transport)

    # Node id errors
    def test_insufficient_data(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/node_id_errors/insufficient_data.yaml', transport)

    def test_invalid_column_value(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/node_id_errors/invalid_column_value.yaml', transport)

    def test_invalid_id(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/node_id_errors/invalid_id.yaml', transport)

    def test_missing_columns(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/node_id_errors/missing_columns.yaml', transport)

    def test_non_array_id(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/node_id_errors/non_array_id.yaml', transport)

    def test_unexpected_columns(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/node_id_errors/unexpected_columns.yaml', transport)

    def test_invalid_node_id_version(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/node_id_errors/invalid_node_id_version.yaml', transport)

    def test_non_integer_version(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/node_id_errors/non_integer_version.yaml', transport)

@pytest.mark.parametrize('transport', ['http', 'websocket'])
@usefixtures('per_class_tests_db_state')
class TestRelayQueriesPermissions:

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/relay/permissions'

    def test_author_connection(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/author_connection.yaml', transport)

    def test_author_node(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/author_node.yaml', transport)

    def test_author_node_null(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/author_node_null.yaml', transport)

    # Article forward pagination
    def test_article_pagination_forward(self, hge_ctx, transport):
        _test_relay_pagination(hge_ctx, transport, self.dir() + '/article_pagination/forward', 2)

    def test_article_pagination_backward(self, hge_ctx, transport):
        _test_relay_pagination(hge_ctx, transport, self.dir() + '/article_pagination/backward', 2)

def _test_relay_pagination(hge_ctx, transport, test_file_prefix, no_of_pages):
    for i in range(no_of_pages):
        page_no = i + 1
        test_file = "page_" + str(page_no) + ".yaml"
        check_query_f(hge_ctx, test_file_prefix + "/" + test_file, transport)

use_function_permission_fixtures = pytest.mark.usefixtures(
    'per_method_tests_db_state',
    'functions_permissions_fixtures'
)

@pytest.mark.parametrize('transport', ['http', 'websocket'])
@use_function_permission_fixtures
class TestGraphQLQueryFunctionPermissions:

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/functions/permissions/'

    def test_access_function_without_permission_configured(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + 'get_articles_without_permission_configured.yaml')

    def test_access_function_with_permission_configured(self, hge_ctx, transport):
        st_code, resp = hge_ctx.v1metadataq_f(self.dir() + 'add_function_permission_get_articles.yaml')
        assert st_code == 200, resp
        check_query_f(hge_ctx, self.dir() + 'get_articles_with_permission_configured.yaml')

@pytest.mark.parametrize('transport', ['http', 'websocket'])
@usefixtures('per_class_tests_db_state')
class TestGraphQLQueryBoolExpLtree:
    def test_select_path_where_ancestor(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_path_where_ancestor.yaml')

    def test_select_path_where_ancestor_array(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_path_where_ancestor_array.yaml')

    def test_select_path_where_descendant(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_path_where_descendant.yaml')

    def test_select_path_where_descendant_array(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_path_where_descendant_array.yaml')

    def test_select_path_where_matches(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_path_where_matches.yaml')

    def test_select_path_where_matches_array(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_path_where_matches_array.yaml')

    def test_select_path_where_matches_ltxtquery(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_path_where_matches_ltxtquery.yaml')

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/boolexp/ltree'

@pytest.mark.parametrize("transport", ['http', 'websocket'])
@pytest.mark.parametrize("backend", ['mssql'])
@usefixtures('per_class_tests_db_state')
class TestGraphQLQueryBoolExpSpatialMSSQL:
    @pytest.mark.skip_server_upgrade_test
    def test_select_spatial_mssql_types(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_spatial_types_mssql.yaml', transport)

    def test_select_spatial_mssql_types_where_st_equals(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_spatial_types_where_st_equals_mssql.yaml', transport)

    def test_select_spatial_mssql_types_where_st_contains(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_spatial_types_where_st_contains_mssql.yaml', transport)

    def test_select_spatial_mssql_types_where_st_crosses(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_geom_where_st_crosses_mssql.yaml', transport)

    def test_select_spatial_mssql_types_where_st_intersects(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_spatial_types_where_st_intersects_mssql.yaml', transport)

    def test_select_spatial_mssql_types_where_st_overlaps(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_spatial_types_where_st_overlaps_mssql.yaml', transport)

    def test_select_spatial_mssql_types_where_st_within(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_spatial_types_where_st_within_mssql.yaml', transport)

    def test_select_spatial_mssql_types_where_st_touches(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_spatial_types_where_st_touches_mssql.yaml', transport)

    def test_select_spatial_mssql_types_where_st_crosses_geojson(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_geom_where_st_crosses_geojson_mssql.yaml', transport)

    def test_select_spatial_mssql_types_where_st_contains_geojson(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_spatial_types_where_st_contains_geojson_mssql.yaml', transport)

    def test_select_spatial_mssql_types_where_st_equals_geojson(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_spatial_types_where_st_equals_geojson_mssql.yaml', transport)

    def test_select_spatial_mssql_types_where_st_intersects_geojson(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_spatial_types_where_st_intersects_geojson_mssql.yaml', transport)

    def test_select_spatial_mssql_types_where_st_touches_geojson(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_spatial_types_where_st_touches_geojson_mssql.yaml', transport)

    def test_select_spatial_mssql_types_where_st_within_geojson(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_spatial_types_where_st_within_geojson_mssql.yaml', transport)

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/boolexp/spatial'
