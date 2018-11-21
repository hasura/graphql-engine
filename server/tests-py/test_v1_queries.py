
import pytest
import yaml
from validate import check_query_f

class TestV1SelectBasic:

    def test_select_query_author(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_article.yaml')

    def test_nested_select_article_author(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/nested_select_query_article_author.yaml')

    def test_select_author_where(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_query_author_where.yaml')

    def test_select_col_not_present(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_article_col_not_present_err.yaml')
    def test_nested_select_query_where(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/nested_select_where_query_author_article.yaml')

    def test_select_query_user(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_user.yaml')

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        self.dir = "queries/v1/select/basic"
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp


class TestV1SelectLimits:

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
        self.dir = 'queries/v1/select/limits'
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp


class TestV1SelectOffset:

    def test_offset_1_limit_2(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_query_article_offset_1_limit_2.yaml')

    def test_offset_2_limit_1(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_query_article_offset_2_limit_1.yaml')

    def test_int_as_string_offset_error(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_query_article_int_as_string_offset_error.yaml')

    def test_err_neg_offset_error(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_query_article_neg_offset_error.yaml')

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        self.dir = 'queries/v1/select/offset'
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp


class TestV1SelectBoolExpBasic:

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
        self.dir = 'queries/v1/select/boolexp/basic'
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp


class TestV1SelectBoolExpSearch:

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
        self.dir = 'queries/v1/select/boolexp/search'
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp


class TestV1SelectPermissions:

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
        self.dir = 'queries/v1/select/permissions'
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp


class TestV1InsertBasic:

    def test_insert_author(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/insert_author.yaml')

    def test_insert_author_col_not_present_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/insert_author_col_not_present_err.yaml')

    def test_insert_null_col_value(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + "/order_col_shipped_null.yaml")

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):  
        self.dir = "queries/v1/insert/basic"    
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp


class TestV1InsertOnConflict:
   
    def test_author_on_conflict_update(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/upsert_author.yaml')

    def test_on_conflict_no_action_specified(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/article_on_conflict_no_action_specified.yaml")

    def test_on_conflict_ignore(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/article_on_conflict_ignore_constraint.yaml")

    def test_err_missing_article_constraint(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/article_on_conflict_error_missing_article_constraint.yaml")

    def test_err_unexpected_action(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/article_unexpected_on_conflict_action.yaml")

    def test_err_unexpected_constraint(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/article_unexpected_on_conflict_constraint_error.yaml")

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):  
        self.dir = "queries/v1/insert/onconflict"    
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp


class TestV1InsertPermissions(object):

    def test_user_role_on_conflict_update(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + "/article_on_conflict_user_role.yaml")

    @pytest.mark.xfail(reason="Refer https://github.com/hasura/graphql-engine/issues/563")
    def test_user_role_on_conflict_ignore(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + "/author_on_conflict_ignore_user_role.yaml")
    
    def test_role_has_no_permissions_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + "/address_permission_error.yaml")

    def test_author_user_role_insert_check_perm_success(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + "/author_user_role_insert_check_perm_success.yaml")

    def test_user_role_insert_check_is_registered_fail(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + "/author_user_role_insert_check_is_registered_fail.yaml")

    def test_user_role_insert_check_user_id_fail(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + "/author_user_role_insert_check_user_id_fail.yaml")

    def test_student_role_insert_check_bio_success(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + "/author_student_role_insert_check_bio_success.yaml")

    def test_student_role_insert_check_bio_fail(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + "/author_student_role_insert_check_bio_fail.yaml")

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        self.dir = "queries/v1/insert/permissions"
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp


class TestV1UpdateBasic:

    def test_set_author_name(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/author_set_name.yaml")

    def test_set_person_details(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/person_set_details.yaml")

    def test_person_id_inc(self,  hge_ctx):
       check_query_f(hge_ctx, self.dir + "/person_inc.yaml")

    def test_product_mul_price(self,  hge_ctx):
       check_query_f(hge_ctx, self.dir + "/product_mul_price.yaml")

    def test_product_set_default_price(self,  hge_ctx):
       check_query_f(hge_ctx, self.dir + "/product_set_default_price.yaml")

    def test_no_operator_err(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/person_error_no_operator.yaml")

    def test_no_where_clause_err(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/person_error_no_where_clause.yaml")

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        self.dir = "queries/v1/update/basic"
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp


class TestV1UpdatePermissions:

    def test_user_can_update_unpublished_article(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/user_can_update_unpublished_article.yaml")

    def test_user_cannot_update_published_version_col(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/user_cannot_update_published_article_version.yaml")

    def test_user_cannot_update_another_users_article(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/user_cannot_update_another_users_article.yaml")

    def test_user_cannot_update_id_col(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/user_cannot_update_id_col_article.yaml")

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        self.dir = "queries/v1/update/permissions"
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp


class TestV1Delete:
 
    def test_delete_author(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/delete_article.yaml')

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):  
        self.dir = "queries/v1/delete"    
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp

class TestMetadata:

    def test_reload_metadata(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/reload_metadata.yaml')

    def test_export_metadata(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/export_metadata.yaml')

    def test_clear_metadata(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/clear_metadata.yaml')

    def test_dump_internal_state(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/dump_internal_state.yaml')

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):  
        self.dir = "queries/v1/metadata"    
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp


class TestRunSQL:

    def test_select_query(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/sql_select_query.yaml')

    def test_set_timezone(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/sql_set_timezone.yaml')

    def test_sql_timezone__error(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/sql_set_timezone_error.yaml')

    def test_sql_query_as_user_error(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/sql_query_as_user_error.yaml')

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        self.dir = "queries/v1/run_sql"
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp


class TestRelationships:

    def test_object_relationship_foreign_key(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/object_relationship_foreign_key.yaml')

    def test_create_object_relationship_as_not_admin_error(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/create_object_relationship_as_not_admin_error.yaml')

    def test_object_relationship_col_not_foreign_key_error(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/object_relationship_col_not_foreign_key_error.yaml')

    def test_object_relationship_foreign_key_non_public_schema(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/object_relationship_non_public_schema_foreign_key.yaml')

    def test_object_relationship_manual(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/object_relationship_manual.yaml')

    def test_array_relationship_foreign_key(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/array_relationship_foreign_key.yaml')

    def test_create_array_relationship_as_not_admin_error(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/create_array_relationship_as_not_admin_error.yaml')

    def test_array_relationship_col_not_foreign_key_error(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/array_relationship_col_not_foreign_key_error.yaml')

    def test_array_relationship_foreign_key_non_public_schema(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/array_relationship_non_public_schema_foreign_key.yaml')

    def test_array_relationship_manual(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/array_relationship_manual.yaml')

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        self.dir = "queries/v1/relationships"
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp


class TestTrackTables:

    def test_track_untrack_table(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/track_untrack_table.yaml')

    def test_track_untrack_table_non_public_schema(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/track_untrack_table_non_public_schema.yaml')

    def test_track_untrack_table_as_not_admin_error(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/track_untrack_table_as_not_admin_error.yaml')

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        self.dir = "queries/v1/track_table"
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp
