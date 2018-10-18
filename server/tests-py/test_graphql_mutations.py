
import pytest
import yaml
from validate import check_query_f

class TestGraphQLInsert(object):

    def test_inserts_author_article(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + "/author_article.yaml")

    def test_insert_using_variable(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + "/person_jsonb_variable.yaml")

    def test_insert_using_array_variable(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + "/person_jsonb_variable_array.yaml")

    def test_insert_null_col_value(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + "/order_col_shipped_null.yaml")

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        self.dir = "queries/graphql_mutation/insert/basic"
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp


class TestGraphqlInsertOnConflict(object):

    def test_on_conflict_update(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/article_on_conflict_update.yaml")

    def test_on_conflict_no_action_specified(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/article_on_conflict_no_action_specified.yaml")

    def test_on_conflict_ignore(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/article_on_conflict_ignore_constraint.yaml")

    def test_on_conflict_update_empty_cols(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/article_on_conflict_empty_update_columns.yaml")

    def test_err_missing_article_constraint(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/article_on_conflict_error_missing_article_constraint.yaml")

    def test_err_unexpected_action(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/article_unexpected_on_conflict_action.yaml")

    def test_err_unexpected_constraint(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/article_unexpected_on_conflict_constraint_error.yaml")

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        self.dir = "queries/graphql_mutation/insert/onconflict"
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp



class TestGraphqlInsertPermission(object):

    def test_user_role_on_conflict_update(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + "/article_on_conflict_user_role.yaml")

    @pytest.mark.xfail(reason="Refer https://github.com/hasura/graphql-engine/issues/563")
    def test_user_role_on_conflict_ignore(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + "/author_on_conflict_ignore_user_role.yaml")

    def test_user_on_conflict_err_no_action_specified(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + "/user_article_on_conflict_err_no_action_specified.yaml")

    def test_user_err_missing_article_constraint(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + "/user_article_on_conflict_error_missing_article_constraint.yaml")

    def test_user_err_unexpected_action(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + "/user_article_error_unexpected_on_conflict_action.yaml")

    def test_user_err_unexpected_constraint(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/user_article_unexpected_on_conflict_constraint_error.yaml")

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

    def test_company_user_role_insert(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/company_user_role.yaml")

    def test_company_user_role_insert_on_conflict(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/company_user_role_on_conflict.yaml")

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        self.dir = "queries/graphql_mutation/insert/permissions"
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp


class TestGraphqlInsertConstraints(object):

    def test_address_not_null_constraint_err(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/address_not_null_constraint_error.yaml")

    def test_insert_unique_constraint_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + "/author_unique_constraint_error.yaml")

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        self.dir = "queries/graphql_mutation/insert/constraints"
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp

class TestGraphqlNestedInserts(object):

    def test_author_with_articles(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + "/author_with_articles.yaml")

    def test_author_with_articles_author_id_fail(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + "/author_with_articles_author_id_fail.yaml")

    def test_articles_with_author(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + "/articles_with_author.yaml")

    def test_articles_with_author_author_id_fail(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + "/articles_with_author_author_id_fail.yaml")

    def test_author_upsert_articles_fail(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + "/author_upsert_articles_fail.yaml")

    def test_articles_author_upsert_fail(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + "/articles_author_upsert_fail.yaml")

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        self.dir = "queries/graphql_mutation/insert/nested"
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp

class TestGraphqlInsertViews(object):

    def test_insert_view_author_simple(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + "/insert_view_author_simple.yaml")

    def test_insert_view_author_complex_fail(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + "/insert_view_author_complex_fail.yaml")

    def test_nested_insert_article_author_simple_view(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + "/nested_insert_article_author_simple_view.yaml")

    def test_nested_insert_article_author_complex_view_fail(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + "/nested_insert_article_author_complex_view_fail.yaml")

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        self.dir = "queries/graphql_mutation/insert/views"
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp

class TestGraphqlUpdateBasic:

    def test_set_author_name(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/author_set_name.yaml")

    def test_set_person_details(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/person_set_details.yaml")

    def test_person_id_inc(self,  hge_ctx):
       check_query_f(hge_ctx, self.dir + "/person_inc.yaml")

    def test_no_operator_err(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/person_error_no_operator.yaml")

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        self.dir = "queries/graphql_mutation/update/basic"
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp


class TestGraphqlUpdateJsonB:

    def test_jsonb_append_object(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/person_append_object.yaml")

    def test_jsonb_append_array(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/person_append_array.yaml")

    def test_jsonb_prepend_array(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/person_prepend_array.yaml")

    def test_jsonb_delete_at_path(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/person_delete_at_path.yaml")

    def test_jsonb_delete_array_element(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/person_delete_array_element.yaml")

    def test_jsonb_delete_key(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/person_delete_key.yaml")

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        self.dir = "queries/graphql_mutation/update/jsonb"
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp


class TestGraphqlUpdatePermissions:

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
        self.dir = "queries/graphql_mutation/update/permissions"
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp


class TestGraphqlDeleteBasic:

    def test_article_delete(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/article.yaml")

    def test_article_delete_returning(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/article_returning.yaml")

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        self.dir = "queries/graphql_mutation/delete/basic"
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp


class TestGraphqlDeleteConstraints:

    def test_author_delete_foreign_key_violation(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/author_foreign_key_violation.yaml")

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        self.dir = "queries/graphql_mutation/delete/constraints"
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp


class TestGraphqlDeletePermissions:

    def test_author_can_delete_his_articles(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/author_can_delete_his_articles.yaml")

    def test_author_cannot_delete_other_users_articles(self, hge_ctx):
       check_query_f(hge_ctx, self.dir + "/author_cannot_delete_other_users_articles.yaml")

    def test_resident_delete_without_select_perm_fail(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + "/resident_delete_without_select_perm_fail.yaml")

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        self.dir = "queries/graphql_mutation/delete/permissions"
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp
