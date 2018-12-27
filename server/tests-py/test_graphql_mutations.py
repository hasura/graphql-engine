import pytest
import yaml
from validate import check_query_f
from super_classes import DefaultTestQueries


class TestGraphQLInsert(DefaultTestQueries):

    def test_inserts_author_article(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/author_article.yaml")
        hge_ctx.may_skip_test_teardown = True

    def test_inserts_various_postgres_types(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/insert_various_postgres_types.yaml")
        hge_ctx.may_skip_test_teardown = True

    @pytest.mark.xfail(reason="Refer https://github.com/hasura/graphql-engine/issues/348")
    def test_insert_into_array_col_with_array_input(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/insert_into_array_col_with_array_input.yaml")
        hge_ctx.may_skip_test_teardown = True

    def test_insert_using_variable(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/person_jsonb_variable.yaml")
        hge_ctx.may_skip_test_teardown = True

    def test_insert_using_array_variable(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/person_jsonb_variable_array.yaml")
        hge_ctx.may_skip_test_teardown = True

    def test_insert_person(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/person_jsonb.yaml")
        hge_ctx.may_skip_test_teardown = True

    def test_insert_person_array(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/person_jsonb_array.yaml")
        hge_ctx.may_skip_test_teardown = True

    def test_insert_null_col_value(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/order_col_shipped_null.yaml")
        hge_ctx.may_skip_test_teardown = True

    @classmethod
    def dir(cls):
        return "queries/graphql_mutation/insert/basic"


class TestGraphqlInsertOnConflict(DefaultTestQueries):

    def test_on_conflict_update(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/article_on_conflict_update.yaml")

    def test_on_conflict_ignore(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/article_on_conflict_ignore_constraint.yaml")
        hge_ctx.may_skip_test_teardown = True

    def test_on_conflict_update_empty_cols(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/article_on_conflict_empty_update_columns.yaml")
        hge_ctx.may_skip_test_teardown = True

    def test_err_missing_article_constraint(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/article_on_conflict_error_missing_article_constraint.yaml")

    def test_err_unexpected_action(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/article_unexpected_on_conflict_action.yaml")

    def test_err_unexpected_constraint(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/article_unexpected_on_conflict_constraint_error.yaml")

    @classmethod
    def dir(cls):
        return "queries/graphql_mutation/insert/onconflict"


class TestGraphqlInsertPermission(DefaultTestQueries):

    def test_user_role_on_conflict_update(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/article_on_conflict_user_role.yaml")

    def test_user_role_on_conflict_constraint_on_error(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/article_on_conflict_constraint_on_user_role_error.yaml")

    def test_user_role_on_conflict_ignore(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/author_on_conflict_ignore_user_role.yaml")
        hge_ctx.may_skip_test_teardown = True

    def test_user_err_missing_article_constraint(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/user_article_on_conflict_error_missing_article_constraint.yaml")

    def test_user_err_unexpected_action(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/user_article_error_unexpected_on_conflict_action.yaml")

    def test_user_err_unexpected_constraint(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/user_article_unexpected_on_conflict_constraint_error.yaml")

    def test_role_has_no_permissions_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/address_permission_error.yaml")

    def test_author_user_role_insert_check_perm_success(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/author_user_role_insert_check_perm_success.yaml")

    def test_user_role_insert_check_is_registered_fail(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/author_user_role_insert_check_is_registered_fail.yaml")

    def test_user_role_insert_check_user_id_fail(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/author_user_role_insert_check_user_id_fail.yaml")

    def test_student_role_insert_check_bio_success(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/author_student_role_insert_check_bio_success.yaml")

    def test_student_role_insert_check_bio_fail(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/author_student_role_insert_check_bio_fail.yaml")

    def test_company_user_role_insert(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/company_user_role.yaml")

    def test_company_user_role_insert_on_conflict(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/company_user_role_on_conflict.yaml")

    def test_resident_user_role_insert(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/resident_user.yaml")

    def test_resident_infant_role_insert(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/resident_infant.yaml")

    def test_resident_infant_role_insert_fail(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/resident_infant_fail.yaml")

    def test_resident_5_modifies_resident_6_upsert(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/resident_5_modifies_resident_6_upsert.yaml")

    @classmethod
    def dir(cls):
        return "queries/graphql_mutation/insert/permissions"


class TestGraphqlInsertConstraints(DefaultTestQueries):

    def test_address_not_null_constraint_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/address_not_null_constraint_error.yaml")

    def test_insert_unique_constraint_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/author_unique_constraint_error.yaml")

    @classmethod
    def dir(cls):
        return "queries/graphql_mutation/insert/constraints"


class TestGraphqlInsertGeoJson(DefaultTestQueries):

    def test_insert_point_landmark(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/insert_landmark.yaml")
        hge_ctx.may_skip_test_teardown = True

    def test_insert_3d_point_drone_loc(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/insert_drone_3d_location.yaml")
        hge_ctx.may_skip_test_teardown = True

    def test_insert_landmark_single_position_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/insert_landmark_single_position_err.yaml")

    def test_insert_line_string_road(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/insert_road.yaml")
        hge_ctx.may_skip_test_teardown = True

    def test_insert_road_single_point_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/insert_road_single_point_err.yaml")

    def test_insert_multi_point_service_locations(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/insert_service_locations.yaml")
        hge_ctx.may_skip_test_teardown = True

    def test_insert_multi_line_string_route(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/insert_route.yaml")
        hge_ctx.may_skip_test_teardown = True

    def test_insert_polygon(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/insert_area.yaml")
        hge_ctx.may_skip_test_teardown = True

    def test_insert_linear_ring_less_than_4_points_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/insert_area_less_than_4_points_err.yaml")

    def test_insert_linear_ring_last_point_not_equal_to_first_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/insert_linear_ring_last_point_not_equal_to_first_err.yaml")

    def test_insert_multi_polygon_compounds(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/insert_compounds.yaml")
        hge_ctx.may_skip_test_teardown = True

    def test_insert_geometry_collection(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/insert_geometry_collection.yaml")
        hge_ctx.may_skip_test_teardown = True

    def test_insert_unexpected_geometry_type_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/insert_geometry_unexpected_type_err.yaml")

    @classmethod
    def dir(cls):
        return "queries/graphql_mutation/insert/geojson"


class TestGraphqlNestedInserts(DefaultTestQueries):

    def test_author_with_articles(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/author_with_articles.yaml")

    def test_author_with_articles_author_id_fail(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/author_with_articles_author_id_fail.yaml")

    def test_articles_with_author(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/articles_with_author.yaml")

    def test_articles_with_author_author_id_fail(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/articles_with_author_author_id_fail.yaml")

    def test_author_upsert_articles_fail(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/author_upsert_articles_fail.yaml")

    def test_articles_author_upsert_fail(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/articles_author_upsert_fail.yaml")

    @classmethod
    def dir(cls):
        return "queries/graphql_mutation/insert/nested"


class TestGraphqlInsertViews(DefaultTestQueries):

    def test_insert_view_author_simple(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/insert_view_author_simple.yaml")

    def test_insert_view_author_complex_fail(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/insert_view_author_complex_fail.yaml")

    def test_nested_insert_article_author_simple_view(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/nested_insert_article_author_simple_view.yaml")

    def test_nested_insert_article_author_complex_view_fail(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/nested_insert_article_author_complex_view_fail.yaml")

    @classmethod
    def dir(cls):
        return "queries/graphql_mutation/insert/views"


class TestGraphqlUpdateBasic(DefaultTestQueries):

    def test_set_author_name(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/author_set_name.yaml")

    def test_set_person_details(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/person_set_details.yaml")

    def test_person_id_inc(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/person_inc.yaml")

    def test_no_operator_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/person_error_no_operator.yaml")

    @classmethod
    def dir(cls):
        return "queries/graphql_mutation/update/basic"


class TestGraphqlUpdateJsonB(DefaultTestQueries):

    def test_jsonb_append_object(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/person_append_object.yaml")

    def test_jsonb_append_array(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/person_append_array.yaml")

    def test_jsonb_prepend_array(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/person_prepend_array.yaml")

    def test_jsonb_delete_at_path(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/person_delete_at_path.yaml")

    def test_jsonb_delete_array_element(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/person_delete_array_element.yaml")

    def test_jsonb_delete_key(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/person_delete_key.yaml")

    @classmethod
    def dir(cls):
        return "queries/graphql_mutation/update/jsonb"


class TestGraphqlUpdatePermissions(DefaultTestQueries):

    def test_user_can_update_unpublished_article(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/user_can_update_unpublished_article.yaml")

    def test_user_cannot_update_published_version_col(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/user_cannot_update_published_article_version.yaml")
        hge_ctx.may_skip_test_teardown = True

    def test_user_cannot_update_another_users_article(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/user_cannot_update_another_users_article.yaml")
        hge_ctx.may_skip_test_teardown = True

    def test_user_cannot_update_id_col(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/user_cannot_update_id_col_article.yaml")
        hge_ctx.may_skip_test_teardown = True

    @classmethod
    def dir(cls):
        return "queries/graphql_mutation/update/permissions"


class TestGraphqlDeleteBasic(DefaultTestQueries):

    def test_article_delete(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/article.yaml")

    def test_article_delete_returning(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/article_returning.yaml")

    @classmethod
    def dir(cls):
        return "queries/graphql_mutation/delete/basic"


class TestGraphqlDeleteConstraints(DefaultTestQueries):

    def test_author_delete_foreign_key_violation(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/author_foreign_key_violation.yaml")

    @classmethod
    def dir(cls):
        return "queries/graphql_mutation/delete/constraints"


class TestGraphqlDeletePermissions(DefaultTestQueries):

    def test_author_can_delete_his_articles(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/author_can_delete_his_articles.yaml")

    def test_author_cannot_delete_other_users_articles(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/author_cannot_delete_other_users_articles.yaml")
        hge_ctx.may_skip_test_teardown = True

    def test_resident_delete_without_select_perm_fail(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/resident_delete_without_select_perm_fail.yaml")
        hge_ctx.may_skip_test_teardown = True

    @classmethod
    def dir(cls):
        return "queries/graphql_mutation/delete/permissions"
