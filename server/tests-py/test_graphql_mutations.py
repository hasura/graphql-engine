import pytest
from validate import check_query_f, check_query, get_conf_f


# Marking all tests in this module that server upgrade tests can be run
# Few of them cannot be run, which will be marked skip_server_upgrade_test
pytestmark = pytest.mark.allow_server_upgrade_test

usefixtures = pytest.mark.usefixtures

use_mutation_fixtures = usefixtures(
    'per_class_db_schema_for_mutation_tests',
    'per_method_db_data_for_mutation_tests'
)

@pytest.mark.parametrize("transport", ['http', 'websocket'])
@use_mutation_fixtures
class TestGraphQLInsert:

    def test_inserts_author_article(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/author_article.yaml", transport)

    def test_inserts_various_postgres_types(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/insert_various_postgres_types.yaml")

    @pytest.mark.xfail(reason="Refer https://github.com/hasura/graphql-engine/issues/348")
    def test_insert_into_array_col_with_array_input(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/insert_into_array_col_with_array_input.yaml")

    def test_insert_using_variable(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/person_jsonb_variable.yaml")

    def test_insert_using_array_variable(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/person_jsonb_variable_array.yaml")

    def test_insert_person(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/person_jsonb.yaml")

    def test_insert_person_array(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/person_jsonb_array.yaml")

    def test_insert_null_col_value(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/order_col_shipped_null.yaml")

    @classmethod
    def dir(cls):
        return "queries/graphql_mutation/insert/basic"


@pytest.mark.parametrize("transport", ['http', 'websocket'])
@use_mutation_fixtures
class TestGraphqlInsertOnConflict:

    def test_on_conflict_update(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/article_on_conflict_update.yaml")

    def test_on_conflict_ignore(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/article_on_conflict_ignore_constraint.yaml")

    def test_on_conflict_update_empty_cols(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/article_on_conflict_empty_update_columns.yaml")

    def test_err_missing_article_constraint(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/article_on_conflict_error_missing_article_constraint.yaml")

    def test_err_unexpected_action(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/article_unexpected_on_conflict_action.yaml")

    def test_err_unexpected_constraint(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/article_unexpected_on_conflict_constraint_error.yaml")

    def test_order_on_conflict_where(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/order_on_conflict_where.yaml')

    @classmethod
    def dir(cls):
        return "queries/graphql_mutation/insert/onconflict"


@pytest.mark.parametrize("transport", ['http', 'websocket'])
@use_mutation_fixtures
class TestGraphqlInsertPermission:

    def test_user_role_on_conflict_update(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/article_on_conflict_user_role.yaml")

    def test_user_role_on_conflict_constraint_on_error(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/article_on_conflict_constraint_on_user_role_error.yaml")

    def test_user_role_on_conflict_ignore(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/author_on_conflict_ignore_user_role.yaml")

    def test_user_err_missing_article_constraint(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/user_article_on_conflict_error_missing_article_constraint.yaml")

    def test_user_err_unexpected_action(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/user_article_error_unexpected_on_conflict_action.yaml")

    def test_user_err_unexpected_constraint(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/user_article_unexpected_on_conflict_constraint_error.yaml")

    def test_role_has_no_permissions_err(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/address_permission_error.yaml")

    # This test captures a bug in the previous release
    # Avoiding this test for server upgrades
    @pytest.mark.skip_server_upgrade_test
    def test_author_user_role_insert_check_perm_success(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/author_user_role_insert_check_perm_success.yaml")

    def test_user_role_insert_check_is_registered_fail(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/author_user_role_insert_check_is_registered_fail.yaml")

    def test_user_role_insert_check_user_id_fail(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/author_user_role_insert_check_user_id_fail.yaml")

    def test_student_role_insert_check_bio_success(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/author_student_role_insert_check_bio_success.yaml")

    def test_student_role_insert_check_bio_fail(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/author_student_role_insert_check_bio_fail.yaml")

    def test_company_user_role_insert(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/company_user_role.yaml")

    def test_company_user_role_insert_on_conflict(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/company_user_role_on_conflict.yaml")

    def test_resident_user_role_insert(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/resident_user.yaml")

    def test_resident_infant_role_insert(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/resident_infant.yaml")

    def test_resident_infant_role_insert_fail(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/resident_infant_fail.yaml")

    def test_resident_5_modifies_resident_6_upsert(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/resident_5_modifies_resident_6_upsert.yaml")

    def test_resident_on_conflict_where(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/resident_on_conflict_where.yaml")

    def test_blog_on_conflict_update_preset(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/blog_on_conflict_update_preset.yaml")

    def test_arr_sess_var_insert_article_as_editor_allowed_user_id(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/insert_article_arr_sess_var_editor_allowed_user_id.yaml")

    def test_arr_sess_var_insert_article_as_editor_err_not_allowed_user_id(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/insert_article_arr_sess_var_editors_err_not_allowed_user_id.yaml")

    def test_seller_insert_computer_json_has_keys_all(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/seller_insert_computer_has_keys_all_pass.yaml")

    def test_seller_insert_computer_json_has_keys_all_err(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/seller_insert_computer_has_keys_all_fail.yaml")

    def test_developer_insert_computer_json_has_keys_any(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/developer_insert_has_keys_any_pass.yaml")

    def test_developer_insert_computer_json_has_keys_any_err(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/developer_insert_has_keys_any_fail.yaml")

    def test_user_insert_account_success(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/user_insert_account_success.yaml")

    def test_user_insert_account_fail(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/user_insert_account_fail.yaml")

    def test_backend_user_insert_fail(self, hge_ctx, transport):
        check_query_admin_secret(hge_ctx, self.dir() + "/backend_user_insert_fail.yaml", transport)

    def test_backend_user_insert_pass(self, hge_ctx, transport):
        check_query_admin_secret(hge_ctx, self.dir() + "/backend_user_insert_pass.yaml", transport)

    def test_backend_user_insert_invalid_bool(self, hge_ctx, transport):
        check_query_admin_secret(hge_ctx, self.dir() + "/backend_user_insert_invalid_bool.yaml")

    def test_user_with_no_backend_privilege(self, hge_ctx, transport):
        check_query_admin_secret(hge_ctx, self.dir() + "/user_with_no_backend_privilege.yaml")

    def test_backend_user_no_admin_secret_fail(self, hge_ctx, transport):
        if hge_ctx.hge_key and (hge_ctx.hge_jwt_key or hge_ctx.hge_webhook):
            check_query_f(hge_ctx, self.dir() + "/backend_user_no_admin_secret_fail.yaml", transport)
        else:
            pytest.skip("authorization not configured, skipping the test")

    @classmethod
    def dir(cls):
        return "queries/graphql_mutation/insert/permissions"

def check_query_admin_secret(hge_ctx, f, transport='http'):
    conf = get_conf_f(f)
    admin_secret = hge_ctx.hge_key
    if admin_secret:
        conf['headers']['x-hasura-admin-secret'] = admin_secret
    check_query(hge_ctx, conf, transport, False)


@pytest.mark.parametrize("transport", ['http', 'websocket'])
@usefixtures('per_class_tests_db_state')
class TestGraphqlInsertConstraints:

    def test_address_not_null_constraint_err(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/address_not_null_constraint_error.yaml")

    def test_insert_unique_constraint_err(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/author_unique_constraint_error.yaml")

    @classmethod
    def dir(cls):
        return "queries/graphql_mutation/insert/constraints"


@pytest.mark.parametrize("transport", ['http', 'websocket'])
@usefixtures('per_class_tests_db_state')
class TestGraphqlInsertNullPrefixedColumnOnConflict:

    def test_address_not_null_constraint_err(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/null_prefixed_column_ok.yaml")

    @classmethod
    def dir(cls):
        return "queries/graphql_mutation/insert/nullprefixcolumn"


@pytest.mark.parametrize("transport", ['http', 'websocket'])
@use_mutation_fixtures
class TestGraphqlInsertGeoJson:

    def test_insert_point_landmark(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/insert_landmark.yaml")

    def test_insert_3d_point_drone_loc(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/insert_drone_3d_location.yaml")

    def test_insert_landmark_single_position_err(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/insert_landmark_single_position_err.yaml")

    def test_insert_line_string_road(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/insert_road.yaml")

    def test_insert_road_single_point_err(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/insert_road_single_point_err.yaml")

    def test_insert_multi_point_service_locations(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/insert_service_locations.yaml")
    def test_insert_multi_line_string_route(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/insert_route.yaml")

    def test_insert_polygon(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/insert_area.yaml")

    def test_insert_linear_ring_less_than_4_points_err(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/insert_area_less_than_4_points_err.yaml")

    def test_insert_linear_ring_last_point_not_equal_to_first_err(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/insert_linear_ring_last_point_not_equal_to_first_err.yaml")

    def test_insert_multi_polygon_compounds(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/insert_compounds.yaml")

    def test_insert_geometry_collection(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/insert_geometry_collection.yaml")

    def test_insert_unexpected_geometry_type_err(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/insert_geometry_unexpected_type_err.yaml")

    @classmethod
    def dir(cls):
        return "queries/graphql_mutation/insert/geojson"


@pytest.mark.parametrize("transport", ['http', 'websocket'])
@use_mutation_fixtures
# Skipping server upgrade tests for a few tests below
# Those tests capture bugs in the previous release
class TestGraphqlNestedInserts:

    @pytest.mark.skip_server_upgrade_test
    def test_author_with_articles(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/author_with_articles.yaml")

    @pytest.mark.skip_server_upgrade_test
    def test_author_with_articles_empty(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/author_with_articles_empty.yaml")

    def test_author_with_articles_null(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/author_with_articles_null.yaml")

    def test_author_with_articles_author_id_fail(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/author_with_articles_author_id_fail.yaml")

    @pytest.mark.skip_server_upgrade_test
    def test_articles_with_author(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/articles_with_author.yaml")

    def test_articles_with_author_author_id_fail(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/articles_with_author_author_id_fail.yaml")

    def test_author_upsert_articles_fail(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/author_upsert_articles_fail.yaml")

    def test_articles_author_upsert_fail(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/articles_author_upsert_fail.yaml")

    @pytest.mark.skip_server_upgrade_test
    def test_articles_with_author_returning(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/articles_with_author_returning.yaml")

    def test_author_one(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/author_one.yaml")

    def test_author_with_articles_one(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/author_with_articles_one.yaml")

    def test_author_upsert_one_update(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/author_upsert_one_update.yaml")

    def test_author_upsert_one_no_update(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/author_upsert_one_no_update.yaml")

    @classmethod
    def dir(cls):
        return "queries/graphql_mutation/insert/nested"


@pytest.mark.parametrize("transport", ['http', 'websocket'])
@use_mutation_fixtures
class TestGraphqlInsertViews:

    def test_insert_view_author_simple(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/insert_view_author_simple.yaml")

    def test_insert_view_author_complex_fail(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/insert_view_author_complex_fail.yaml")

    # This test captures a bug in the previous release
    # Avoiding this test for server upgrades
    @pytest.mark.skip_server_upgrade_test
    def test_nested_insert_article_author_simple_view(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/nested_insert_article_author_simple_view.yaml")

    def test_nested_insert_article_author_complex_view_fail(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/nested_insert_article_author_complex_view_fail.yaml")

    @classmethod
    def dir(cls):
        return "queries/graphql_mutation/insert/views"


@pytest.mark.parametrize("transport", ['http', 'websocket'])
@use_mutation_fixtures
class TestGraphqlUpdateBasic:

    # This test captures a bug in the previous release
    # Avoiding this test for server upgrades
    @pytest.mark.skip_server_upgrade_test
    def test_set_author_name(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/author_set_name.yaml")

    def test_empty_set_author(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/author_empty_set.yaml")
        hge_ctx.may_skip_test_teardown = True

    def test_set_person_details(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/person_set_details.yaml")

    def test_person_id_inc(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/person_inc.yaml")

    def test_no_operator_err(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/person_error_no_operator.yaml")

    def test_column_in_multiple_operators(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/article_column_multiple_operators.yaml")

    def test_column_in_multiple_operators(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/article_column_multiple_operators.yaml")

    def test_author_by_pk(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/author_by_pk.yaml")

    def test_author_by_pk_null(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/author_by_pk_null.yaml")

    @classmethod
    def dir(cls):
        return "queries/graphql_mutation/update/basic"


@pytest.mark.parametrize("transport", ['http', 'websocket'])
@use_mutation_fixtures
class TestGraphqlUpdateJsonB:

    def test_jsonb_append_object(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/person_append_object.yaml")

    def test_jsonb_append_array(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/person_append_array.yaml")

    def test_jsonb_prepend_array(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/person_prepend_array.yaml")

    def test_jsonb_delete_at_path(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/person_delete_at_path.yaml")

    def test_jsonb_delete_array_element(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/person_delete_array_element.yaml")

    def test_jsonb_delete_key(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/person_delete_key.yaml")

    @classmethod
    def dir(cls):
        return "queries/graphql_mutation/update/jsonb"


@pytest.mark.parametrize("transport", ['http', 'websocket'])
@use_mutation_fixtures
class TestGraphqlUpdatePermissions:

    # This test captures a bug in the previous release
    # Avoiding this test for server upgrades
    @pytest.mark.skip_server_upgrade_test
    def test_user_update_author(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/user_update_author.yaml")

    def test_user_can_update_unpublished_article(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/user_can_update_unpublished_article.yaml")

    def test_user_cannot_update_published_version_col(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/user_cannot_update_published_article_version.yaml")

    def test_user_cannot_update_another_users_article(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/user_cannot_update_another_users_article.yaml")

    def test_user_cannot_publish(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/user_cannot_publish.yaml")

    def test_user_cannot_update_id_col(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/user_cannot_update_id_col_article.yaml")

    def test_user_update_resident_preset(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/user_update_resident_preset.yaml', transport)

    def test_user_update_resident_preset_session_var(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/user_update_resident_preset_session_var.yaml', transport)

    def test_user_account_update_success(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/user_account_update_success.yaml')

    def test_user_account_update_no_rows(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/user_account_update_no_rows.yaml')
    @classmethod
    def dir(cls):
        return "queries/graphql_mutation/update/permissions"


@pytest.mark.parametrize("transport", ['http', 'websocket'])
@use_mutation_fixtures
class TestGraphqlDeleteBasic:

    def test_article_delete(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/article.yaml", transport)

    def test_article_delete_returning(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/article_returning.yaml", transport)

    def test_article_delete_returning_author(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/article_returning_author.yaml", transport)

    def test_author_returning_empty_articles(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/author_returning_empty_articles.yaml", transport)

    def test_article_by_pk(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/article_by_pk.yaml", transport)

    def test_article_by_pk_null(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/article_by_pk_null.yaml", transport)

    @classmethod
    def dir(cls):
        return "queries/graphql_mutation/delete/basic"

@pytest.mark.parametrize("transport", ['http', 'websocket'])
@use_mutation_fixtures
class TestGraphqlDeleteConstraints:

    def test_author_delete_foreign_key_violation(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/author_foreign_key_violation.yaml")

    @classmethod
    def dir(cls):
        return "queries/graphql_mutation/delete/constraints"


@pytest.mark.parametrize("transport", ['http', 'websocket'])
@use_mutation_fixtures
class TestGraphqlDeletePermissions:

    def test_author_can_delete_his_articles(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/author_can_delete_his_articles.yaml")

    def test_author_cannot_delete_other_users_articles(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/author_cannot_delete_other_users_articles.yaml")

    def test_resident_delete_without_select_perm_fail(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/resident_delete_without_select_perm_fail.yaml")

    def test_agent_delete_perm_arr_sess_var(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/agent_delete_perm_arr_sess_var.yaml")

    def test_agent_delete_perm_arr_sess_var_fail(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/agent_delete_perm_arr_sess_var_fail.yaml")

    def test_user_delete_account_success(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/user_delete_account_success.yaml")

    def test_user_delete_account_no_rows(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/user_delete_account_no_rows.yaml")
    @classmethod
    def dir(cls):
        return "queries/graphql_mutation/delete/permissions"

@pytest.mark.parametrize("transport", ['http', 'websocket'])
@use_mutation_fixtures
class TestGraphqlMutationCustomSchema:

    def test_insert_author(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/insert_author.yaml', transport)

    # This test captures a bug in the previous release
    # Avoiding this test for server upgrades
    @pytest.mark.skip_server_upgrade_test
    def test_insert_article_author(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/insert_article_author.yaml', transport)

    def test_update_article(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/update_article.yaml', transport)

    def test_delete_article(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/delete_article.yaml', transport)

    @classmethod
    def dir(cls):
        return "queries/graphql_mutation/custom_schema"

@pytest.mark.parametrize('transport', ['http', 'websocket'])
@use_mutation_fixtures
class TestGraphQLMutateEnums:
    @classmethod
    def dir(cls):
        return 'queries/graphql_mutation/enums'

    def test_insert_enum_field(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/insert_enum_field.yaml', transport)

    def test_insert_nullable_enum_field(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/insert_nullable_enum_field.yaml', transport)

    def test_insert_enum_field_bad_value(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/insert_enum_field_bad_value.yaml', transport)

    def test_update_enum_field(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/update_enum_field.yaml', transport)

    def test_update_where_enum_field(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/update_where_enum_field.yaml', transport)

    def test_delete_where_enum_field(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/delete_where_enum_field.yaml', transport)
