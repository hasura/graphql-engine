from validate import check_query_f
import pytest

usefixtures = pytest.mark.usefixtures

use_mutation_fixtures = usefixtures(
    'per_class_db_schema_for_mutation_tests',
    'per_method_db_data_for_mutation_tests'
)

class TestDropNoColsTable:
    def test_drop_no_cols_table(self, hge_ctx):
        check_query_f(hge_ctx, 'queries/v1/ddl/drop_no_cols_table.yaml')

@usefixtures('per_method_tests_db_state')
class TestV1General:

    def test_query_string_input_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/query_string_input_err.yaml')

    def test_query_unknown_type_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/query_unknown_type_err.yaml')

    def test_query_args_as_string_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/query_args_as_string_err.yaml')

    def test_query_v2_invalid_version(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/query_v2_invalid_version.yaml')

    @classmethod
    def dir(cls):
        return "queries/v1/basic"

@usefixtures('per_class_tests_db_state')
class TestV1SelectBasic:

    def test_select_query_author(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_article.yaml')

    def test_nested_select_article_author(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/nested_select_query_article_author.yaml')

    def test_nested_select_article_author_alias_for_relationship(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/nested_select_query_article_author_rel_alias.yaml')

    def test_select_author_where(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_query_author_where.yaml')

    def test_select_table_not_present(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_article_table_not_present_err.yaml')

    def test_select_col_not_present(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_article_col_not_present_err.yaml')

    def test_select_nested_where_query(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/nested_select_where_query_author_article.yaml')

    def test_select_query_user(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_user.yaml')

    @classmethod
    def dir(cls):
        return "queries/v1/select/basic"


@usefixtures('per_class_tests_db_state')
class TestV1SelectLimits:

    def test_limit_1(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_query_article_limit_1.yaml')

    def test_limit_2(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_query_article_limit_2.yaml')

    def test_err_str_limit_error(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_query_article_string_limit_error.yaml')

    def test_err_neg_limit_error(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_query_article_neg_limit_error.yaml')

    @classmethod
    def dir(cls):
        return 'queries/v1/select/limits'


@usefixtures('per_class_tests_db_state')
class TestV1SelectOffset:

    def test_offset_1_limit_2(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_query_article_offset_1_limit_2.yaml')

    def test_offset_2_limit_1(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_query_article_offset_2_limit_1.yaml')

    def test_int_as_string_offset_error(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_query_article_int_as_string_offset_error.yaml')

    def test_err_neg_offset_error(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_query_article_neg_offset_error.yaml')

    @classmethod
    def dir(cls):
        return 'queries/v1/select/offset'


@usefixtures('per_class_tests_db_state')
class TestV1SelectBoolExpBasic:

    def test_author_article_where_not_equal(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_author_article_where_neq.yaml')

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

    def test_author_article_where_nin(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_author_article_where_nin.yaml')

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

    @classmethod
    def dir(cls):
        return 'queries/v1/select/boolexp/basic'


@usefixtures('per_class_tests_db_state')
class TestV1SelectBoolExpSearch:

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
        return 'queries/v1/select/boolexp/search'

@usefixtures('per_class_tests_db_state')
class TestV1SelectBoolExpJSONB:

    def test_select_article_author_jsonb_contained_in_bestseller_latest(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_article_author_jsonb_contained_in_bestseller_latest.yaml')

    def test_select_article_author_jsonb_contained_in_latest(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_article_author_jsonb_contained_in_latest.yaml')

    def test_select_article_author_jsonb_contains_latest(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_article_author_jsonb_contains_latest.yaml')

    def test_select_author_article_jsonb_contains_bestseller(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_author_article_jsonb_contains_bestseller.yaml')

    # TODO:- Uncomment the following after adding has_keys_all and has_keys_any operators
    # def test_select_product_jsonb_has_keys_all_ram_touchscreen(self, hge_ctx):
    #     check_query_f(hge_ctx, self.dir() + '/select_product_jsonb_has_keys_all_ram_touchscreen.yaml')

    # def test_select_product_jsonb_has_keys_any_os_operating_system(self, hge_ctx):
    #     check_query_f(hge_ctx, self.dir() + '/select_product_jsonb_has_keys_any_os_operating_system.yaml')

    def test_select_product_jsonb_has_key_sim_type(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_product_jsonb_has_key_sim_type.yaml')

    @classmethod
    def dir(cls):
        return 'queries/v1/select/boolexp/jsonb'

@usefixtures('per_class_tests_db_state')
class TestV1SelectBoolExpPostGIS:

    def test_query_st_equals(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/query_st_equals.yaml')

    def test_query_st_contains(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/query_st_contains.yaml')

    def test_query_st_touches(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/query_st_touches.yaml')

    def test_query_not_st_intersects(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/query_not_st_intersects.yaml')

    def test_query_st_d_within(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/query_st_d_within.yaml')

    def test_query_geog_stdwithin(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/query_geog_stdwithin.yaml')

    def test_query_geog_stdwithin_sph(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/query_geog_stdwithin_sph.yaml')

    def test_query_geog_stintersects(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/query_geog_stintersects.yaml')

    def test_query_cast_geometry_to_geography(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/query_cast_geometry_to_geography.yaml')

    def test_query_cast_geography_to_geometry(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/query_cast_geography_to_geometry.yaml')

    def test_query_illegal_cast_is_not_allowed(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/query_illegal_cast_is_not_allowed.yaml')

    @classmethod
    def dir(cls):
        return 'queries/v1/select/boolexp/postgis'

@usefixtures('per_class_tests_db_state')
class TestV1SelectPermissions:

    def test_user_select_unpublished_articles(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/user_select_query_unpublished_articles.yaml')

    def test_user_only_other_users_published_articles(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/user_can_query_other_users_published_articles.yaml')

    def test_anonymous_only_published_articles(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/anonymous_can_only_get_published_articles.yaml')

    def test_user_cannot_access_remarks_col(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/user_cannot_access_remarks_col.yaml')

    def test_user_can_query_geometry_values_filter(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/user_can_query_geometry_values_filter.yaml')

    def test_user_can_query_geometry_values_filter_session_vars(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/user_can_query_geometry_values_filter_session_vars.yaml')

    def test_user_can_query_geog_filter(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/user_can_query_geog_filter.yaml')

    def test_user_can_query_geog_filter_session_vars(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/user_can_query_geog_filter_session_vars.yaml')

    def test_user_can_query_jsonb_values_filter(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/user_can_query_jsonb_values_filter.yaml')

    def test_user_can_query_jsonb_values_filter_session_vars(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/user_can_query_jsonb_values_filter_session_vars.yaml')

    def test_user_query_auction(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/user_query_auction.yaml')

    @classmethod
    def dir(cls):
        return 'queries/v1/select/permissions'

@use_mutation_fixtures
class TestV1InsertBasic:

    def test_insert_author(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/insert_author.yaml')

    def test_insert_author_col_not_present_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/insert_author_col_not_present_err.yaml')

    def test_insert_null_col_value(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/order_col_shipped_null.yaml")

    def test_insert_nullable_enum_field(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/insert_nullable_enum_field.yaml")

    @classmethod
    def dir(cls):
        return "queries/v1/insert/basic"

@use_mutation_fixtures
class TestV1InsertOnConflict:

    def test_author_on_conflict_update(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/upsert_author.yaml')

    def test_on_conflict_no_action_specified(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/article_on_conflict_no_action_specified.yaml")

    def test_on_conflict_ignore(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/article_on_conflict_ignore_constraint.yaml")
        hge_ctx.may_skip_test_teardown = True

    def test_err_missing_article_constraint(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/article_on_conflict_error_missing_article_constraint.yaml")

    def test_err_unexpected_action(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/article_unexpected_on_conflict_action.yaml")

    def test_err_unexpected_constraint(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/article_unexpected_on_conflict_constraint_error.yaml")

    @classmethod
    def dir(cls):
        return "queries/v1/insert/onconflict"

@use_mutation_fixtures
class TestV1InsertPermissions:

    def test_user_role_on_conflict_update(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/article_on_conflict_user_role.yaml")

    def test_user_role_on_conflict_ignore(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/author_on_conflict_ignore_user_role.yaml")
        hge_ctx.may_skip_test_teardown = True

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

    def test_resident_1_modifies_resident_2_upsert(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/resident_1_modifies_resident_2_upsert.yaml")

    @classmethod
    def dir(cls):
        return "queries/v1/insert/permissions"

@use_mutation_fixtures
class TestV1UpdateBasic:

    def test_set_author_name(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/author_set_name.yaml")

    def test_set_person_details(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/person_set_details.yaml")

    def test_person_id_inc(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/person_inc.yaml")

    def test_product_mul_price(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/product_mul_price.yaml")

    def test_product_set_default_price(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/product_set_default_price.yaml")

    def test_no_operator_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/person_error_no_operator.yaml")

    def test_no_where_clause_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/person_error_no_where_clause.yaml")

    @classmethod
    def dir(cls):
        return "queries/v1/update/basic"

@use_mutation_fixtures
class TestV1UpdatePermissions:

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

    def test_user_update_resident_preset_error(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/user_update_resident_preset_error.yaml")
        hge_ctx.may_skip_test_teardown = True

    def test_user_update_resident_preset(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/user_update_resident_preset.yaml")

    def test_user_update_resident_preset_session_var(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/user_update_resident_preset_session_var.yaml")

    @classmethod
    def dir(cls):
        return "queries/v1/update/permissions"

@usefixtures('per_class_tests_db_state')
class TestV1CountBasic:

    def test_count_authors(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/count_authors.yaml")

    def test_count_published_articles(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/count_published_articles.yaml")

    def test_count_unpublished_articles(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/count_unpublished_articles.yaml")

    def test_count_distinct_authors_with_published_articles(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/count_distinct_authors_with_published_articles.yaml")

    def test_count_articles_registered_authors(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/count_articles_with_registered_authors.yaml")

    def test_count_articles_non_registered_authors(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/count_articles_with_non_registered_authors.yaml")

    def test_count_distinct_col_not_present_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/count_distinct_col_not_present_err.yaml")

    def test_count_distinct_authors_with_unpublished_articles(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/count_distinct_authors_with_unpublished_articles.yaml")

    @classmethod
    def dir(cls):
        return "queries/v1/count/basic"


@usefixtures('per_class_tests_db_state')
class TestV1CountPermissions:

    def test_count_user_has_no_select_permission_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/count_user_has_no_select_perm_error.yaml")

    def test_count_other_users_unpublished_articles(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/count_users_unpublished_articles.yaml")

    @classmethod
    def dir(cls):
        return "queries/v1/count/permissions"

@usefixtures('per_method_tests_db_state')
class TestV1Delete:

    def test_delete_author(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/delete_article.yaml')

    @classmethod
    def dir(cls):
        return "queries/v1/delete"

import ruamel.yaml as yaml
@usefixtures('per_method_tests_db_state')
class TestMetadata:

    def test_reload_metadata(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/reload_metadata.yaml')

    def test_export_metadata(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/export_metadata.yaml')

    def test_clear_metadata(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/clear_metadata.yaml')

    def test_replace_metadata(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/replace_metadata.yaml')

    def test_replace_metadata_wo_remote_schemas(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/replace_metadata_wo_rs.yaml')

    def test_replace_metadata_v2(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/replace_metadata_v2.yaml')

    def test_dump_internal_state(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/dump_internal_state.yaml')

    @classmethod
    def dir(cls):
        return "queries/v1/metadata"

# TODO These look like dependent tests. Ideally we should be able to run tests independently
@usefixtures('per_class_tests_db_state')
class TestMetadataOrder:
    @classmethod
    def dir(cls):
        return "queries/v1/metadata_order"

    def test_export_metadata(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/export_metadata.yaml')

    def test_clear_export_metadata(self, hge_ctx):
        # In the 'clear_export_metadata.yaml' the metadata is added
        # using the metadata APIs
        check_query_f(hge_ctx, self.dir() + '/clear_export_metadata.yaml')

    def test_export_replace(self, hge_ctx):
        url = '/v1/query'
        export_query = {
            'type': 'export_metadata',
            'args': {}
        }
        headers = {}
        if hge_ctx.hge_key is not None:
            headers['X-Hasura-Admin-Secret'] = hge_ctx.hge_key
        # we are exporting the metadata here after creating it though
        # the metadata APIs
        export_code, export_resp, _ = hge_ctx.anyq(url, export_query, headers)
        assert export_code == 200, export_resp
        replace_query = {
            'type': 'replace_metadata',
            'args': export_resp
        }
        # we are replacing the metadata with the exported metadata from the
        # `export_metadata` response.
        replace_code, replace_resp, _ = hge_ctx.anyq(url, replace_query, headers)
        assert replace_code == 200, replace_resp
        # This test catches incorrect key names(if any) in the export_metadata serialization,
        # for example, A new query collection is added to the allow list using the
        # add_collection_to_allowlist metadata API. When
        # the metadata is exported it will contain the allowlist. Now, when this
        # metadata is imported, if the graphql-engine is expecting a different key
        # like allow_list(instead of allowlist) then the allow list won't be imported.
        # Now, exporting the metadata won't contain the allowlist key
        # because it wasn't imported properly and hence the two exports will differ.
        export_code_1, export_resp_1, _ = hge_ctx.anyq(url, export_query, headers)
        assert export_code_1 == 200
        assert export_resp == export_resp_1

@usefixtures('per_method_tests_db_state')
class TestRunSQL:

    def test_select_query(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/sql_select_query.yaml')
        hge_ctx.may_skip_test_teardown = True

    def test_select_query_read_only(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/sql_select_query_read_only.yaml')
        hge_ctx.may_skip_test_teardown = True

    def test_set_timezone(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/sql_set_timezone.yaml')
        hge_ctx.may_skip_test_teardown = True

    def test_sql_timezone_error(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/sql_set_timezone_error.yaml')

    def test_sql_query_as_user_error(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/sql_query_as_user_error.yaml')

    def test_sql_rename_table(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/sql_rename_table.yaml')

    def test_sql_rename_columns_article(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/sql_rename_columns_article.yaml')

    def test_sql_rename_columns_author(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/sql_rename_columns_author.yaml')

    def test_sql_rename_table_and_column(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/sql_rename_table_and_column.yaml')

    def test_sql_alter_test_bool_col(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/sql_alter_test_bool_col.yaml')
        hge_ctx.may_skip_test_teardown = True

    def test_sql_alter_test_id(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/sql_alter_test_id.yaml')
        hge_ctx.may_skip_test_teardown = True

    @classmethod
    def dir(cls):
        return "queries/v1/run_sql"


@usefixtures('per_method_tests_db_state')
class TestRelationships:

    def test_object_relationship_foreign_key(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/object_relationship_foreign_key.yaml')

    def test_create_object_relationship_as_not_admin_error(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/create_object_relationship_as_not_admin_error.yaml')

    def test_object_relationship_col_not_foreign_key_error(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/object_relationship_col_not_foreign_key_error.yaml')

    def test_object_relationship_foreign_key_non_public_schema(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/object_relationship_non_public_schema_foreign_key.yaml')

    def test_object_relationship_manual(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/object_relationship_manual.yaml')

    def test_array_relationship_foreign_key(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/array_relationship_foreign_key.yaml')

    def test_create_array_relationship_as_not_admin_error(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/create_array_relationship_as_not_admin_error.yaml')

    def test_array_relationship_col_not_foreign_key_error(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/array_relationship_col_not_foreign_key_error.yaml')

    def test_array_relationship_foreign_key_non_public_schema(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/array_relationship_non_public_schema_foreign_key.yaml')

    def test_array_relationship_manual(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/array_relationship_manual.yaml')

    @classmethod
    def dir(cls):
        return "queries/v1/relationships"


@usefixtures('per_method_tests_db_state')
class TestTrackTables:

    def test_track_table_function_same_name(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/track_table_function_same_name.yaml')

    def test_track_function_table_same_name(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/track_function_table_same_name.yaml')

    def test_track_untrack_table(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/track_untrack_table.yaml')
        hge_ctx.may_skip_test_teardown = True

    def test_track_untrack_materialized_view(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/track_untrack_materialized_view.yaml')
        hge_ctx.may_skip_test_teardown = True

    def test_track_untrack_table_with_deps(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/track_untrack_table_deps.yaml')
        hge_ctx.may_skip_test_teardown = True

    def test_track_untrack_table_non_public_schema(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/track_untrack_table_non_public_schema.yaml')
        hge_ctx.may_skip_test_teardown = True

    def test_track_untrack_table_as_not_admin_error(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/track_untrack_table_as_not_admin_error.yaml')

    @classmethod
    def dir(cls):
        return "queries/v1/track_table"


@usefixtures('per_method_tests_db_state')
class TestCreatePermission:

    def test_create_permission_admin_role_error(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/create_article_permission_role_admin_error.yaml')

    def test_create_permission_user_role_error(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/create_article_permission_role_user.yaml')

    def test_create_author_insert_permission_long_role(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/create_author_insert_permission_long_role.yaml')

    @classmethod
    def dir(cls):
        return "queries/v1/permissions"

# All these tests fail. So it should be fine to not have a cleanup after tests
class TestNonEmptyText:

    def test_create_event_trigger(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/create_event_trigger.yaml')

    def test_create_insert_permission(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/create_insert_permission.yaml')

    def test_create_query_collection(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/create_query_collection.yaml')

    def test_create_query_collection_queryname(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/create_query_collection_queryname.yaml')

    def test_create_object_relationship(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/create_object_relationship.yaml')

    def test_create_remote_schema(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/create_remote_schema.yaml')

    @classmethod
    def dir(cls):
        return "queries/v1/non_empty_text"

@usefixtures('per_method_tests_db_state')
class TestSetTableIsEnum:
    @classmethod
    def dir(cls):
        return 'queries/v1/set_table_is_enum'

    def test_add_and_remove(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/add_and_remove.yaml')

    def test_add_invalid(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/add_invalid.yaml')

    def test_add_test_schema_enum_table(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/add_test_schema_enum_table.yaml')

    def test_relationship_with_inconsistent_enum_table(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/relationship_with_inconsistent_enum_table.yaml')

@usefixtures('per_method_tests_db_state')
class TestSetTableCustomFields:

    @classmethod
    def dir(cls):
        return 'queries/v1/set_table_custom_fields'

    def test_set_and_unset(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/set_and_unset.yaml')

    def test_set_invalid_table(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/set_invalid_table.yaml')

    def test_alter_column(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/alter_column.yaml')

    def test_conflict_with_relationship(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/conflict_with_relationship.yaml')

    def test_column_field_swap(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/column_field_swap.yaml")

    def test_relationship_conflict_with_custom_column(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/relationship_conflict_with_custom_column.yaml")

@usefixtures('per_method_tests_db_state')
class TestComputedFields:
    @classmethod
    def dir(cls):
        return 'queries/v1/computed_fields'

    def test_add_computed_fields_errors(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/add_computed_field_errors.yaml')

    def test_add_and_drop(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/add_and_drop.yaml')

    def test_create_permissions(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/create_permissions.yaml')

    def test_run_sql(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/run_sql.yaml')

@usefixtures('per_method_tests_db_state')
class TestBulkQuery:
    @classmethod
    def dir(cls):
        return 'queries/v1/bulk'

    def test_run_bulk(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/basic.yaml')

    def test_run_bulk_mixed_access_mode(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/mixed_access_mode.yaml')

    def test_run_bulk_with_select_and_writes(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_with_writes.yaml')

    def test_run_bulk_with_select_and_reads(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_with_reads.yaml')
