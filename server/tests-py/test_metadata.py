import ruamel.yaml as yaml
from validate import check_query_f
import pytest
import os

usefixtures = pytest.mark.usefixtures

use_mutation_fixtures = usefixtures(
    'per_class_db_schema_for_mutation_tests',
    'per_method_db_data_for_mutation_tests'
)


@usefixtures('per_method_tests_db_state')
class TestMetadata:

    def test_reload_metadata(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/reload_metadata.yaml')

    # FIXME:- Using export_metadata will dump
    # the source configuration dependent on --database-url
    # def test_export_metadata(self, hge_ctx):
    #     check_query_f(hge_ctx, self.dir() + '/export_metadata.yaml')

    def test_clear_metadata(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/clear_metadata.yaml')

    def test_clear_metadata_as_user(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/metadata_as_user_err.yaml')

    def test_replace_metadata(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/replace_metadata.yaml')

    def test_replace_metadata_wo_remote_schemas(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/replace_metadata_wo_rs.yaml')

    def test_replace_metadata_v2(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/replace_metadata_v2.yaml')

    def test_replace_metadata_allow_inconsistent(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() +
                      '/replace_metadata_allow_inconsistent_inconsistent.yaml')
        check_query_f(hge_ctx, self.dir() +
                      '/replace_metadata_allow_inconsistent.yaml')

    def test_dump_internal_state(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/dump_internal_state.yaml')

    def test_pg_add_source(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/pg_add_source.yaml')

    def test_pg_track_table_source(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/pg_track_table_source.yaml')

    def test_rename_source(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/rename_source.yaml')

    def test_pg_multisource_query(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/pg_multisource_query.yaml')

    @pytest.mark.skipif(
        os.getenv('HASURA_GRAPHQL_PG_SOURCE_URL_1') == os.getenv('HASURA_GRAPHQL_PG_SOURCE_URL_2') or
        os.getenv('HASURA_GRAPHQL_PG_SOURCE_URL_1') is None or
        os.getenv('HASURA_GRAPHQL_PG_SOURCE_URL_2') is None,
        reason="We need two different and valid instances of postgres for this test.")
    def test_pg_multisource_table_name_conflict(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/pg_multisource_table_name_conflict.yaml')

    @classmethod
    def dir(cls):
        return "queries/v1/metadata"

# TODO These look like dependent tests. Ideally we should be able to run tests independently


@usefixtures('per_class_tests_db_state')
class TestMetadataOrder:
    @classmethod
    def dir(cls):
        return "queries/v1/metadata_order"

    # FIXME:- Using export_metadata will dump
    # the source configuration dependent on --database-url
    # def test_export_metadata(self, hge_ctx):
    #     check_query_f(hge_ctx, self.dir() + '/export_metadata.yaml')

    # def test_clear_export_metadata(self, hge_ctx):
        # In the 'clear_export_metadata.yaml' the metadata is added
        # using the metadata APIs
        # check_query_f(hge_ctx, self.dir() + '/clear_export_metadata.yaml')

    def test_export_replace(self, hge_ctx):
        url = '/v1/query'
        export_query = {
            'type': 'export_metadata',
            'args': {}
        }
        headers = {}
        if hge_ctx.hge_key is not None:
            headers['X-Hasura-Admin-Secret'] = hge_ctx.hge_key
        # we are exporting the metadata here after creating it through
        # the metadata APIs
        export_code, export_resp, _ = hge_ctx.anyq(url, export_query, headers)
        assert export_code == 200, export_resp
        replace_query = {
            'type': 'replace_metadata',
            'args': export_resp
        }
        # we are replacing the metadata with the exported metadata from the
        # `export_metadata` response.
        replace_code, replace_resp, _ = hge_ctx.anyq(
            url, replace_query, headers)
        assert replace_code == 200, replace_resp
        # This test catches incorrect key names(if any) in the export_metadata serialization,
        # for example, A new query collection is added to the allow list using the
        # add_collection_to_allowlist metadata API. When
        # the metadata is exported it will contain the allowlist. Now, when this
        # metadata is imported, if the graphql-engine is expecting a different key
        # like allow_list(instead of allowlist) then the allow list won't be imported.
        # Now, exporting the metadata won't contain the allowlist key
        # because it wasn't imported properly and hence the two exports will differ.
        export_code_1, export_resp_1, _ = hge_ctx.anyq(
            url, export_query, headers)
        assert export_code_1 == 200
        assert export_resp == export_resp_1

    def test_export_replace_v2(self, hge_ctx):
        url = '/v1/metadata'
        export_query = {
            'type': 'export_metadata',
            'version': 2,
            'args': {}
        }
        headers = {}
        if hge_ctx.hge_key is not None:
            headers['X-Hasura-Admin-Secret'] = hge_ctx.hge_key
        # we are exporting the metadata here after creating it through
        # the metadata APIs
        export_code, export_resp, _ = hge_ctx.anyq(url, export_query, headers)
        assert export_code == 200, export_resp

        replace_query = {
            'type': 'replace_metadata',
            'version': 2,
            'resource_version': export_resp['resource_version'],
            'args': {'metadata': export_resp['metadata']}
        }
        # we are replacing the metadata with the exported metadata from the
        # `export_metadata` response.
        replace_code, replace_resp, _ = hge_ctx.anyq(
            url, replace_query, headers)
        assert replace_code == 200, replace_resp

        export_code_1, export_resp_1, _ = hge_ctx.anyq(
            url, export_query, headers)
        assert export_code_1 == 200
        assert export_resp['metadata'] == export_resp_1['metadata']

        # `resource_version` should have been incremented
        assert export_resp['resource_version'] + \
            1 == export_resp_1['resource_version']

    def test_export_replace_v2_conflict(self, hge_ctx):
        url = '/v1/metadata'
        export_query = {
            'type': 'export_metadata',
            'version': 2,
            'args': {}
        }
        headers = {}
        if hge_ctx.hge_key is not None:
            headers['X-Hasura-Admin-Secret'] = hge_ctx.hge_key
        # we are exporting the metadata here after creating it through
        # the metadata APIs
        export_code, export_resp, _ = hge_ctx.anyq(url, export_query, headers)
        assert export_code == 200, export_resp

        replace_query = {
            'type': 'replace_metadata',
            'version': 2,
            'resource_version': export_resp['resource_version'] - 1,
            'args': {'metadata': export_resp['metadata']}
        }
        # we are replacing the metadata with the exported metadata from the
        # `export_metadata` response.
        # Using the wrong `resource_version` should result in a 409 conflict
        replace_code, replace_resp, _ = hge_ctx.anyq(
            url, replace_query, headers)
        assert replace_code == 409, replace_resp

        export_code_1, export_resp_1, _ = hge_ctx.anyq(
            url, export_query, headers)
        assert export_code_1 == 200
        assert export_resp['metadata'] == export_resp_1['metadata']

        # `resource_version` should be unchanged
        assert export_resp['resource_version'] == export_resp_1['resource_version']

    def test_reload_metadata(self, hge_ctx):
        url = '/v1/metadata'
        export_query = {
            'type': 'export_metadata',
            'version': 2,
            'args': {}
        }
        headers = {}
        if hge_ctx.hge_key is not None:
            headers['X-Hasura-Admin-Secret'] = hge_ctx.hge_key
        # we are exporting the metadata here after creating it through
        # the metadata APIs
        export_code, export_resp, _ = hge_ctx.anyq(url, export_query, headers)
        assert export_code == 200, export_resp

        reload_query = {
            'type': 'reload_metadata',
            'resource_version': export_resp['resource_version'],
            'args': {}
        }
        # we are replacing the metadata with the exported metadata from the
        # `export_metadata` response.
        reload_code, reload_resp, _ = hge_ctx.anyq(
            url, reload_query, headers)
        assert reload_code == 200, reload_resp

        export_code_1, export_resp_1, _ = hge_ctx.anyq(
            url, export_query, headers)
        assert export_code_1 == 200
        assert export_resp['metadata'] == export_resp_1['metadata']

        # `resource_version` should have been incremented
        assert export_resp['resource_version'] + \
            1 == export_resp_1['resource_version']

    def test_reload_metadata_conflict(self, hge_ctx):
        url = '/v1/metadata'
        export_query = {
            'type': 'export_metadata',
            'version': 2,
            'args': {}
        }
        headers = {}
        if hge_ctx.hge_key is not None:
            headers['X-Hasura-Admin-Secret'] = hge_ctx.hge_key
        # we are exporting the metadata here after creating it through
        # the metadata APIs
        export_code, export_resp, _ = hge_ctx.anyq(url, export_query, headers)
        assert export_code == 200, export_resp

        reload_query = {
            'type': 'reload_metadata',
            'resource_version': export_resp['resource_version'] - 1,
            'args': {}
        }
        # we are replacing the metadata with the exported metadata from the
        # `export_metadata` response.
        reload_code, reload_resp, _ = hge_ctx.anyq(
            url, reload_query, headers)
        assert reload_code == 409, reload_resp

        export_code_1, export_resp_1, _ = hge_ctx.anyq(
            url, export_query, headers)
        assert export_code_1 == 200
        assert export_resp['metadata'] == export_resp_1['metadata']

        # `resource_version` should be unchanged
        assert export_resp['resource_version'] == export_resp_1['resource_version']


@pytest.mark.parametrize("backend", ['citus', 'mssql', 'postgres'])
@usefixtures('per_class_tests_db_state')
class TestSetTableCustomization:

    @classmethod
    def dir(cls):
        return "queries/v1/metadata"

    def test_set_table_customization(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + hge_ctx.backend_suffix('/set_table_customization') + '.yaml')
