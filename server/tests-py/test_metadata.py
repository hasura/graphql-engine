import os
import pytest

from validate import check_query_f

usefixtures = pytest.mark.usefixtures

use_mutation_fixtures = usefixtures(
    'per_class_db_schema_for_mutation_tests',
    'per_method_db_data_for_mutation_tests'
)


@usefixtures('gql_server', 'per_method_tests_db_state')
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

    def test_replace_metadata_no_tables(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/replace_metadata_no_tables.yaml')

    def test_replace_metadata_wo_remote_schemas(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/replace_metadata_wo_rs.yaml')

    def test_replace_metadata_v2(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/replace_metadata_v2.yaml')

    def test_replace_metadata_allow_inconsistent(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() +
                      '/replace_metadata_allow_inconsistent_inconsistent.yaml')
        check_query_f(hge_ctx, self.dir() +
                      '/replace_metadata_allow_inconsistent.yaml')

    def test_replace_metadata_disallow_inconsistent_metadata(self, hge_ctx):
        resp = hge_ctx.v1metadataq({"type": "export_metadata", "args": {}})
        default_source_config = {}
        default_source = list(filter(lambda source: (source["name"] == "default"), resp["sources"]))
        if default_source:
            default_source_config = default_source[0]["configuration"]
        else:
            assert False, "default source config not found"
        resp = hge_ctx.v1metadataq(
            {
               "type": "replace_metadata",
               "version": 2,
               "args": {
                 "metadata": {
                   "version": 3,
                   "sources": [
                     {
                       "name": "default",
                       "kind": "postgres",
                       "tables": [
                         {
                           "table": {
                             "schema": "public",
                             "name": "author"
                           },
                           "insert_permissions": [
                             {
                               "role": "user1",
                               "permission": {
                                 "check": {},
                                 "columns": [
                                   "id",
                                   "name"
                                 ],
                                 "backend_only": False
                               }
                             },
                             {
                               "role": "user2",
                               "permission": {
                                 "check": {
                                   "id": {
                                     "_eq": "X-Hasura-User-Id"
                                   }
                                 },
                                 "columns": [
                                   "id",
                                   "name"
                                 ],
                                 "backend_only": False
                               }
                             }
                           ]
                         }
                       ],
                       "configuration": default_source_config
                     }
                   ],
                   "inherited_roles": [
                     {
                       "role_name": "users",
                       "role_set": [
                         "user2",
                         "user1"
                       ]
                     }
                   ]
                 }
               }
             },
            expected_status_code = 400
        )
        assert resp == {
            "internal": [
                {
                    "reason": "Could not inherit permission for the role 'users' for the entity: 'insert permission, table: author, source: 'default''",
                    "name": "users",
                    "type": "inherited role permission inconsistency",
                    "entity": {
                        "permission_type": "insert",
                        "source": "default",
                        "table": "author"
                    }
                }
            ],
            "path": "$.args",
            "error": "cannot continue due to inconsistent metadata",
            "code": "unexpected"
        }

    """Test that missing "kind" key in metadata source defaults to "postgres".
    Regression test for https://github.com/hasura/graphql-engine-mono/issues/4501"""
    def test_replace_metadata_default_kind(self, hge_ctx):
        resp = hge_ctx.v1metadataq({"type": "export_metadata", "args": {}})
        default_source_config = {}
        default_source = list(filter(lambda source: (source["name"] == "default"), resp["sources"]))
        if default_source:
            default_source_config = default_source[0]["configuration"]
        else:
            assert False, "default source config not found"
        hge_ctx.v1metadataq({
               "type": "replace_metadata",
               "version": 2,
               "args": {
                 "metadata": {
                   "version": 3,
                   "sources": [
                     {
                       "name": "default",
                       "tables": [],
                       "configuration": default_source_config
                     }
                   ]
                 }
               }
             })
        resp = hge_ctx.v1metadataq({"type": "export_metadata", "args": {}})
        assert resp["sources"][0]["kind"] == "postgres"

    def test_dump_internal_state(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/dump_internal_state.yaml')

    def test_pg_add_source(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/pg_add_source.yaml')

    def test_pg_add_source_with_replace_config(self, hge_ctx):
        hge_ctx.v1metadataq({
              "type": "pg_add_source",
              "args": {
                "name": "pg1",
                "configuration": {
                  "connection_info": {
                    "database_url": {
                      "from_env": "HASURA_GRAPHQL_PG_SOURCE_URL_1"
                    }
                  }
                }
              }
            })
        hge_ctx.v1metadataq({
              "type": "pg_add_source",
              "args": {
                "name": "pg1",
                "configuration": {
                  "connection_info": {
                    "database_url": {
                      "from_env": "HASURA_GRAPHQL_PG_SOURCE_URL_1"
                    }
                  }
                },
                "customization": {
                  "root_fields": {
                    "namespace": "some_namespace"
                  }
                },
                "replace_configuration": True
              }
            })
        resp = hge_ctx.v1metadataq({"type": "export_metadata", "args": {}})
        assert resp["sources"][1]["customization"]["root_fields"]["namespace"] == "some_namespace"
        hge_ctx.v1metadataq({
              "type": "pg_drop_source",
              "args": {
                "name": "pg1"
              }
            })

    def test_pg_update_unknown_source(self, hge_ctx):
        resp = hge_ctx.v1metadataq(
            {
              "type": "pg_update_source",
              "args": {
                "name": "pg-not-previously-added",
                "configuration": {
                  "connection_info": {
                    "database_url": {
                      "from_env": "HASURA_GRAPHQL_PG_SOURCE_URL_1"
                    }
                  }
                }
              }
            },
            expected_status_code = 400
        )
        assert resp["error"] == "source with name \"pg-not-previously-added\" does not exist"

    def test_pg_update_source(self, hge_ctx):
        hge_ctx.v1metadataq({
              "type": "pg_add_source",
              "args": {
                "name": "pg1",
                "configuration": {
                  "connection_info": {
                    "database_url": {
                      "from_env": "HASURA_GRAPHQL_PG_SOURCE_URL_1"
                    },
                    "pool_settings": {
                      "max_connections": 10
                    }
                  }
                }
              }
            })
        hge_ctx.v1metadataq({
              "type": "pg_update_source",
              "args": {
                "name": "pg1",
                "customization": {
                  "root_fields": {
                    "namespace": "some_namespace"
                  }
                }
              }
            })
        resp = hge_ctx.v1metadataq({"type": "export_metadata", "args": {}})
        assert resp["sources"][1]["customization"]["root_fields"]["namespace"] == "some_namespace"
        assert resp["sources"][1]["configuration"]["connection_info"]["pool_settings"]["max_connections"] == 10
        hge_ctx.v1metadataq({
              "type": "pg_update_source",
              "args": {
                "name": "pg1",
                "configuration": {
                  "connection_info": {
                    "database_url": {
                      "from_env": "HASURA_GRAPHQL_PG_SOURCE_URL_1"
                    },
                    "pool_settings": {
                      "max_connections": 50
                    }
                  }
                }
              }
            })
        resp = hge_ctx.v1metadataq({"type": "export_metadata", "args": {}})
        assert resp["sources"][1]["customization"]["root_fields"]["namespace"] == "some_namespace"
        assert resp["sources"][1]["configuration"]["connection_info"]["pool_settings"]["max_connections"] == 50
        hge_ctx.v1metadataq({
              "type": "pg_drop_source",
              "args": {
                "name": "pg1"
              }
            })

    @pytest.mark.skipif(
        os.getenv('HASURA_GRAPHQL_PG_SOURCE_URL_1') != 'postgresql://gql_test:gql_test@localhost:5432/pg_source_1',
        reason="This test relies on hardcoded connection parameters that match Circle's setup.")
    def test_pg_add_source_with_source_parameters(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/pg_add_source_with_parameters.yaml')

    def test_pg_track_table_source(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/pg_track_table_source.yaml')

    def test_rename_source(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/rename_source.yaml')

    def test_pg_multisource_query(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/pg_multisource_query.yaml')

    def test_pg_remote_source_query(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/pg_remote_source_query.yaml')

    @pytest.mark.skipif(
        os.getenv('HASURA_GRAPHQL_PG_SOURCE_URL_1') == os.getenv('HASURA_GRAPHQL_PG_SOURCE_URL_2') or
        os.getenv('HASURA_GRAPHQL_PG_SOURCE_URL_1') is None or
        os.getenv('HASURA_GRAPHQL_PG_SOURCE_URL_2') is None,
        reason="We need two different and valid instances of postgres for this test.")
    def test_pg_remote_source_customized_query(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/pg_remote_source_customized_query.yaml')

    def test_pg_source_namespace_query(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/pg_source_namespace_query.yaml')

    def test_pg_source_prefix_query(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/pg_source_prefix_query.yaml')

    def test_pg_source_customization(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/pg_source_customization.yaml')

    def test_pg_source_cust_custom_name(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/pg_source_customization_custom_name.yaml')

    def test_pg_function_tracking_with_comment(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/pg_track_function_with_comment_setup.yaml')

        # make an introspection query to see if the description of the function has changed
        introspection_query = """{
            __schema {
                queryType {
                fields {
                    name
                    description
                }
                }
            }
        }"""
        url = "/v1/graphql"
        query = {
            "query": introspection_query,
            "variables": {}
        }
        headers = {}
        if hge_ctx.hge_key is not None:
            headers['x-hasura-admin-secret'] = hge_ctx.hge_key

        status_code, resp, _ = hge_ctx.anyq(url, query, headers)
        assert status_code == 200, f'Expected {status_code} to be 200. Response:\n{resp}'

        fn_name = 'search_authors_s1'
        fn_description = 'this function helps fetch articles based on the title'

        resp_fields = resp['data']['__schema']['queryType']['fields']
        if resp_fields is not None:
            comment_found = False
            for field_info in resp_fields:
                if field_info['name'] == fn_name and field_info['description'] == fn_description:
                    comment_found = True
                    break
            assert comment_found == True, resp

        check_query_f(hge_ctx, self.dir() + '/pg_track_function_with_comment_teardown.yaml')

    def test_webhook_transform_success(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/test_webhook_transform_success.yaml')

    def test_webhook_transform_success_remove_body(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/test_webhook_transform_success_remove_body.yaml')

    def test_webhook_transform_success_old_body_schema(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/test_webhook_transform_success_old_body_schema.yaml')

    def test_webhook_transform_success_form_urlencoded(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/test_webhook_transform_success_form_urlencoded.yaml')

    def test_webhook_transform_with_url_env_reference_success(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/test_webhook_transform_env_reference_success.yaml')

    def test_webhook_transform_bad_parse(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/test_webhook_transform_bad_parse.yaml')

    def test_webhook_transform_bad_eval(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/test_webhook_transform_bad_eval.yaml')

    def test_webhook_transform_custom_functions(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/test_webhook_transform_custom_functions.yaml')

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


@pytest.mark.backend('citus', 'mssql', 'postgres', 'bigquery')
@usefixtures('per_class_tests_db_state')
class TestSetTableCustomizationPostgresMSSQLCitusBigquery:

    @classmethod
    def dir(cls):
        return "queries/v1/metadata"

    def test_set_table_customization(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + hge_ctx.backend_suffix('/set_table_customization') + '.yaml')

@pytest.mark.backend('bigquery')
@usefixtures('per_method_tests_db_state')
class TestMetadataBigquery:

    def test_replace_metadata_no_tables(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/replace_metadata_no_tables.yaml')

    @classmethod
    def dir(cls):
        return "queries/v1/metadata/bigquery"
