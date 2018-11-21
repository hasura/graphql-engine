#!/usr/bin/env python3

import pytest
import yaml

from validate import check_query_f, check_query

def mk_add_remote_q(name, url):
    return {
        "type": "add_remote_schema",
        "args": {
            "name": name,
            "comment": "testing " + name,
            "definition": {
                "url": url,
                "forward_client_headers": False
            }
        }
    }


add_country_gql_remote = {
    "type": "add_remote_schema",
    "args": {
        "name": "test-remote",
        "comment": "test remote",
        "definition": {
            "url": "https://countries.trevorblades.com/",
            "forward_client_headers": False
        }
    }
}


class TestRemoteSchemaBasic:
    """ basic => no hasura tables are tracked """

    teardown = {"type": "clear_metadata", "args": {}}
    dir = 'queries/remote_schemas'

    @pytest.fixture(autouse=True)
    def transact(self, hge_ctx):
        st_code, resp = hge_ctx.v1q(add_country_gql_remote)
        assert st_code == 200, resp
        yield
        hge_ctx.v1q(self.teardown)

    def test_add_schema(self, hge_ctx):
        """ check if the remote schema is added in the db """
        conn = hge_ctx.engine.connect()
        res = conn.execute('select * from hdb_catalog.remote_schemas')
        row = res.fetchone()
        assert row['name'] == "test-remote"
        conn.close()

    def test_introspection(self, hge_ctx):
        check_query_f(hge_ctx, 'queries/graphql_introspection/introspection.yaml')

    def test_introspection_as_user(self, hge_ctx):
        check_query_f(hge_ctx, 'queries/graphql_introspection/introspection_user_role.yaml')

    def test_remote_query(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/basic_query.yaml')

    def test_remote_subscription(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/basic_subscription_not_supported.yaml')

    def test_add_schema_conflicts(self, hge_ctx):
        """add 2 remote schemas with same node or types"""
        q = mk_add_remote_q('new-test-remote', 'https://countries.trevorblades.com/')
        st_code, resp = hge_ctx.v1q(q)
        assert st_code == 400
        assert resp['code'] == 'remote-schema-conflicts'


class TestAddRemoteSchemaTbls:
    """ tests with adding a table in hasura """

    dir = 'queries/remote_schemas'

    @pytest.fixture(autouse=True)
    def transact(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f('queries/remote_schemas/tbls_setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f('queries/remote_schemas/tbls_teardown.yaml')
        assert st_code == 200, resp

    def test_add_schema(self, hge_ctx):
        """ check if the remote schema is added in the db """
        conn = hge_ctx.engine.connect()
        res = conn.execute('select * from hdb_catalog.remote_schemas')
        row = res.fetchone()
        assert row['name'] == "bahnql-remote"
        conn.close()

    def test_add_schema_conflicts_with_tables(self, hge_ctx):
        """add remote schema which conflicts with hasura tables"""
        st_code, resp = hge_ctx.v1q(add_country_gql_remote)
        assert st_code == 400
        assert resp['code'] == 'remote-schema-conflicts'

    def test_remote_query(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/bahnql_query.yaml')

    def test_add_conflicting_table(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir + '/create_conflicting_table.yaml')
        assert st_code == 400
        assert resp['code'] == 'remote-schema-conflicts'

    def test_introspection(self, hge_ctx):
        has_route_ty = False
        has_country_ty = False
        has_routing = False
        has_country = False
        with open('queries/graphql_introspection/introspection.yaml') as f:
            query = yaml.load(f)
        st_code, resp = check_query(hge_ctx, query)
        assert st_code == 200, resp
        for t in resp['data']['__schema']['types']:
            print(t['name'])
            if t['name'] == 'Route':
                has_route_ty = True
            if t['name'] == 'country':
                has_country_ty = True
            if t['name'] == 'query_root':
                for fld in t['fields']:
                    if fld['name'] == 'country':
                        has_country = True
                    if fld['name'] == 'routing':
                        has_routing = True

        assert has_route_ty
        assert has_country_ty
        assert has_routing
        assert has_country

#    def test_add_schema_url_from_env(self, hge_ctx):
#        pass
#    def test_add_schema_header_from_env(self, hge_ctx):
#        pass
#    def test_add_schema_duplicate_name(self, hge_ctx):
#        pass
#
#class TestRemoveRemoteSchema:
#    def test_remove_schema_basic(self, hge_ctx):
#        pass
#    def test_remove_schema_not_exists(self, hge_ctx):
#        pass
#
#class TestResolveRemoteSchemaQuery:
#    pass
