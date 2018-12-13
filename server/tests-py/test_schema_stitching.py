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


class TestRemoteSchemaBasic:
    """ basic => no hasura tables are tracked """

    teardown = {"type": "clear_metadata", "args": {}}
    dir = 'queries/remote_schemas'

    @pytest.fixture(autouse=True)
    def transact(self, hge_ctx):
        q = mk_add_remote_q('simple 1', 'http://localhost:5000/hello-graphql')
        st_code, resp = hge_ctx.v1q(q)
        assert st_code == 200, resp
        yield
        hge_ctx.v1q(self.teardown)

    def test_add_schema(self, hge_ctx):
        """ check if the remote schema is added in the db """
        conn = hge_ctx.engine.connect()
        res = conn.execute('select * from hdb_catalog.remote_schemas')
        row = res.fetchone()
        assert row['name'] == "simple 1"
        conn.close()

    def test_introspection(self, hge_ctx):
        #check_query_f(hge_ctx, 'queries/graphql_introspection/introspection.yaml')
        with open('queries/graphql_introspection/introspection.yaml') as f:
            query = yaml.load(f)
        st_code, resp = check_query(hge_ctx, query)
        assert st_code == 200, resp
        assert check_introspection_result(resp, ['Hello'], ['hello'])
#

    def test_introspection_as_user(self, hge_ctx):
        check_query_f(hge_ctx, 'queries/graphql_introspection/introspection_user_role.yaml')

    def test_remote_query(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/basic_query.yaml')

    def test_remote_subscription(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/basic_subscription_not_supported.yaml')

    def test_add_schema_conflicts(self, hge_ctx):
        """add 2 remote schemas with same node or types"""
        q = mk_add_remote_q('simple 2', 'http://localhost:5000/hello-graphql')
        st_code, resp = hge_ctx.v1q(q)
        assert st_code == 400
        assert resp['code'] == 'remote-schema-conflicts'

    def test_add_second_remote_schema(self, hge_ctx):
        """add 2 remote schemas with different node and types"""
        q = mk_add_remote_q('my remote', 'http://localhost:5000/user-graphql')
        st_code, resp = hge_ctx.v1q(q)
        assert st_code == 200, resp
        hge_ctx.v1q({"type": "remove_remote_schema", "args": {"name": "my remote"}})
        assert st_code == 200, resp

    def test_bulk_remove_add_remote_schema(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir + '/basic_bulk_remove_add.yaml')
        assert st_code == 200, resp


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
        assert row['name'] == "simple2-graphql"
        conn.close()

    def test_add_schema_conflicts_with_tables(self, hge_ctx):
        """add remote schema which conflicts with hasura tables"""
        q = mk_add_remote_q('simple2', 'http://localhost:5000/hello-graphql')
        st_code, resp = hge_ctx.v1q(q)
        assert st_code == 400
        assert resp['code'] == 'remote-schema-conflicts'

    def test_add_second_remote_schema(self, hge_ctx):
        """add 2 remote schemas with different node and types"""
        q = mk_add_remote_q('my remote2', 'http://localhost:5000/country-graphql')
        st_code, resp = hge_ctx.v1q(q)
        assert st_code == 200, resp
        hge_ctx.v1q({"type": "remove_remote_schema", "args": {"name": "my remote2"}})
        assert st_code == 200, resp

    def test_remote_query(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/simple2_query.yaml')

    def test_remote_mutation(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/simple2_mutation.yaml')

    def test_add_conflicting_table(self, hge_ctx):
        st_code, resp = hge_ctx.v1q_f(self.dir + '/create_conflicting_table.yaml')
        assert st_code == 400
        assert resp['code'] == 'remote-schema-conflicts'

    def test_introspection(self, hge_ctx):
        with open('queries/graphql_introspection/introspection.yaml') as f:
            query = yaml.load(f)
        st_code, resp = check_query(hge_ctx, query)
        assert st_code == 200, resp
        assert check_introspection_result(resp, ['User', 'hello'], ['user', 'hello'])

    def test_add_schema_duplicate_name(self, hge_ctx):
        q = mk_add_remote_q('simple2-graphql', 'http://localhost:5000/country-graphql')
        st_code, resp = hge_ctx.v1q(q)
        assert st_code == 500, resp
        assert resp['code'] == 'postgres-error'

    def test_add_schema_same_type(self, hge_ctx):
        """
        test types get merged when remote schema has type with same name and
        same structure
        """
        st_code, resp = hge_ctx.v1q_f(self.dir + '/person_table.yaml')
        assert st_code == 200, resp
        q = mk_add_remote_q('person-graphql', 'http://localhost:5000/person-graphql')

        st_code, resp = hge_ctx.v1q(q)
        assert st_code == 200, resp
        st_code, resp = hge_ctx.v1q_f(self.dir + '/drop_person_table.yaml')
        assert st_code == 200, resp
        hge_ctx.v1q({"type": "remove_remote_schema", "args": {"name": "person-graphql"}})
        assert st_code == 200, resp

#    def test_remote_query_variables(self, hge_ctx):
#        pass
#    def test_add_schema_url_from_env(self, hge_ctx):
#        pass
#    def test_add_schema_header_from_env(self, hge_ctx):
#        pass


def _map(f, l):
    return list(map(f, l))

def _filter(f, l):
    return list(filter(f, l))

def check_introspection_result(res, types, node_names):
    all_types = _map(lambda t: t['name'], res['data']['__schema']['types'])
    print(all_types)
    q_root = _filter(lambda t: t['name'] == 'query_root',
                     res['data']['__schema']['types'])[0]
    all_nodes = _map(lambda f: f['name'], q_root['fields'])
    print(all_nodes)

    satisfy_ty = True
    satisfy_node = True

    for ty_name in types:
        if ty_name not in all_types:
            satisfy_ty = False

    for nn in node_names:
        if nn not in all_nodes:
            satisfy_node = False

    return satisfy_node and satisfy_ty
