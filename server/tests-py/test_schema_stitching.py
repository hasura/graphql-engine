#!/usr/bin/env python3

import string
import random
import yaml
import json
import queue
import requests

import pytest

from validate import check_query_f, check_query


def mk_add_remote_q(name, url, headers=None, client_hdrs=False):
    return {
        "type": "add_remote_schema",
        "args": {
            "name": name,
            "comment": "testing " + name,
            "definition": {
                "url": url,
                "headers": headers,
                "forward_client_headers": client_hdrs
            }
        }
    }

def mk_delete_remote_q(name):
    return {
        "type" : "remove_remote_schema",
        "args" : {
            "name": name
        }
    }

def mk_reload_remote_q(name):
    return {
        "type" : "reload_remote_schema",
        "args" : {
            "name" : name
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
            query = yaml.safe_load(f)
        resp = check_query(hge_ctx, query)
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

    def test_remove_schema_error(self, hge_ctx):
        """remove remote schema which is not added"""
        q = mk_delete_remote_q('random name')
        st_code, resp = hge_ctx.v1q(q)
        assert st_code == 400
        assert resp['code'] == 'not-exists'

    def test_reload_remote_schema(self, hge_ctx):
        """reload a remote schema"""
        q = mk_reload_remote_q('simple 1')
        st_code, resp = hge_ctx.v1q(q)
        assert st_code == 200

    def test_add_second_remote_schema(self, hge_ctx):
        """add 2 remote schemas with different node and types"""
        q = mk_add_remote_q('my remote', 'http://localhost:5000/user-graphql')
        st_code, resp = hge_ctx.v1q(q)
        assert st_code == 200, resp
        st_code, resp = hge_ctx.v1q(mk_delete_remote_q('my remote'))
        assert st_code == 200, resp

    def test_add_remote_schema_with_interfaces(self, hge_ctx):
        """add a remote schema with interfaces in it"""
        q = mk_add_remote_q('my remote interface one', 'http://localhost:5000/character-iface-graphql')
        st_code, resp = hge_ctx.v1q(q)
        assert st_code == 200, resp
        # TODO: Support interface in remote relationships
        # check_query_f(hge_ctx, self.dir + '/character_interface_query.yaml')
        st_code, resp = hge_ctx.v1q(mk_delete_remote_q('my remote interface one'))
        assert st_code == 200, resp

    def test_add_remote_schema_with_interface_err_empty_fields_list(self, hge_ctx):
        """add a remote schema with an interface having no fields"""
        check_query_f(hge_ctx, self.dir + '/add_remote_schema_with_iface_err_empty_fields_list.yaml')

    def test_add_remote_schema_err_unknown_interface(self, hge_ctx):
        """add a remote schema with an interface having no fields"""
        check_query_f(hge_ctx, self.dir + '/add_remote_schema_err_unknown_interface.yaml')

    def test_add_remote_schema_with_interface_err_missing_field(self, hge_ctx):
        """ add a remote schema where an object implementing an interface does
        not have a field defined in the interface """
        check_query_f(hge_ctx, self.dir + '/add_remote_schema_err_missing_field.yaml')

    def test_add_remote_schema_with_interface_err_wrong_field_type(self, hge_ctx):
        """add a remote schema where an object implementing an interface have a
        field with the same name as in the interface, but of different type"""
        check_query_f(hge_ctx, self.dir + '/add_remote_schema_with_iface_err_wrong_field_type.yaml')

    def test_add_remote_schema_with_interface_err_missing_arg(self, hge_ctx):
        """add a remote schema where a field of an object implementing an
        interface does not have the argument defined in the same field of
        interface"""
        check_query_f(hge_ctx, self.dir + '/add_remote_schema_err_missing_arg.yaml')

    def test_add_remote_schema_with_interface_err_wrong_arg_type(self, hge_ctx):
        """add a remote schema where the argument of a field of an object
        implementing the interface does not have the same type as the argument
        defined in the field of interface"""
        check_query_f(hge_ctx, self.dir + '/add_remote_schema_iface_err_wrong_arg_type.yaml')

    def test_add_remote_schema_with_interface_err_extra_non_null_arg(self, hge_ctx):
        """add a remote schema with a field of an object implementing interface
        having extra non_null argument"""
        check_query_f(hge_ctx, self.dir + '/add_remote_schema_with_iface_err_extra_non_null_arg.yaml')

    def test_add_remote_schema_with_union(self, hge_ctx):
        """add a remote schema with union in it"""
        q = mk_add_remote_q('my remote union one', 'http://localhost:5000/union-graphql')
        st_code, resp = hge_ctx.v1q(q)
        assert st_code == 200, resp
        # TODO: Support unions in remote relationships
        # check_query_f(hge_ctx, self.dir + '/search_union_type_query.yaml')
        hge_ctx.v1q({"type": "remove_remote_schema", "args": {"name": "my remote union one"}})
        assert st_code == 200, resp

    def test_add_remote_schema_with_union_err_no_member_types(self, hge_ctx):
        """add a remote schema with a union having no member types"""
        check_query_f(hge_ctx, self.dir + '/add_remote_schema_with_union_err_no_member_types.yaml')

    def test_add_remote_schema_with_union_err_unkown_types(self, hge_ctx):
        """add a remote schema with a union having unknown types as memberTypes"""
        check_query_f(hge_ctx, self.dir + '/add_remote_schema_with_union_err_unknown_types.yaml')

    def test_add_remote_schema_with_union_err_subtype_iface(self, hge_ctx):
        """add a remote schema with a union having interface as a memberType"""
        check_query_f(hge_ctx, self.dir + '/add_remote_schema_with_union_err_member_type_interface.yaml')

    def test_add_remote_schema_with_union_err_wrapped_type(self, hge_ctx):
        """add a remote schema with error in spec for union"""
        check_query_f(hge_ctx, self.dir + '/add_remote_schema_with_union_err_wrapped_type.yaml')

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
        res = hge_ctx.sql('select * from hdb_catalog.remote_schemas')
        row = res.fetchone()
        assert row['name'] == "simple2-graphql"

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
            query = yaml.safe_load(f)
        resp = check_query(hge_ctx, query)
        assert check_introspection_result(resp, ['User', 'hello'], ['user', 'hello'])

    def test_add_schema_duplicate_name(self, hge_ctx):
        q = mk_add_remote_q('simple2-graphql', 'http://localhost:5000/country-graphql')
        st_code, resp = hge_ctx.v1q(q)
        assert st_code == 400, resp
        assert resp['code'] == 'already-exists'

    def test_add_schema_same_type_containing_same_scalar(self, hge_ctx):
        """
        test types get merged when remote schema has type with same name and
        same structure + a same custom scalar
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

    def test_remote_schema_forward_headers(self, hge_ctx):
        """
        test headers from client and conf and resolved info gets passed
        correctly to remote schema, and no duplicates are sent. this test just
        tests if the remote schema returns success or not. checking of header
        duplicate logic is in the remote schema server
        """
        conf_hdrs = [{'name': 'x-hasura-test', 'value': 'abcd'}]
        add_remote = mk_add_remote_q('header-graphql',
                                     'http://localhost:5000/header-graphql',
                                     headers=conf_hdrs, client_hdrs=True)
        st_code, resp = hge_ctx.v1q(add_remote)
        assert st_code == 200, resp
        q = {'query': '{ wassup }'}
        hdrs = {
            'x-hasura-test': 'xyzz',
            'x-hasura-role': 'user',
            'x-hasura-user-id': 'abcd1234',
            'content-type': 'application/json',
            'Authorization': 'Bearer abcdef',
        }
        if hge_ctx.hge_key:
            hdrs['x-hasura-admin-secret'] = hge_ctx.hge_key

        resp = hge_ctx.http.post(hge_ctx.hge_url+'/v1alpha1/graphql', json=q,
                                 headers=hdrs)
        print(resp.status_code, resp.json())
        assert resp.status_code == 200
        res = resp.json()
        assert 'data' in res
        assert res['data']['wassup'] == 'Hello world'

        hge_ctx.v1q({'type': 'remove_remote_schema',
                     'args': {'name': 'header-graphql'}})
        assert st_code == 200, resp


class TestRemoteSchemaQueriesOverWebsocket:
    dir = 'queries/remote_schemas'
    teardown = {"type": "clear_metadata", "args": {}}

    @pytest.fixture(autouse=True)
    def transact(self, hge_ctx, ws_client):
        st_code, resp = hge_ctx.v1q_f('queries/remote_schemas/tbls_setup.yaml')
        assert st_code == 200, resp
        ws_client.init_as_admin()
        yield
        # teardown
        st_code, resp = hge_ctx.v1q_f('queries/remote_schemas/tbls_teardown.yaml')
        assert st_code == 200, resp
        st_code, resp = hge_ctx.v1q(self.teardown)
        assert st_code == 200, resp

    def test_remote_query(self, ws_client):
        query = """
        query {
          user(id: 2) {
            id
            username
          }
        }
        """
        query_id = ws_client.gen_id()
        resp = ws_client.send_query({'query': query}, query_id=query_id,
                                    timeout=5)
        try:
            ev = next(resp)
            assert ev['type'] == 'data' and ev['id'] == query_id, ev
            assert ev['payload']['data']['user']['username'] == 'john'
        finally:
            ws_client.stop(query_id)

    def test_remote_query_error(self, ws_client):
        query = """
        query {
          user(id: 2) {
            blah
            username
          }
        }
        """
        query_id = ws_client.gen_id()
        resp = ws_client.send_query({'query': query}, query_id=query_id,
                                    timeout=5)
        try:
            ev = next(resp)
            print(ev)
            assert ev['type'] == 'error' and ev['id'] == query_id, ev
            assert 'errors' in ev['payload']
            assert ev['payload']['errors'][0]['message'] == \
                'field "blah" not found in type: \'User\''
        finally:
            ws_client.stop(query_id)

    def test_remote_mutation(self, ws_client):
        query = """
        mutation {
          createUser(id: 42, username: "foobar") {
            user {
              id
              username
            }
          }
        }
        """
        query_id = ws_client.gen_id()
        resp = ws_client.send_query({'query': query}, query_id=query_id,
                                    timeout=5)
        try:
            ev = next(resp)
            assert ev['type'] == 'data' and ev['id'] == query_id, ev
            assert ev['payload']['data']['createUser']['user']['id'] == 42
            assert ev['payload']['data']['createUser']['user']['username'] == 'foobar'
        finally:
            ws_client.stop(query_id)


class TestRemoteSchemaResponseHeaders():
    teardown = {"type": "clear_metadata", "args": {}}
    dir = 'queries/remote_schemas'

    @pytest.fixture(autouse=True)
    def transact(self, hge_ctx):
        q = mk_add_remote_q('sample-auth', 'http://localhost:5000/auth-graphql')
        st_code, resp = hge_ctx.v1q(q)
        assert st_code == 200, resp
        yield
        hge_ctx.v1q(self.teardown)

    def test_response_headers_from_remote(self, hge_ctx):
        headers = {}
        if hge_ctx.hge_key:
            headers = {'x-hasura-admin-secret': hge_ctx.hge_key}
        q = {'query': 'query { hello (arg: "me") }'}
        resp = hge_ctx.http.post(hge_ctx.hge_url + '/v1/graphql', json=q,
                                 headers=headers)
        assert resp.status_code == 200
        assert ('Set-Cookie' in resp.headers and
                resp.headers['Set-Cookie'] == 'abcd')
        res = resp.json()
        assert res['data']['hello'] == "Hello me"


class TestAddRemoteSchemaCompareRootQueryFields:

    remote = 'http://localhost:5000/default-value-echo-graphql'

    @pytest.fixture(autouse=True)
    def transact(self, hge_ctx):
        st_code, resp = hge_ctx.v1q(mk_add_remote_q('default_value_test', self.remote))
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q(mk_delete_remote_q('default_value_test'))
        assert st_code == 200, resp

    def test_schema_check_arg_default_values_and_field_and_arg_types(self, hge_ctx):
        with open('queries/graphql_introspection/introspection.yaml') as f:
            query = yaml.safe_load(f)
        introspect_hasura = check_query(hge_ctx, query)
        resp = requests.post(
            self.remote,
            json=query['query']
        )
        introspect_remote = resp.json()
        assert resp.status_code == 200, introspect_remote
        remote_root_ty_info = get_query_root_info(introspect_remote)
        hasura_root_ty_info = get_query_root_info(introspect_hasura)
        has_fld = dict()

        for fldR in remote_root_ty_info['fields']:
            has_fld[fldR['name']] = False
            for fldH in get_fld_by_name(hasura_root_ty_info, fldR['name']):
                has_fld[fldR['name']] = True
                compare_flds(fldH, fldR)
            assert has_fld[fldR['name']], 'Field ' + fldR['name'] + ' in the remote shema root query type not found in Hasura schema'


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

def get_query_root_info(res):
    root_ty_name = res['data']['__schema']['queryType']['name']
    return _filter(lambda ty: ty['name'] == root_ty_name, get_types(res))[0]

def get_types(res):
    return res['data']['__schema']['types']

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

def get_fld_by_name(ty, fldName):
    return _filter(lambda f: f['name'] == fldName, ty['fields'])

def get_arg_by_name(fld, argName):
    return _filter(lambda a: a['name'] == argName, fld['args'])

def compare_args(argH, argR):
    assert argR['type'] == argH['type'], yaml.dump({
        'error' : 'Types do not match for arg ' + arg_path,
        'remote_type' : argR['type'],
        'hasura_type' : argH['type']
    })
    assert argR['defaultValue'] == argH['defaultValue'], yaml.dump({
        'error' : 'Default values do not match for arg ' + arg_path,
        'remote_default_value' : argR['defaultValue'],
        'hasura_default_value' : argH['defaultValue']
    })

def compare_flds(fldH, fldR):
    assert fldH['type'] == fldR['type'], yaml.dump({
        'error' : 'Types do not match for fld ' + fldH['name'],
        'remote_type' : fldR['type'],
        'hasura_type' : fldH['type']
    })
    has_arg = dict()
    for argR in fldR['args']:
        arg_path = fldR['name'] + '(' + argR['name'] + ':)'
        has_arg[arg_path] = False
        for argH in get_arg_by_name(fldH, argR['name']):
            has_arg[arg_path] = True
            compare_args(argH, argR)
        assert has_arg[arg_path], 'Argument ' + arg_path + ' in the remote schema root query type not found in Hasura schema'
