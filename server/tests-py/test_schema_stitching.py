#!/usr/bin/env python3

from ruamel.yaml import YAML
import graphql
import requests
import time

import pytest

pytestmark = [
    pytest.mark.usefixtures('gql_server'),
]

yaml=YAML(typ='safe', pure=True)

from validate import check_query_f, check_query

def mk_add_remote_q(name, url, headers=None, client_hdrs=False, timeout=None, customization=None):
    return {
        "type": "add_remote_schema",
        "args": {
            "name": name,
            "comment": "testing " + name,
            "definition": {
                "url": url,
                "headers": headers,
                "forward_client_headers": client_hdrs,
                "timeout_seconds": timeout,
                "customization": customization
            }
        }
    }

def type_prefix_customization(type_prefix, mapping={}):
    return { "type_names": {"prefix": type_prefix, "mapping": mapping }}

def mk_update_remote_q(name, url, headers=None, client_hdrs=False, timeout=None, customization=None):
    return {
        "type": "update_remote_schema",
        "args": {
            "name": name,
            "comment": "testing " + name,
            "definition": {
                "url": url,
                "headers": headers,
                "forward_client_headers": client_hdrs,
                "timeout_seconds": timeout,
                "customization": customization
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

export_metadata_q = {"type": "export_metadata", "args": {}}

class TestRemoteSchemaBasic:
    """ basic => no hasura tables are tracked """

    teardown = {"type": "clear_metadata", "args": {}}
    dir = 'queries/remote_schemas'

    @pytest.fixture(autouse=True)
    def transact(self, hge_ctx, gql_server):
        q = mk_add_remote_q('simple 1', f'{gql_server.url}/hello-graphql')
        hge_ctx.v1q(q)
        yield
        hge_ctx.v1q(self.teardown)

    def test_add_schema(self, hge_ctx):
        """ check if the remote schema is added in the metadata """
        resp = hge_ctx.v1q(export_metadata_q)
        assert resp['remote_schemas'][0]['name'] == "simple 1"

    def test_update_schema_with_no_url_change(self, hge_ctx, gql_server):
        """ call update_remote_schema API and check the details stored in metadata """
        q = mk_update_remote_q('simple 1', f'{gql_server.url}/hello-graphql', None, True, 120)
        hge_ctx.v1q(q)

        resp = hge_ctx.v1q(export_metadata_q)
        assert resp['remote_schemas'][0]['name'] == "simple 1"
        assert resp['remote_schemas'][0]['definition']['timeout_seconds'] == 120
        assert resp['remote_schemas'][0]['definition']['forward_client_headers'] == True

        """ revert to original config for remote schema """
        q = mk_update_remote_q('simple 1', f'{gql_server.url}/hello-graphql', None, False, 60)
        hge_ctx.v1q(q)

    def test_update_schema_with_url_change(self, hge_ctx, gql_server):
        """ call update_remote_schema API and check the details stored in metadata """
        # This should succeed since there isn't any conflicting relations or permissions set up
        q = mk_update_remote_q('simple 1', f'{gql_server.url}/user-graphql', None, True, 80)
        hge_ctx.v1q(q)

        resp = hge_ctx.v1q(export_metadata_q)
        assert resp['remote_schemas'][0]['name'] == "simple 1"
        assert resp['remote_schemas'][0]['definition']['url'] == f'{gql_server.url}/user-graphql'
        assert resp['remote_schemas'][0]['definition']['timeout_seconds'] == 80
        assert resp['remote_schemas'][0]['definition']['forward_client_headers'] == True

        """ revert to original config for remote schema """
        q = mk_update_remote_q('simple 1', f'{gql_server.url}/hello-graphql', None, False, 60)
        hge_ctx.v1q(q)

    def test_update_schema_with_customization_change(self, hge_ctx, gql_server):
        """ call update_remote_schema API and check the details stored in metadata """
        # This should succeed since there isn't any conflicting relations or permissions set up
        customization = {'type_names': { 'prefix': 'Foo', 'mapping': {'String': 'MyString'}}, 'field_names': [{'parent_type': 'Hello', 'prefix': 'my_', 'mapping': {}}]}
        q = mk_update_remote_q('simple 1', f'{gql_server.url}/hello-graphql', None, False, 60, customization=customization)
        hge_ctx.v1q(q)

        resp = hge_ctx.v1q(export_metadata_q)
        assert resp['remote_schemas'][0]['name'] == "simple 1"
        assert resp['remote_schemas'][0]['definition']['url'] == f'{gql_server.url}/hello-graphql'
        assert resp['remote_schemas'][0]['definition']['timeout_seconds'] == 60
        assert resp['remote_schemas'][0]['definition']['customization'] == customization

        with open('queries/graphql_introspection/introspection.yaml') as f:
            query = yaml.load(f)
        resp, _ = check_query(hge_ctx, query)
        assert check_introspection_result(resp, ['MyString'], ['my_hello'])

        check_query_f(hge_ctx, self.dir + '/basic_query_customized.yaml')

        """ revert to original config for remote schema """
        q = mk_update_remote_q('simple 1', f'{gql_server.url}/hello-graphql', None, False, 60)
        hge_ctx.v1q(q)

        resp = hge_ctx.v1q(export_metadata_q)
        assert 'customization' not in resp['remote_schemas'][0]['definition']

    def test_update_schema_with_customization_change_invalid(self, hge_ctx, gql_server):
        """ call update_remote_schema API and check the details stored in metadata """
        customization = {'type_names': { 'mapping': {'String': 'Foo', 'Hello': 'Foo'} } }
        q = mk_update_remote_q('simple 1', f'{gql_server.url}/hello-graphql', None, False, 60, customization=customization)
        resp = hge_ctx.v1q(q, expected_status_code = 400)
        assert resp['error'] == 'Inconsistent object: Type name mappings are not distinct; the following types appear more than once: "Foo"'

        """ revert to original config for remote schema """
        q = mk_update_remote_q('simple 1', f'{gql_server.url}/hello-graphql', None, False, 60)
        hge_ctx.v1q(q)

    def test_introspection(self, hge_ctx):
        #check_query_f(hge_ctx, 'queries/graphql_introspection/introspection.yaml')
        with open('queries/graphql_introspection/introspection.yaml') as f:
            query = yaml.load(f)
        resp, _ = check_query(hge_ctx, query)
        assert check_introspection_result(resp, ['String'], ['hello'])

    def test_introspection_as_user(self, hge_ctx):
        check_query_f(hge_ctx, 'queries/graphql_introspection/introspection_user_role.yaml')

    def test_remote_query(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/basic_query.yaml')

    def test_remote_subscription(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/basic_subscription_not_supported.yaml')

    def test_add_schema_conflicts(self, hge_ctx, gql_server):
        """add 2 remote schemas with same node or types"""
        q = mk_add_remote_q('simple 2', f'{gql_server.url}/hello-graphql')
        # FYI: resp = ordereddict([('code', 'invalid-configuration'), ('error', "Inconsistent object: Duplicate remote field 'hello', Incons...on', "Inconsistent object: Duplicate remote field 'delayedHello'"), ('type', 'remote_schema')])]), ('path', '$.args')]) 
        resp = hge_ctx.v1q(q, expected_status_code = 400)
        assert resp['code'] == 'invalid-configuration', resp

    def test_remove_schema_error(self, hge_ctx):
        """remove remote schema which is not added"""
        q = mk_delete_remote_q('random name')
        resp = hge_ctx.v1q(q, expected_status_code = 400)
        assert resp['code'] == 'not-exists'

    def test_reload_remote_schema(self, hge_ctx):
        """reload a remote schema"""
        q = mk_reload_remote_q('simple 1')
        hge_ctx.v1q(q)

    def test_add_second_remote_schema(self, hge_ctx, gql_server):
        """add 2 remote schemas with different node and types"""
        q = mk_add_remote_q('my remote', f'{gql_server.url}/user-graphql')
        hge_ctx.v1q(q)
        hge_ctx.v1q(mk_delete_remote_q('my remote'))

    def test_json_scalar_dict(self, hge_ctx, gql_server):
        q = mk_add_remote_q('my remote', f'{gql_server.url}/json-scalar-graphql')
        hge_ctx.v1q(q)
        check_query_f(hge_ctx, self.dir + '/json_scalar.yaml')
        hge_ctx.v1q(mk_delete_remote_q('my remote'))

    def test_add_remote_schema_with_interfaces(self, hge_ctx, gql_server):
        """add a remote schema with interfaces in it"""
        q = mk_add_remote_q('my remote interface one', f'{gql_server.url}/character-iface-graphql')
        hge_ctx.v1q(q)
        check_query_f(hge_ctx, self.dir + '/character_interface_query.yaml')
        hge_ctx.v1q(mk_delete_remote_q('my remote interface one'))

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

    def test_add_remote_schema_with_union(self, hge_ctx, gql_server):
        """add a remote schema with union in it"""
        q = mk_add_remote_q('my remote union one', f'{gql_server.url}/union-graphql')
        hge_ctx.v1q(q)
        check_query_f(hge_ctx, self.dir + '/search_union_type_query.yaml')
        hge_ctx.v1q({"type": "remove_remote_schema", "args": {"name": "my remote union one"}})

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
        hge_ctx.v1q_f(self.dir + '/basic_bulk_remove_add.yaml')

class TestRemoteSchemaBasicExtensions:
    """ basic => no hasura tables are tracked """

    teardown = {"type": "clear_metadata", "args": {}}
    dir = 'queries/remote_schemas'

    @pytest.fixture(autouse=True)
    def transact(self, hge_ctx, gql_server):
        q = mk_add_remote_q('simple 1', f'{gql_server.url}/hello-graphql-extensions')
        hge_ctx.v1q(q)
        yield
        hge_ctx.v1q(self.teardown)

    def test_remote_query(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/basic_query.yaml')


class TestAddRemoteSchemaTbls:
    """ tests with adding a table in hasura """

    dir = 'queries/remote_schemas'

    @pytest.fixture(autouse=True)
    def transact(self, hge_ctx):
        hge_ctx.v1q_f('queries/remote_schemas/tbls_setup.yaml')
        yield
        hge_ctx.v1q_f('queries/remote_schemas/tbls_teardown.yaml')

    def test_add_schema(self, hge_ctx):
        """ check if the remote schema is added in the metadata """
        resp = hge_ctx.v1q(export_metadata_q)
        assert resp['remote_schemas'][0]['name'] == "simple2-graphql"

    def test_add_schema_conflicts_with_tables(self, hge_ctx, gql_server):
        """add remote schema which conflicts with hasura tables"""
        q = mk_add_remote_q('simple2', f'{gql_server.url}/hello-graphql')
        resp = hge_ctx.v1q(q, expected_status_code = 400)
        assert resp['code'] == 'invalid-configuration'

    def test_add_second_remote_schema(self, hge_ctx, gql_server):
        """add 2 remote schemas with different node and types"""
        q = mk_add_remote_q('my remote2', f'{gql_server.url}/country-graphql')
        hge_ctx.v1q(q)
        hge_ctx.v1q({"type": "remove_remote_schema", "args": {"name": "my remote2"}})

    def test_remote_query(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/simple2_query.yaml')

    def test_remote_mutation(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/simple2_mutation.yaml')

    def test_add_conflicting_table(self, hge_ctx):
        resp = hge_ctx.v1q_f(self.dir + '/create_conflicting_table.yaml', expected_status_code = 400)
        assert resp['code'] == 'remote-schema-conflicts'
        # Drop "user" table which is created in the previous test
        hge_ctx.v1q_f(self.dir + '/drop_user_table.yaml')

    def test_introspection(self, hge_ctx):
        with open('queries/graphql_introspection/introspection.yaml') as f:
            query = yaml.load(f)
        resp, _ = check_query(hge_ctx, query)
        assert check_introspection_result(resp, ['User', 'hello'], ['user', 'hello'])

    def test_add_schema_duplicate_name(self, hge_ctx, gql_server):
        q = mk_add_remote_q('simple2-graphql', f'{gql_server.url}/country-graphql')
        resp = hge_ctx.v1q(q, expected_status_code = 400)
        assert resp['code'] == 'already-exists'

    def test_add_schema_same_type_containing_same_scalar(self, hge_ctx, gql_server):
        """
        test types get merged when remote schema has type with same name and
        same structure + a same custom scalar
        """
        hge_ctx.v1q_f(self.dir + '/person_table.yaml')
        q = mk_add_remote_q('person-graphql', f'{gql_server.url}/person-graphql')

        hge_ctx.v1q(q)
        hge_ctx.v1q_f(self.dir + '/drop_person_table.yaml')
        hge_ctx.v1q({"type": "remove_remote_schema", "args": {"name": "person-graphql"}})

    def test_remote_schema_forward_headers(self, hge_ctx, gql_server):
        """
        test headers from client and conf and resolved info gets passed
        correctly to remote schema, and no duplicates are sent. this test just
        tests if the remote schema returns success or not. checking of header
        duplicate logic is in the remote schema server
        """
        conf_hdrs = [{'name': 'x-hasura-test', 'value': 'abcd'}]
        add_remote = mk_add_remote_q('header-graphql',
                                     f'{gql_server.url}/header-graphql',
                                     headers=conf_hdrs, client_hdrs=True)
        hge_ctx.v1q(add_remote)
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
        assert 'data' in res and res['data'] is not None, res
        assert res['data']['wassup'] == 'Hello world'

        hge_ctx.v1q({'type': 'remove_remote_schema', 'args': {'name': 'header-graphql'}})


class TestRemoteSchemaQueriesOverWebsocket:
    dir = 'queries/remote_schemas'
    teardown = {"type": "clear_metadata", "args": {}}

    @pytest.fixture(autouse=True)
    def transact(self, hge_ctx, ws_client):
        hge_ctx.v1q_f('queries/remote_schemas/tbls_setup.yaml')
        ws_client.init_as_admin()
        yield
        # teardown
        hge_ctx.v1q_f('queries/remote_schemas/tbls_teardown.yaml')
        hge_ctx.v1q(self.teardown)

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
            generateError
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
            assert ev['type'] == 'data' and ev['id'] == query_id, ev
            assert 'errors' in ev['payload']
            assert ev['payload']['errors'][0]['message'] == \
                'Cannot query field "generateError" on type "User".'
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
    def transact(self, hge_ctx, gql_server):
        q = mk_add_remote_q('sample-auth', f'{gql_server.url}/auth-graphql')
        hge_ctx.v1q(q)
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

    @pytest.fixture(autouse=True)
    def transact(self, hge_ctx, gql_server):
        remote = f'{gql_server.url}/default-value-echo-graphql'
        hge_ctx.v1q(mk_add_remote_q('default_value_test', remote))
        yield
        hge_ctx.v1q(mk_delete_remote_q('default_value_test'))

    def test_schema_check_arg_default_values_and_field_and_arg_types(self, hge_ctx, gql_server):
        remote = f'{gql_server.url}/default-value-echo-graphql'
        with open('queries/graphql_introspection/introspection.yaml') as f:
            query = yaml.load(f)
        introspect_hasura, _ = check_query(hge_ctx, query)
        resp = requests.post(remote, json=query['query'])
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

class TestRemoteSchemaTimeout:
    dir = 'queries/remote_schemas'
    teardown = {"type": "clear_metadata", "args": {}}

    @pytest.fixture(autouse=True)
    def transact(self, hge_ctx):
        q = mk_add_remote_q('simple 1', '{{REMOTE_SCHEMAS_WEBHOOK_DOMAIN}}/hello-graphql', timeout = 5)
        hge_ctx.v1q(q)
        yield
        hge_ctx.v1q(self.teardown)

    def test_remote_query_timeout(self, hge_ctx):
        with open(self.dir + '/basic_timeout_query.yaml') as f:
            query = yaml.load(f)
        resp, _ = check_query(hge_ctx, query)

        # tests for query timeout
        assert resp["errors"][0]["extensions"]["internal"]["type"] == "http_exception"
        assert resp["errors"][0]["extensions"]["internal"]["message"] == "Response timeout"

        # tests that graphql server url environment variable template did not serialize in the error message
        assert resp["errors"][0]["message"] == 'HTTP exception occurred while sending the request to "{{REMOTE_SCHEMAS_WEBHOOK_DOMAIN}}/hello-graphql"'

        # wait for graphql server to finish else teardown throws
        time.sleep(6)

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

def compare_args(arg_path, argH, argR):
    assert argR['type'] == argH['type'], yaml.dump({
        'error' : 'Types do not match for arg ' + arg_path,
        'remote_type' : argR['type'],
        'hasura_type' : argH['type']
    })
    compare_default_value(argR['defaultValue'], argH['defaultValue'])

# There doesn't seem to be any Python code that can correctly compare GraphQL
# 'Value's for equality. So we try to do it here.
def compare_default_value(valH, valR):
    a = graphql.parse_value(valH)
    b = graphql.parse_value(valR)
    if a == b:
        return True
    for field in a.fields:
        assert field in b.fields
    for field in b.fields:
        assert field in a.fields

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
            compare_args(arg_path, argH, argR)
        assert has_arg[arg_path], 'Argument ' + arg_path + ' in the remote schema root query type not found in Hasura schema'

reload_metadata_q = {
    'type': 'reload_metadata',
    "args": {
        "reload_remote_schemas": True
    }
}

get_inconsistent_metadata_q = {
    'type': 'get_inconsistent_metadata',
    'args': {}
}

class TestRemoteSchemaReload:

    def test_inconsistent_remote_schema_reload_metadata(self, gql_server, hge_ctx):
        # Add remote schema
        hge_ctx.v1q(mk_add_remote_q('simple 1', f'{gql_server.url}/hello-graphql'))
        # stop remote graphql server
        gql_server.stop_server()
        # Reload metadata with remote schemas
        resp = hge_ctx.v1q(reload_metadata_q)
        # Check if the remote schema present in inconsistent metadata
        assert resp['is_consistent'] == False, resp
        assert resp['inconsistent_objects'][0]['type'] == 'remote_schema', resp
        # Restart remote graphql server
        gql_server.start_server()
        # Reload the inconsistent remote schema
        hge_ctx.v1q(mk_reload_remote_q('simple 1'))
        # Check if metadata is consistent
        resp = hge_ctx.v1q(get_inconsistent_metadata_q)
        assert resp['is_consistent'] == True, resp
        # Delete remote schema
        hge_ctx.v1q(mk_delete_remote_q('simple 1'))

@pytest.mark.usefixtures('per_class_tests_db_state')
class TestValidateRemoteSchemaQuery:

    @classmethod
    def dir(cls):
        return "queries/remote_schemas/validation/"

    def test_remote_schema_argument_validation(self, hge_ctx):
        """ test to check that the graphql-engine throws an validation error
            when an remote object is queried with an unknown argument  """
        check_query_f(hge_ctx, self.dir() + '/argument_validation.yaml')

    def test_remote_schema_field_validation(self, hge_ctx):
        """ test to check that the graphql-engine throws an validation error
            when an remote object is queried with an unknown field  """
        check_query_f(hge_ctx, self.dir() + '/field_validation.yaml')

class TestRemoteSchemaTypePrefix:
    """ basic => no hasura tables are tracked """

    teardown = {"type": "clear_metadata", "args": {}}
    dir = 'queries/remote_schemas'

    @pytest.fixture(autouse=True)
    def transact(self, hge_ctx, gql_server):
        q = mk_add_remote_q('simple 2', f'{gql_server.url}/user-graphql', customization=type_prefix_customization("Foo"))
        hge_ctx.v1q(q)
        yield
        hge_ctx.v1q(self.teardown)

    def test_add_schema(self, hge_ctx):
        """ check if the remote schema is added in the metadata """
        resp = hge_ctx.v1q(export_metadata_q)
        found = [schema for schema in resp['remote_schemas'] if schema['name'] == 'simple 2']
        assert len(found) == 1, resp

    def test_introspection(self, hge_ctx):
        #check_query_f(hge_ctx, 'queries/graphql_introspection/introspection.yaml')
        with open('queries/graphql_introspection/introspection.yaml') as f:
            query = yaml.load(f)
        resp, _ = check_query(hge_ctx, query)
        assert check_introspection_result(resp, ['FooUser', 'FooCreateUser', 'FooCreateUserInputObject', 'FooUserDetailsInput'], ['user', 'allUsers'])

class TestValidateRemoteSchemaTypePrefixQuery:

    teardown = {"type": "clear_metadata", "args": {}}

    @pytest.fixture(autouse=True)
    def transact(self, hge_ctx, gql_server):
        q = mk_add_remote_q('character-foo', f'{gql_server.url}/character-iface-graphql', customization=type_prefix_customization("Foo"))
        hge_ctx.v1q(q)
        yield
        hge_ctx.v1q(self.teardown)

    @classmethod
    def dir(cls):
        return "queries/remote_schemas/validation/"

    def test_remote_schema_type_prefix_validation(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/type_prefix_validation.yaml')

class TestValidateRemoteSchemaFieldPrefixQuery:

    teardown = {"type": "clear_metadata", "args": {}}

    @pytest.fixture(autouse=True)
    def transact(self, hge_ctx, gql_server):
        customization = { "field_names": [{"parent_type": "Character", "prefix": "foo_"},{"parent_type": "Human", "prefix": "foo_"},{"parent_type": "Droid", "prefix": "foo_"}] }
        q = mk_add_remote_q('character-foo', f'{gql_server.url}/character-iface-graphql', customization=customization)
        hge_ctx.v1q(q)
        yield
        hge_ctx.v1q(self.teardown)

    @classmethod
    def dir(cls):
        return "queries/remote_schemas/validation/"

    def test_remote_schema_field_prefix_validation(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/field_prefix_validation.yaml')

class TestValidateRemoteSchemaCustomization:
    @classmethod
    def dir(cls):
        return "queries/remote_schemas/validation/"

    def test_remote_schema_interface_field_validation(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/interface_field_validation.yaml')

class TestValidateRemoteSchemaNamespaceQuery:

    teardown = {"type": "clear_metadata", "args": {}}

    @pytest.fixture(autouse=True)
    def transact(self, hge_ctx, gql_server):
        customization = { "root_fields_namespace": "foo" }
        q = mk_add_remote_q('character-foo', f'{gql_server.url}/character-iface-graphql', customization=customization)
        hge_ctx.v1q(q)
        yield
        hge_ctx.v1q(self.teardown)

    @classmethod
    def dir(cls):
        return "queries/remote_schemas/validation/"

    def test_remote_schema_namespace_validation(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/namespace_validation.yaml')

    def test_multiple_remote_schema_with_namespace(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/multiple_remote_schema_with_namespace.yaml')

class TestValidateRemoteSchemaCustomizeAllTheThings:

    teardown = {"type": "clear_metadata", "args": {}}

    @pytest.fixture(autouse=True)
    def transact(self, hge_ctx, gql_server):
        customization = {
            "root_fields_namespace": "star_wars",
            "type_names": {"prefix": "Foo", "suffix": "_x", "mapping": { "Droid": "Android", "Int": "MyInt"}},
            "field_names": [
                    {"parent_type": "Character", "prefix": "foo_", "suffix": "_f", "mapping": {"id": "ident"}},
                    {"parent_type": "Human", "mapping": {"id": "ident", "name": "foo_name_f", "droid": "android"}},
                    {"parent_type": "Droid", "prefix": "foo_", "suffix": "_f", "mapping": {"id": "ident"}},
                    {"parent_type": "CharacterIFaceQuery", "prefix": "super_" }
                ]
            }
        q = mk_add_remote_q('character-foo', f'{gql_server.url}/character-iface-graphql', customization=customization)
        hge_ctx.v1q(q)
        yield
        hge_ctx.v1q(self.teardown)

    @classmethod
    def dir(cls):
        return "queries/remote_schemas/validation/"

    def test_remote_schema_customize_all_the_things(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/customize_all_the_things.yaml')

class TestRemoteSchemaRequestPayload:
    dir = 'queries/remote_schemas'
    teardown = {"type": "clear_metadata", "args": {}}

    @pytest.fixture(autouse=True)
    def transact(self, hge_ctx, gql_server):
        q = mk_add_remote_q('echo request', f'{gql_server.url}/hello-echo-request-graphql')
        hge_ctx.v1q(q)
        yield
        hge_ctx.v1q(self.teardown)

    def test_remote_schema_operation_name_in_response(self, hge_ctx):

        with open('queries/remote_schemas/basic_query_with_op_name.yaml') as f:
            query = yaml.load(f)
        resp, _ = check_query(hge_ctx, query)

        assert resp['data']['hello']['operationName'] == "HelloMe"
