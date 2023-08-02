#!/usr/bin/env python3

import pytest
import time

from conftest import use_action_fixtures, extract_server_address_from
from remote_server import NodeGraphQL
from validate import check_query_f, check_query, get_conf_f

"""
TODO:- Test Actions metadata
"""

@pytest.fixture(scope='class')
@pytest.mark.early
def graphql_service(worker_id: str, hge_fixture_env: dict[str, str]):
    (_, port) = extract_server_address_from('GRAPHQL_SERVICE_HANDLER')
    server = NodeGraphQL(worker_id, 'remote_schemas/nodejs/actions_remote_join_schema.js', port=port)
    server.start()
    print(f'{graphql_service.__name__} server started on {server.url}')
    hge_fixture_env['GRAPHQL_SERVICE_HANDLER'] = server.url
    yield server
    server.stop()


use_action_fixtures_with_remote_joins = pytest.mark.usefixtures(
    "graphql_service",
    "actions_fixture",
    "per_class_db_schema_for_mutation_tests",
    "per_method_db_data_for_mutation_tests"
)

@pytest.mark.parametrize("transport", ['websocket'])
@use_action_fixtures
class TestActionsSyncWebsocket:

    @classmethod
    def dir(cls):
        return 'queries/actions/sync'

    def test_create_user_fail(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/create_user_fail.yaml', transport)

    def test_create_user_success(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/create_user_success.yaml', transport)

    def test_create_user_relationship(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/create_user_relationship.yaml', transport)

    def test_create_user_relationship(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/create_user_relationship_fail.yaml', transport)

    def test_create_users_fail(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/create_users_fail.yaml', transport)

    def test_create_users_success(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/create_users_success.yaml', transport)

@pytest.mark.parametrize("transport", ['http', 'websocket'])
@pytest.mark.usefixtures(
    "graphql_service",
    "actions_fixture",
    'per_class_tests_db_state'
)
class TestActionsRelationshipsBasic:

    @classmethod
    def dir(cls):
        return 'queries/actions/relationships/basic'

    def test_query_with_relationships(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_with_relationships.yaml', transport)

@use_action_fixtures
class TestActionsSync:

    @classmethod
    def dir(cls):
        return 'queries/actions/sync'

    def test_null_response(self, hge_ctx):
        check_query_secret(hge_ctx, self.dir() + '/null_response.yaml')
    
    def test_omitted_field_response_for_nullable_field(self, hge_ctx):
        check_query_secret(hge_ctx, self.dir() + '/omitted_field_response_for_nullable_field.yaml')

    def test_expecting_object_response_got_null(self, hge_ctx):
        check_query_secret(hge_ctx, self.dir() + '/expecting_object_response_got_null.yaml')

    def test_expecting_array_response_got_null(self, hge_ctx):
        check_query_secret(hge_ctx, self.dir() + '/expecting_array_response_got_null.yaml')

    def test_expecting_object_response_got_array(self, hge_ctx):
        check_query_secret(hge_ctx, self.dir() + '/expecting_object_response.yaml')

    def test_expecting_array_response_got_object(self, hge_ctx):
        check_query_secret(hge_ctx, self.dir() + '/expecting_array_response.yaml')

    # Scalar webhook response tests.
    def test_expecting_scalar_output_type_success(self, hge_ctx):
        check_query_secret(hge_ctx, self.dir() + '/get_scalar_action_output_type_success.yaml')

    def test_expecting_scalar_string_output_type_got_object(self, hge_ctx):
        check_query_secret(hge_ctx, self.dir() + '/expecting_scalar_response_got_object.yaml')

    def test_expecting_object_output_type_got_scalar_string(self, hge_ctx):
        check_query_secret(hge_ctx, self.dir() + '/expecting_object_response_got_scalar.yaml')

    def test_scalar_response_action_transformed_output(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/scalar_response_action_transformed_output.yaml')

    def test_object_response_action_transformed_output(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/object_response_action_transformed_output.yaml')

    # Scalar array tests
    def test_expecting_string_scalar_array_output_type_success(self, hge_ctx):
        check_query_secret(hge_ctx, self.dir() + '/get_string_scalar_array_action_output_type_success.yaml')

    def test_expecting_number_scalar_array_output_type_got_string_array(self, hge_ctx):
        check_query_secret(hge_ctx, self.dir() + '/get_string_scalar_array_action_output_type_expecting_number_array.yaml')

    def test_scalar_array_field_nullability_check(self, hge_ctx):
        check_query_secret(hge_ctx, self.dir() + '/get_null_field_expecting_non_nullable_field_array.yaml')

    def test_expecting_object_response_with_nested_null(self, hge_ctx):
       check_query_f(hge_ctx, self.dir() + '/expecting_object_response_with_nested_null.yaml')

    def test_expecting_jsonb_response_success(self, hge_ctx):
       check_query_f(hge_ctx, self.dir() + '/expecting_jsonb_response_success.yaml')

    def test_expecting_custom_scalar_response_success(self, hge_ctx):
       check_query_f(hge_ctx, self.dir() + '/expecting_custom_scalar_response_success.yaml')

    def test_expecting_custom_scalar_array_response_success(self, hge_ctx):
       check_query_f(hge_ctx, self.dir() + '/expecting_custom_scalar_array_response_success.yaml')

    def test_expecting_custom_scalar_array_response_got_different_type(self, hge_ctx):
        query_obj = {
            "query": """
                mutation {
                    custom_scalar_nested_array_response
                }
            """
        }
        headers = {}
        admin_secret = hge_ctx.hge_key
        if admin_secret is not None:
            headers['X-Hasura-Admin-Secret'] = admin_secret
        code, resp, _ = hge_ctx.anyq('/v1/graphql', query_obj, headers)
        assert code == 200, resp

        error_message = resp['errors'][0]['message']

        assert error_message == 'expecting array for the action webhook response', error_message

    def test_expecting_object_response_with_nested_null_wrong_field(self, hge_ctx):
        query_obj = {
            "query": """
                query {
                    typed_nested_null_wrong_field {
                        id
                        child {
                        id
                        }
                    }
                }
            """
        }
        headers = {}
        admin_secret = hge_ctx.hge_key
        if admin_secret is not None:
            headers['X-Hasura-Admin-Secret'] = admin_secret
        code, resp, _ = hge_ctx.anyq('/v1/graphql', query_obj, headers)
        assert code == 200, resp

        error_message = resp['errors'][0]['message']

        assert error_message == 'expecting not null value for field "id"', error_message

    # Webhook response validation tests. See https://github.com/hasura/graphql-engine/issues/3977
    def test_mirror_action_not_null(self, hge_ctx):
        check_query_secret(hge_ctx, self.dir() + '/mirror_action_not_null.yaml')

    def test_mirror_action_unexpected_field(self, hge_ctx):
        check_query_secret(hge_ctx, self.dir() + '/mirror_action_unexpected_field.yaml')

    def test_mirror_action_no_field(self, hge_ctx):
        check_query_secret(hge_ctx, self.dir() + '/mirror_action_no_field.yaml')

    def test_mirror_action_success(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/mirror_action_success.yaml')

    def test_mirror_action_transformed_success(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/mirror_action_transformed_success.yaml')

    def test_mirror_action_transformed_output_success(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/mirror_action_transformed_output_success.yaml')

    def test_mirror_headers(self, hge_ctx):
        query = """
          query {
            mirror_headers {
              headers {
                name
                value
              }
            }
          }
        """
        query_obj = {
            "query": query
        }
        headers = {}
        admin_secret = hge_ctx.hge_key
        if admin_secret is not None:
            headers['X-Hasura-Admin-Secret'] = admin_secret
        code, resp, _ = hge_ctx.anyq('/v1/graphql', query_obj, headers)
        assert code == 200, resp

        resp_headers = resp['data']['mirror_headers']['headers']

        user_agent_header = next((h['value'] for h in resp_headers if h['name'] == 'User-Agent'), None)
        assert user_agent_header is not None
        assert user_agent_header.startswith("hasura-graphql-engine/")

    def test_results_list_transformed_output_success(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/results_list_transformed_output_success.yaml')

    #https://github.com/hasura/graphql-engine/issues/6631
    def test_create_users_output_type(self, hge_ctx):
        gql_query = '''
        query {
          __type(name: "mutation_root"){
            fields {
              name
              type{
                kind
              }
            }
          }
        }
        '''
        query = {
            'query': gql_query
        }
        headers = {}
        admin_secret = hge_ctx.hge_key
        if admin_secret is not None:
            headers['X-Hasura-Admin-Secret'] = admin_secret
        code, resp, _ = hge_ctx.anyq('/v1/graphql', query, headers)
        assert code == 200, resp
        resp_data = resp['data']
        mutation_root_fields = resp_data['__type']['fields']
        # check type for create_users root field
        for root_field in mutation_root_fields:
            if root_field['name'] == 'create_users':
                assert root_field['type']['kind'] == 'LIST', root_field

@use_action_fixtures_with_remote_joins
class TestActionsSyncWithRemoteJoins:

    @classmethod
    def dir(cls):
        return 'queries/actions/sync/remote_joins'

    def test_action_with_remote_joins(self,hge_ctx):
        check_query_f(hge_ctx,self.dir() + '/action_with_remote_joins.yaml')

# Check query with admin secret tokens
def check_query_secret(hge_ctx, f):
    conf = get_conf_f(f)
    admin_secret = hge_ctx.hge_key
    def add_secret(c):
        if admin_secret is not None:
            if 'headers' in c:
                c['headers']['x-hasura-admin-secret'] = admin_secret
            else:
                c['headers'] = {
                    'x-hasura-admin-secret': admin_secret
                }
        return c

    if isinstance(conf, list):
        for _, sconf in enumerate(conf):
            check_query(hge_ctx, add_secret(sconf), add_auth = False)
    else:
            check_query(hge_ctx, add_secret(conf), add_auth = False)

@use_action_fixtures
class TestQueryActions:

    @classmethod
    def dir(cls):
        return 'queries/actions/sync'

    # toplevel, extensions with error
    def test_query_action_extensions_code_both_codes_fail(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/extensions_code_both_codes.yaml')
    # toplevel, extensions with no error
    def test_query_action_extensions_code_toplevel_empty_extensions_fail(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/extensions_code_toplevel_empty_extensions.yaml')
    # toplevel, no extensions
    def test_query_action_extensions_code_toplevel_no_extensions_fail(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/extensions_code_toplevel_no_extensions.yaml')
    # no toplevel, extensions with error
    def test_query_action_extensions_code_only_extensions_code_fail(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/extensions_code_only_extensions_code.yaml')
    # no toplevel, extensions with no error
    def test_query_action_extensions_code_only_empty_extensions_fail(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/extensions_code_only_empty_extensions.yaml')
    # no toplevel, no extensions
    def test_query_action_extensions_code_nothing_fail(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/extensions_code_nothing.yaml')

    def test_query_action_fail(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/get_user_by_email_fail.yaml')

    def test_query_action_success_output_object(self, hge_ctx):
        gql_query = '''
        mutation {
          insert_user_one(object: {email: "clarke@gmail.com", name:"Clarke"}){
            id
          }
        }
        '''
        query = {
            'query': gql_query
        }
        headers = {}
        admin_secret = hge_ctx.hge_key
        if admin_secret is not None:
            headers['X-Hasura-Admin-Secret'] = admin_secret
        code, resp, _ = hge_ctx.anyq('/v1/graphql', query, headers)
        assert code == 200,resp
        check_query_f(hge_ctx, self.dir() + '/get_user_by_email_success.yaml')

    def test_query_action_success_output_nested_object(self, hge_ctx):
        gql_query = '''
        mutation {
          insert_user_one(object: {email: "clarke@gmail.com", name:"Clarke"}){
            id
          }
        }
        '''
        query = {
            'query': gql_query
        }
        headers = {}
        admin_secret = hge_ctx.hge_key
        if admin_secret is not None:
            headers['X-Hasura-Admin-Secret'] = admin_secret
        code, resp, _ = hge_ctx.anyq('/v1/graphql', query, headers)
        assert code == 200,resp
        check_query_f(hge_ctx, self.dir() + '/get_user_by_email_nested_success.yaml')

    def test_query_action_success_output_nested_join(self, hge_ctx):
        gql_query = '''
        mutation {
          insert_user_one(object: {email: "clarke@gmail.com", name:"Clarke"}){
            id
          }
        }
        '''
        query = {
            'query': gql_query
        }
        headers = {}
        admin_secret = hge_ctx.hge_key
        if admin_secret is not None:
            headers['X-Hasura-Admin-Secret'] = admin_secret
        code, resp, _ = hge_ctx.anyq('/v1/graphql', query, headers)
        assert code == 200,resp
        check_query_f(hge_ctx, self.dir() + '/get_user_by_email_nested_join_success.yaml')

    def test_query_action_success_output_list(self, hge_ctx):
        gql_query = '''
        mutation {
          insert_user(objects:
        [{id:1,email: "clarke@gmail.com", name:"Clarke 1"},
         {id:2,email: "clarke@gmail.com", name:"Clarke 2"}])
        {
            returning {
               id
           }
          }
        }
        '''
        query = {
            'query': gql_query
        }
        headers = {}
        admin_secret = hge_ctx.hge_key
        if admin_secret is not None:
            headers['X-Hasura-Admin-Secret'] = admin_secret
        code, resp, _ = hge_ctx.anyq('/v1/graphql', query, headers)
        assert code == 200,resp
        check_query_f(hge_ctx, self.dir() + '/get_users_by_email_success.yaml')

    # This test is to make sure that query actions work well with variables.
    # Earlier the HGE used to add the query action to the plan cache, which
    # results in interrmittent validation errors, like:
    # {
    #   "errors": [
    #     {
    #       "extensions": {
    #         "path": "$.variableValues",
    #         "code": "validation-failed"
    #       },
    #       "message": "unexpected variables: email"
    #     }
    #   ]
    # }
    def test_query_action_should_not_throw_validation_error(self, hge_ctx):
        for _ in range(25):
            self.test_query_action_success_output_object(hge_ctx)

    def test_query_action_with_relationship(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/query_action_relationship_with_permission.yaml')

    def test_query_action_recursive_output(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/query_action_recursive_output.yaml')

def mk_headers_with_secret(hge_ctx, headers={}):
    admin_secret = hge_ctx.hge_key
    if admin_secret:
        headers['X-Hasura-Admin-Secret'] = admin_secret
    return headers

@use_action_fixtures
class TestActionsSyncResponseHeaders:

    @classmethod
    def dir(cls):
        return 'queries/actions/sync'

    # See https://github.com/hasura/graphql-engine/issues/4021
    def test_set_cookie_header(self, hge_ctx):
        mutation = '''
          mutation {
            create_user(email: "clarke@gmail.com", name: "Clarke"){
              id
            }
          }
        '''
        query = {
            'query': mutation,
            'variables': {}
        }
        status, resp, resp_headers = hge_ctx.anyq('/v1/graphql', query, mk_headers_with_secret(hge_ctx))
        assert status == 200, resp
        assert 'data' in resp, resp
        assert ('Set-Cookie' in resp_headers and
                resp_headers['Set-Cookie'] == 'abcd'), resp_headers


@use_action_fixtures
class TestActionsAsync:
    @classmethod
    def dir(cls):
        return 'queries/actions/async'

    def test_create_user_fail(self, hge_ctx):
        graphql_mutation = '''
        mutation {
          create_user(email: "random-email", name: "Clarke")
        }
        '''
        query = {
            'query': graphql_mutation,
            'variables': {}
        }
        status, resp, _ = hge_ctx.anyq('/v1/graphql', query, mk_headers_with_secret(hge_ctx))
        assert status == 200, resp
        assert 'data' in resp
        action_id = resp['data']['create_user']

        query_async = '''
        query ($action_id: uuid!){
          create_user(id: $action_id){
            id
            errors
          }
        }
        '''
        query = {
            'query': query_async,
            'variables': {
                'action_id': action_id
            }
        }
        response = {
            'data': {
                'create_user': {
                    'id': action_id,
                    'errors': {
                        'code': 'invalid-email',
                        'path': '$',
                        'error': 'Given email address is not valid'
                    }
                }
            }
        }
        conf = {
            'url': '/v1/graphql',
            'headers': {},
            'query': query,
            'status': 200,
            'response': response
        }
        check_query_timeout(hge_ctx, conf, True, 10)

    def test_create_user_success(self, hge_ctx):
        graphql_mutation = '''
        mutation {
          create_user(email: "clarke@hasura.io", name: "Clarke")
        }
        '''
        query = {
            'query': graphql_mutation,
            'variables': {}
        }
        status, resp, _ = hge_ctx.anyq('/v1/graphql', query, mk_headers_with_secret(hge_ctx))
        assert status == 200, resp
        assert 'data' in resp
        action_id = resp['data']['create_user']

        query_async = '''
        query ($action_id: uuid!){
          create_user(id: $action_id){
            __typename
            id
            output {
              __typename
              id
              user {
                __typename
                name
                email
                is_admin
              }
            }
          }
        }
        '''
        query = {
            'query': query_async,
            'variables': {
                'action_id': action_id
            }
        }
        response = {
            'data': {
                'create_user': {
                    '__typename': 'create_user',
                    'id': action_id,
                    'output': {
                        '__typename': 'UserId',
                        'id': 1,
                        'user': {
                            '__typename': 'user',
                            'name': 'Clarke',
                            'email': 'clarke@hasura.io',
                            'is_admin': False
                        }
                    }
                }
            }
        }
        conf = {
            'url': '/v1/graphql',
            'headers': {},
            'query': query,
            'status': 200,
            'response': response
        }
        check_query_timeout(hge_ctx, conf, True, 10)

    def test_create_user_roles(self, hge_ctx):
        graphql_mutation = '''
        mutation {
          create_user(email: "blake@hasura.io", name: "Blake")
        }
        '''
        query = {
            'query': graphql_mutation,
            'variables': {}
        }
        headers_user_1 = mk_headers_with_secret(hge_ctx, {
            'X-Hasura-Role': 'user',
            'X-Hasura-User-Id': '1'
        })
        # create action with user-id 1
        status, resp, headers = hge_ctx.anyq('/v1/graphql', query, headers_user_1)
        assert status == 200, resp
        assert 'data' in resp
        action_id = resp['data']['create_user']

        query_async = '''
        query ($action_id: uuid!){
          create_user(id: $action_id){
            id
            output {
              id
            }
          }
        }
        '''
        query = {
            'query': query_async,
            'variables': {
                'action_id': action_id
            }
        }

        headers_user_2 = mk_headers_with_secret(hge_ctx, {
            'X-Hasura-Role': 'user',
            'X-Hasura-User-Id': '2'
        })
        conf_user_2 = {
            'url': '/v1/graphql',
            'headers': headers_user_2,
            'query': query,
            'status': 200,
            'response': {
                'data': {
                    'create_user': None # User 2 shouldn't able to access the action
                }
            }
        }
        # Query the action as user-id 2
        # Make request without auth using admin_secret
        check_query_timeout(hge_ctx, conf_user_2, add_auth = False, timeout = 10)

        conf_user_1 = {
            'url': '/v1/graphql',
            'headers': headers_user_1,
            'query': query,
            'status': 200,
            'response': {
                'data': {
                    'create_user': {
                        'id': action_id,
                        'output': {
                            'id': 1
                        }
                    }
                }
            }
        }
        # Query the action as user-id 1
        # Make request without auth using admin_secret
        check_query_timeout(hge_ctx, conf_user_1, add_auth = False, timeout = 10)

    def test_create_user_transformed_success(self, hge_ctx):
        graphql_mutation = '''
        mutation {
          create_user_transformed(email: "clarke@hasura.io", name: "Clarke")
        }
        '''
        query = {
            'query': graphql_mutation,
            'variables': {}
        }
        status, resp, _ = hge_ctx.anyq('/v1/graphql', query, mk_headers_with_secret(hge_ctx))
        assert status == 200, resp
        assert 'data' in resp
        action_id = resp['data']['create_user_transformed']

        query_async = '''
        query ($action_id: uuid!){
          create_user_transformed(id: $action_id){
            __typename
            id
            output {
              __typename
              id
              user {
                __typename
                name
                email
                is_admin
              }
            }
          }
        }
        '''
        query = {
            'query': query_async,
            'variables': {
                'action_id': action_id
            }
        }
        response = {
            'data': {
                'create_user_transformed': {
                    '__typename': 'create_user_transformed',
                    'id': action_id,
                    'output': {
                        '__typename': 'UserId',
                        'id': 1,
                        'user': {
                            '__typename': 'user',
                            'name': 'notClarke',
                            'email': 'foo@bar.com',
                            'is_admin': False
                        }
                    }
                }
            }
        }
        conf = {
            'url': '/v1/graphql',
            'headers': {},
            'query': query,
            'status': 200,
            'response': response
        }
        check_query_timeout(hge_ctx, conf, True, 10)

    def test_create_user_nested_success(self, hge_ctx):
        graphql_mutation = '''
        mutation {
          create_user_nested(email: "clarke@hasura.io", name: "Clarke")
        }
        '''
        query = {
            'query': graphql_mutation,
            'variables': {}
        }
        status, resp, _ = hge_ctx.anyq('/v1/graphql', query, mk_headers_with_secret(hge_ctx))
        assert status == 200, resp
        assert 'data' in resp
        action_id = resp['data']['create_user_nested']

        query_async = '''
        query ($action_id: uuid!){
          create_user_nested(id: $action_id){
            __typename
            id
            output {
              __typename
              userObj {
                __typename
                id
              }
            }
          }
        }
        '''
        query = {
            'query': query_async,
            'variables': {
                'action_id': action_id
            }
        }
        response = {
            'data': {
                'create_user_nested': {
                    '__typename': 'create_user_nested',
                    'id': action_id,
                    'output': {
                        '__typename': 'UserIdNested',
                        'userObj': {
                            '__typename': 'UserIdObj',
                            'id': 1
                        }
                    }
                }
            }
        }
        conf = {
            'url': '/v1/graphql',
            'headers': {},
            'query': query,
            'status': 200,
            'response': response
        }
        check_query_timeout(hge_ctx, conf, True, 10)

def check_query_timeout(hge_ctx, conf, add_auth, timeout):
    wait_until = time.time() + timeout
    while True:
        time.sleep(2)
        try:
            check_query(hge_ctx, conf, add_auth = add_auth)
        except AssertionError:
            if time.time() > wait_until:
                raise
            else:
                continue
        break

@use_action_fixtures
class TestCreateActionNestedTypeWithRelation:
    @classmethod
    def dir(cls):
        return 'queries/actions/nested-relation'

    # no toplevel, extensions with no error
    def test_create_async_action_with_nested_output_and_relation_fail(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/create_async_action_with_nested_output_and_relation.yaml')

    # no toplevel, extensions with no error
    def test_create_sync_action_with_nested_output_and_nested_relation(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/create_sync_action_with_nested_output_and_nested_relation.yaml')

@pytest.mark.usefixtures('postgis', 'actions_fixture', 'per_class_tests_db_state')
class TestSetCustomTypes:

    @classmethod
    def dir(cls):
        return 'queries/actions/custom-types'

    def test_reuse_pgscalars(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/reuse_pgscalars.yaml')

    def test_reuse_unknown_pgscalar(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/reuse_unknown_pgscalar.yaml')

    def test_create_action_pg_scalar(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/create_action_pg_scalar.yaml')

    def test_list_type_relationship(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/list_type_relationship.yaml')

    def test_drop_relationship(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/drop_relationship.yaml')

@pytest.mark.usefixtures('actions_fixture', 'per_class_tests_db_state')
class TestActionsMetadata:

    @classmethod
    def dir(cls):
        return 'queries/actions/metadata'

    def test_recreate_permission(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/recreate_permission.yaml')

    def test_create_with_headers(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/create_with_headers.yaml')

@pytest.mark.usefixtures('per_class_tests_db_state')
class TestActionIntrospection:

    @classmethod
    def dir(cls):
        return 'queries/actions/introspection'

    def test_introspection_query(self, hge_ctx):
        conf = get_conf_f(self.dir() + '/introspection_query.yaml')
        headers = {}
        admin_secret = hge_ctx.hge_key
        if admin_secret:
            headers['X-Hasura-Admin-Secret'] = admin_secret
        code, resp, _ = hge_ctx.anyq(conf['url'], conf['query'], headers)
        assert code == 200, resp
        assert 'data' in resp, resp

    def test_output_types(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/output_types_query.yaml')

@pytest.mark.usefixtures('per_class_tests_db_state')
class TestFunctionReturnTypeIntrospection:

    @classmethod
    def dir(cls):
        return 'queries/actions/introspection/function_return_type'

    def test_function_return_type(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/function_return_type.yaml')


@use_action_fixtures
class TestActionTimeout:

    @classmethod
    def dir(cls):
        return 'queries/actions/timeout'

    def test_action_timeout_fail(self, hge_ctx):
        graphql_mutation = '''
        mutation {
          create_user(email: "random-email", name: "Clarke")
        }
        '''
        query = {
            'query': graphql_mutation,
            'variables': {}
        }
        status, resp, _ = hge_ctx.anyq('/v1/graphql', query, mk_headers_with_secret(hge_ctx))
        assert status == 200, resp
        assert 'data' in resp
        action_id = resp['data']['create_user']
        query_async = '''
        query ($action_id: uuid!){
          create_user(id: $action_id){
            id
            errors
          }
        }
        '''
        query = {
            'query': query_async,
            'variables': {
                'action_id': action_id
            }
        }
        conf = {
            'url': '/v1/graphql',
            'headers': {},
            'query': query,
            'status': 200,
        }
        # Since, the above is an async action, we don't wait for the execution of the webhook.
        # We need this sleep of 4 seconds here because only after 3 seconds (sleep duration in the handler)
        # we will be getting the result, otherwise the following asserts will fail because the
        # response will be empty. This 4 seconds sleep will be concurrent with the sleep duration
        # of the handler's execution. So, total time taken for this test will be 4 seconds.
        time.sleep(4)
        response, _ = check_query(hge_ctx, conf)

        assert 'errors' in response['data']['create_user']
        assert 'Response timeout' == response['data']['create_user']['errors']['internal']['error']['message']

        # tests that actions webhook url environment variable template did not serialize in the error message
        assert "{{ACTION_WEBHOOK_HANDLER}}/create-user-timeout" == response['data']['create_user']['errors']['internal']['request']['url']
