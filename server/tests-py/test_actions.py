#!/usr/bin/env python3

import pytest
import time

from validate import check_query_f, check_query

"""
TODO:- Test Actions metadata
"""

use_action_fixtures = pytest.mark.usefixtures(
    "actions_fixture",
    'per_class_db_schema_for_mutation_tests',
    'per_method_db_data_for_mutation_tests'
)

@pytest.mark.parametrize("transport", ['http', 'websocket'])
@use_action_fixtures
class TestActionsSyncWebsocket:

    @classmethod
    def dir(cls):
        return 'queries/actions/sync'

    def test_create_user_fail(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/create_user_fail.yaml', transport)

    def test_create_user_success(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/create_user_success.yaml', transport)

    def test_create_users_fail(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/create_users_fail.yaml', transport)

    def test_create_users_success(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/create_users_success.yaml', transport)

@use_action_fixtures
class TestActionsSync:

    @classmethod
    def dir(cls):
        return 'queries/actions/sync'

    def test_invalid_webhook_response(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/invalid_webhook_response.yaml')

    def test_expecting_object_response(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/expecting_object_response.yaml')

    def test_expecting_array_response(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/expecting_array_response.yaml')

    # Webhook response validation tests. See https://github.com/hasura/graphql-engine/issues/3977
    def test_mirror_action_not_null(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/mirror_action_not_null.yaml')

    def test_mirror_action_unexpected_field(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/mirror_action_unexpected_field.yaml')

    def test_mirror_action_no_field(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/mirror_action_no_field.yaml')

    def test_mirror_action_success(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/mirror_action_success.yaml')

@use_action_fixtures
class TestQueryActions:

    @classmethod
    def dir(cls):
        return 'queries/actions/sync'

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
        for _ in range(100):
            self.test_query_action_success_output_object(hge_ctx)

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

@pytest.mark.usefixtures('per_class_tests_db_state')
class TestSetCustomTypes:

    @classmethod
    def dir(cls):
        return 'queries/actions/custom-types'

    def test_resuse_pgscalars(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/reuse_pgscalars.yaml')

    def test_resuse_unknown_pgscalar(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/reuse_unknown_pgscalar.yaml')

    def test_create_action_pg_scalar(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/create_action_pg_scalar.yaml')

@pytest.mark.usefixtures('per_class_tests_db_state')
class TestActionsMetadata:

    @classmethod
    def dir(cls):
        return 'queries/actions/metadata'

    def test_recreate_permission(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/recreate_permission.yaml')
