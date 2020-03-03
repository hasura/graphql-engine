#!/usr/bin/env python3

import pytest
import time

from validate import check_query_f, check_query

"""
TODO:- Test Actions metadata
"""

use_action_fixtures = pytest.mark.usefixtures(
    "actions_webhook",
    'per_class_db_schema_for_mutation_tests',
    'per_method_db_data_for_mutation_tests'
)

@pytest.mark.parametrize("transport", ['http', 'websocket'])
@use_action_fixtures
class TestActionsSync:

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

    def test_invalid_webhook_response(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/invalid_webhook_response.yaml')

    def test_expecting_object_response(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/expecting_object_response.yaml')

    def test_expecting_array_response(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/expecting_array_response.yaml')

@use_action_fixtures
class TestActionsAsync:
    @classmethod
    def dir(cls):
        return 'queries/actions/async'

    def mk_headers_with_secret(self, hge_ctx, headers={}):
        admin_secret = hge_ctx.hge_key
        if admin_secret:
            headers['X-Hasura-Admin-Secret'] = admin_secret
        return headers


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
        status, resp, _ = hge_ctx.anyq('/v1/graphql', query, self.mk_headers_with_secret(hge_ctx))
        assert status == 200, resp
        assert 'data' in resp
        action_id = resp['data']['create_user']
        time.sleep(2)

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
        check_query(hge_ctx, conf)

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
        status, resp, _ = hge_ctx.anyq('/v1/graphql', query, self.mk_headers_with_secret(hge_ctx))
        assert status == 200, resp
        assert 'data' in resp
        action_id = resp['data']['create_user']
        time.sleep(2)

        query_async = '''
        query ($action_id: uuid!){
          create_user(id: $action_id){
            id
            output {
              id
              user {
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
                    'id': action_id,
                    'output': {
                        'id': 1,
                        'user': {
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
        check_query(hge_ctx, conf)

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
        headers_user_1 = self.mk_headers_with_secret(hge_ctx, {
            'X-Hasura-Role': 'user',
            'X-Hasura-User-Id': '1'
        })
        # create action with user-id 1
        status, resp, headers = hge_ctx.anyq('/v1/graphql', query, headers_user_1)
        assert status == 200, resp
        assert 'data' in resp
        action_id = resp['data']['create_user']
        time.sleep(2)

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

        headers_user_2 = self.mk_headers_with_secret(hge_ctx, {
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
        check_query(hge_ctx, conf_user_2, add_auth = False)

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
        check_query(hge_ctx, conf_user_1, add_auth = False)
