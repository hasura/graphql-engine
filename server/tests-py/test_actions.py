#!/usr/bin/env python3

import pytest
import time

from validate import check_query_f, check_query
from super_classes import DefaultTestQueries

"""
TODO:- Test Actions metadata
"""

class TestActionsSync(DefaultTestQueries):

    @classmethod
    def dir(cls):
        return 'queries/actions/sync'

    def test_create_user_fail(self, hge_ctx, actions_webhook):
        check_query_f(hge_ctx, self.dir() + '/create_user_fail.yaml')

    def test_create_user_success(self, hge_ctx, actions_webhook):
        check_query_f(hge_ctx, self.dir() + '/create_user_success.yaml')

class TestActionsAsync(DefaultTestQueries):
    @classmethod
    def dir(cls):
        return 'queries/actions/async'

    def test_create_user_fail(self, hge_ctx, actions_webhook):
        graphql_mutation = '''
        mutation {
          create_user(email: "random-email", name: "Clarke")
        }
        '''
        query = {
            'query': graphql_mutation,
            'variables': {}
        }
        status, resp, headers = hge_ctx.anyq('/v1/graphql', query, {})
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

    def test_create_user_success(self, hge_ctx, actions_webhook):
        graphql_mutation = '''
        mutation {
          create_user(email: "clarke@hasura.io", name: "Clarke")
        }
        '''
        query = {
            'query': graphql_mutation,
            'variables': {}
        }
        status, resp, headers = hge_ctx.anyq('/v1/graphql', query, {})
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

    def test_create_user_roles(self, hge_ctx, actions_webhook):
        graphql_mutation = '''
        mutation {
          create_user(email: "blake@hasura.io", name: "Blake")
        }
        '''
        query = {
            'query': graphql_mutation,
            'variables': {}
        }
        headers = {
            'X-Hasura-Role': 'user',
            'X-Hasura-User-Id': '1'
        }
        # create action with user-id 1
        status, resp, headers = hge_ctx.anyq('/v1/graphql', query, headers)
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

        conf_user_2 = {
            'url': '/v1/graphql',
            'headers': {
                'X-Hasura-Role': 'user',
                'X-Hasura-User-Id': '2'
            },
            'query': query,
            'status': 200,
            'response': {
                'data': {
                    'create_user': None # User 2 shouldn't able to access the action
                }
            }
        }
        # Query the action as user-id 2
        check_query(hge_ctx, conf_user_2)

        conf_user_1 = {
            'url': '/v1/graphql',
            'headers': {
                'X-Hasura-Role': 'user',
                'X-Hasura-User-Id': '1'
            },
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
        check_query(hge_ctx, conf_user_1)
