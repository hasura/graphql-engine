import pytest
from validate import check_query
import validate
from context import GQLWsClient

usefixtures = pytest.mark.usefixtures

@usefixtures('per_class_tests_db_state')
class TestV1Alpha1GraphQLErrors:

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/v1alpha1/errors'

    def test_v1alpha1_authorization_error(self, hge_ctx):
        gql_query = """
          query {
            author {
              id
              name
            }
          }
        """
        http_conf = {
            'url': '/v1alpha1/graphql',
            'status': 200,
            'query': {'query': gql_query},
        }

        if hge_ctx.hge_key is not None and hge_ctx.hge_webhook is None and hge_ctx.hge_jwt_key is None:
            # Test whether it is forbidden when incorrect/no admin_secret is specified
            validate.test_forbidden_when_admin_secret_reqd(hge_ctx, http_conf)

        elif hge_ctx.hge_webhook is not None:
            if not hge_ctx.webhook_insecure:
            # Check whether the output is also forbidden when webhook returns forbidden
                validate.test_forbidden_webhook(hge_ctx, http_conf)
        else:
            assert True


    @pytest.mark.parametrize('transport', ['http', 'websocket'])
    def test_v1alpha1_validation_error(self, hge_ctx, transport):
        gql_query = """
          query {
            author {
              id
              name
              notPresentCol
            }
          }
        """
        http_conf = {
            'url': '/v1alpha1/graphql',
            'status': 400,
            'query': {'query': gql_query},
            'response': {
                "errors": [{
                    "extensions": {
                        "path": "$.selectionSet.author.selectionSet.notPresentCol",
                        "code": "validation-failed"
                    },
                    "message": "field \"notPresentCol\" not found in type: 'author'"
                }]
            }
        }
        ws_conf = {
            'url': '/v1alpha1/graphql',
            'query': {'query': gql_query},
            'response': {
                "path": "$.selectionSet.author.selectionSet.notPresentCol",
                "code": "validation-failed",
                "error": "field \"notPresentCol\" not found in type: 'author'"
            }
        }

        if transport == 'http':
            check_query(hge_ctx, http_conf, transport)
        elif transport == 'websocket':
            check_query(hge_ctx, ws_conf, transport)

    @pytest.mark.parametrize('transport', ['http', 'websocket'])
    def test_v1alpha1_execution_error(self, hge_ctx, transport):
        mutation = """
        mutation {
          insert_article (objects: [
            {
              title: "test 3"
              content: "test 3 content"
              author_id: 44
              is_published: false
            }
          ]) {
            returning {
              id
            }
          }
        }
        """
        http_conf = {
            'url': '/v1alpha1/graphql',
            'status': 400,
            'query': {'query': mutation},
            'response': {
                "errors": [{
                    "extensions": {
                        "path": "$.selectionSet.insert_article.args.objects",
                        "code": "constraint-violation"
                    },
                    "message": "Foreign key violation. insert or update on table \"article\" violates foreign key constraint \"article_author_id_fkey\""
                }]
            }

        }

        ws_conf = {
            'url': '/v1alpha1/graphql',
            'query': {'query': mutation},
            'response': {
                'data': None,
                "errors": [{
                    "path": "$.selectionSet.insert_article.args.objects",
                    "error": "Foreign key violation. insert or update on table \"article\" violates foreign key constraint \"article_author_id_fkey\"",
                    "code": "constraint-violation"
                }]
            }
        }

        if transport == 'http':
            check_query(hge_ctx, http_conf, transport)
        elif transport == 'websocket':
            check_query(hge_ctx, ws_conf, transport)


    def test_v1alpha1_ws_start_error(self, hge_ctx):
        ws_client = GQLWsClient(hge_ctx, '/v1alpha1/graphql')
        query = {'query': '{ author { name } }'}
        frame = {
            'id': '1',
            'type': 'start',
            'payload': query
        }
        ws_client.ws_active_query_ids.add('1')
        ws_client.send(frame)
        resp = ws_client.get_ws_query_event('1', 10)
        print(resp)
        assert 'type' in resp
        assert resp['type'] == 'error'
        assert 'payload' in resp
        assert resp['payload'] == {
            'path': '$',
            'error': 'start received before the connection is initialised',
            'code': 'start-failed'
        }
