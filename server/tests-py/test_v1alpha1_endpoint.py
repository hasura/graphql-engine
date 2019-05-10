import yaml
import pytest
from validate import check_query
from super_classes import DefaultTestSelectQueries
from context import GQLWsClient


class TestV1Alpha1GraphQLErrors(DefaultTestSelectQueries):

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/v1alpha1/errors'

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
