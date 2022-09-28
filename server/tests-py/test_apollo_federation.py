#!/usr/bin/env python3

import pytest
import requests
from remote_server import NodeGraphQL
from validate import check_query_f


def make_request(url, query):
    print('Sending request to the local federated server')
    payload = {'query': query}
    resp = requests.post(url, json=payload)
    return resp

@pytest.mark.hge_env('HASURA_GRAPHQL_EXPERIMENTAL_FEATURES', 'apollo_federation')
@pytest.mark.usefixtures('per_class_tests_db_state')
class TestApolloFederation:

    @classmethod
    def dir(cls):
        return 'queries/apollo_federation'

    def test_apollo_federated_server_with_hge_only(self, hge_url: str):
        # start the node server
        fed_server = NodeGraphQL(["node", "remote_schemas/nodejs/apollo_federated_server_with_hge_only.js"], env={
            'HGE_URL': hge_url,
        })
        fed_server.start()

        # run a GQL query
        gql_query = """
            query {
                user_by_pk(id: 1) {
                    id
                    name
                }
            }
            """
        resp = make_request(fed_server.url, gql_query)

        # stop the node server
        fed_server.stop()

        # check if everything was okay
        assert resp.status_code == 200, resp.text
        assert 'data' in resp.text

    def test_apollo_federated_server_with_hge_and_apollo_graphql_server(self, hge_url: str):
        # start the node servers
        server_1 = NodeGraphQL(["node", "remote_schemas/nodejs/apollo_server_1.js"], env={
            'HGE_URL': hge_url,
        })
        server_env = {
            'HGE_URL': hge_url,
            'OTHER_URL': server_1.url,
        }
        fed_server = NodeGraphQL(["node", "remote_schemas/nodejs/apollo_federated_server_with_hge_and_server1.js"], env=server_env)

        server_1.start()
        fed_server.start()

        # run a GQL query
        gql_query = """
            query {
                getUserData(id: 1) {
                    id
                    name
                    city
                    email
                }
            }
            """
        resp = make_request(fed_server.url, gql_query)

        # stop the node servers
        fed_server.stop()
        server_1.stop()

        # check if everything was okay
        assert resp.status_code == 200, resp.text
        assert 'data' in resp.text

    def test_apollo_federation_fields(self,hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/root_fields.yaml')

    def test_apollo_federation_entities(self,hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/entities.yaml')
