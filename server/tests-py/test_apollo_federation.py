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

@pytest.mark.usefixtures('per_class_tests_db_state')
@pytest.mark.admin_secret
@pytest.mark.hge_env('HASURA_GRAPHQL_EXPERIMENTAL_FEATURES', 'apollo_federation')
class TestApolloFederation:

    @classmethod
    def dir(cls):
        return 'queries/apollo_federation'

    @pytest.fixture
    def federated_server_with_hge_only(self, worker_id: str, hge_url: str, hge_key: str):
        server = NodeGraphQL(worker_id, 'remote_schemas/nodejs/apollo_federated_server_with_hge_only.js', env={
            'HGE_URL': hge_url,
            'HASURA_GRAPHQL_ADMIN_SECRET': hge_key,
        })
        server.start()
        yield server
        server.stop()

    @pytest.fixture
    def server_1(self, worker_id: str, hge_url: str):
        server = NodeGraphQL(worker_id, 'remote_schemas/nodejs/apollo_server_1.js', env={
            'HGE_URL': hge_url,
        })
        server.start()
        yield server
        server.stop()

    @pytest.fixture
    def federated_server_with_hge_and_server1(self, worker_id: str, hge_url: str, hge_key: str, server_1):
        server = NodeGraphQL(worker_id, 'remote_schemas/nodejs/apollo_federated_server_with_hge_and_server1.js', env={
            'HGE_URL': hge_url,
            'OTHER_URL': server_1.url,
            'HASURA_GRAPHQL_ADMIN_SECRET': hge_key,
        })
        server.start()
        yield server
        server.stop()

    def test_apollo_federated_server_with_hge_only(self, federated_server_with_hge_only):
        # run a GQL query
        gql_query = """
            query {
                user_by_pk(id: 1) {
                    id
                    name
                }
            }
            """
        resp = make_request(federated_server_with_hge_only.url, gql_query)

        # check if everything was okay
        assert resp.status_code == 200, resp.text
        assert 'data' in resp.text

    def test_apollo_federated_server_with_hge_and_apollo_graphql_server(self, federated_server_with_hge_and_server1):
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
        resp = make_request(federated_server_with_hge_and_server1.url, gql_query)

        # check if everything was okay
        assert resp.status_code == 200, resp.text
        assert 'data' in resp.text

    def test_apollo_federation_fields(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/root_fields.yaml')

    def test_apollo_federation_entities(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/entities.yaml')
