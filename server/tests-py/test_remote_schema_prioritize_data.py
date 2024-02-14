#!/usr/bin/env python3

import pytest

from conftest import extract_server_address_from
from remote_server import NodeGraphQL
from validate import check_query_f

pytestmark = [
    pytest.mark.admin_secret,
    pytest.mark.hge_env('HASURA_GRAPHQL_REMOTE_SCHEMA_PRIORITIZE_DATA', 'true'),
]

@pytest.fixture(scope='class')
@pytest.mark.early
def fake_graphql_service(worker_id: str, hge_fixture_env: dict[str, str]):
    (_, port) = extract_server_address_from('GRAPHQL_SERVICE')
    server = NodeGraphQL(worker_id, 'remote_schemas/nodejs/returns_data_and_errors.js', port=port)
    server.start()
    print(f'{fake_graphql_service.__name__} server started on {server.url}')
    hge_fixture_env['GRAPHQL_SERVICE'] = server.url
    yield server
    server.stop()

use_test_fixtures = pytest.mark.usefixtures(
    'fake_graphql_service',
    'per_method_tests_db_state',
)

@use_test_fixtures
class TestRemoteSchemaPrioritizeData:

    @classmethod
    def dir(cls):
        return "queries/remote_schemas/validate_data_errors_prioritization/"

    def test_data_only_query(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'data_prioritization/test_data_only_query.yaml')

    def test_error_only_query(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'data_prioritization/test_error_only_query.yaml')

    def test_data_and_errors_query(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'data_prioritization/test_data_and_errors_query.yaml')
