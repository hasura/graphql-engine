#!/usr/bin/env python3

import pytest
import subprocess
import time

from validate import check_query_f, check_query
from remote_server import NodeGraphQL

@pytest.fixture(scope="module")
def graphql_service():
    svc = NodeGraphQL(["node", "remote_schemas/nodejs/index.js"])
    svc.start()
    yield svc
    svc.stop()

fixtures = pytest.mark.usefixtures(
    'per_class_db_schema_for_mutation_tests',
    'per_method_db_data_for_mutation_tests'
)

@pytest.mark.parametrize('transport', ['http', 'websocket'])
@fixtures
class TestHeterogeneousQuery:
    @classmethod
    def dir(cls):
        return 'queries/heterogeneous'

    def test_basic(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/basic.yaml', transport)
