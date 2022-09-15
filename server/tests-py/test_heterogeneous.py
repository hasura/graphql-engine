#!/usr/bin/env python3

import pytest

from validate import check_query_f

@pytest.mark.parametrize('transport', ['http', 'websocket'])
@pytest.mark.usefixtures(
    'gql_server',
    'per_class_db_schema_for_mutation_tests',
    'per_method_db_data_for_mutation_tests',
)
class TestHeterogeneousQuery:
    @classmethod
    def dir(cls):
        return 'queries/heterogeneous'

    def test_basic(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/basic.yaml', transport)
