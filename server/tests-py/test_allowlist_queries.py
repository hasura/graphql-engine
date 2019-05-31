#!/usrbin/env python3

import pytest
from validate import check_query_f
from super_classes import DefaultTestSelectQueries

@pytest.mark.skipif(not pytest.config.getoption("--test-allowlist-queries"),
                    reason="flag --test-allowlist-queries is not set. Cannot runt tests for allowlist queries")
@pytest.mark.parametrize("transport", ['http','websocket'])
class TestAllowlistQueries(DefaultTestSelectQueries):

    def test_query_user(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_user.yaml', transport)

    def test_query_user_by_pk(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_user_by_pk.yaml', transport)

    def test_query_user_with_typename(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_user_with_typename.yaml', transport)

    def test_query_non_allowlist(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_non_allowlist.yaml', transport)

    def test_query_as_admin(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_as_admin.yaml', transport)

    def test_update_query(self, hge_ctx, transport):
        # test only for http
        transport = 'http'
        check_query_f(hge_ctx, self.dir() + '/update_query.yaml', transport)

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/allowlist'
