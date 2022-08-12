import requests
import pytest
from context import PytestConf

"""

NOTE:
    These endpoints are admin-only

    "dev/ekg"
    "dev/plan_cache"
    "dev/subscriptions"
    "dev/subscriptions/extended"

    This needs RTS to be enabled and mainly used for benchmarking:
    (hence not adding any tests for this)
    "dev/rts_stats" - has no "admin" role requirements

"""

def get_headers(hge_ctx, role='admin'):
    headers = {}
    if hge_ctx.hge_key != None:
        headers['x-hasura-admin-secret'] = hge_ctx.hge_key
    headers['x-hasura-role'] = role
    return headers

developer_api_enabled = PytestConf.config.getoption("--test-developer-api-enabled")
@pytest.mark.skipif(not developer_api_enabled,
                    reason="flag --test-developer-api-enabled is not set. Cannot run tests for metadata disabled")
class TestDevEndpoints:

    def test_ekg_endpoint_admin_role(self, hge_ctx):
        resp = requests.get(hge_ctx.hge_url + '/dev/ekg', headers=get_headers(hge_ctx))
        assert resp.status_code == 200

    def test_ekg_endpoint_user_role(self, hge_ctx):
        resp = requests.get(hge_ctx.hge_url + '/dev/ekg', headers=get_headers(hge_ctx, 'user'))
        assert resp.status_code == 400

    def test_plan_cache_endpoint_admin_role(self, hge_ctx):
        resp = requests.get(hge_ctx.hge_url + '/dev/plan_cache', headers=get_headers(hge_ctx))
        assert resp.status_code == 200

    def test_plan_cache_endpoint_user_role(self, hge_ctx):
        resp = requests.get(hge_ctx.hge_url + '/dev/plan_cache', headers=get_headers(hge_ctx, 'user'))
        assert resp.status_code == 400
    
    def test_subscriptions_endpoint_admin_role(self, hge_ctx):
        resp = requests.get(hge_ctx.hge_url + '/dev/subscriptions', headers=get_headers(hge_ctx))
        assert resp.status_code == 200

    def test_subscriptions_endpoint_user_role(self, hge_ctx):
        resp = requests.get(hge_ctx.hge_url + '/dev/subscriptions', headers=get_headers(hge_ctx, 'user'))
        assert resp.status_code == 400
    
    def test_subscriptions_extended_endpoint_admin_role(self, hge_ctx):
        resp = requests.get(hge_ctx.hge_url + '/dev/subscriptions/extended', headers=get_headers(hge_ctx))
        assert resp.status_code == 200

    def test_subscriptions_extended_endpoint_user_role(self, hge_ctx):
        resp = requests.get(hge_ctx.hge_url + '/dev/subscriptions/extended', headers=get_headers(hge_ctx, 'user'))
        assert resp.status_code == 400
