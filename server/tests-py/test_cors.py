import pytest
from context import PytestConf

if not PytestConf.config.getoption("--test-cors"):
    pytest.skip("--test-cors flag is missing, skipping tests", allow_module_level=True)


def url(hge_ctx):
    return hge_ctx.hge_url + '/v1/version'

class TestCors():
    """
    currently assumes the following is set:
    HASURA_GRAPHQL_CORS_DOMAIN="http://*.localhost, http://localhost:3000, https://*.foo.bar.com"
    """
    def assert_cors_headers(self, origin, resp):
        headers = resp.headers
        assert 'Access-Control-Allow-Origin' in headers
        assert headers['Access-Control-Allow-Origin'] == origin
        assert 'Access-Control-Allow-Credentials' in headers
        assert headers['Access-Control-Allow-Credentials'] == 'true'
        assert 'Access-Control-Allow-Methods' in headers
        assert headers['Access-Control-Allow-Methods'] == 'GET,POST,PUT,PATCH,DELETE,OPTIONS'

    def test_cors_foo_bar_top_domain(self, hge_ctx):
        origin = 'https://foo.bar.com'
        resp = hge_ctx.http.get(url(hge_ctx), headers={'Origin': origin})
        with pytest.raises(AssertionError):
            self.assert_cors_headers(origin, resp)

    def test_cors_foo_bar_sub_domain(self, hge_ctx):
        origin = 'https://app.foo.bar.com'
        resp = hge_ctx.http.get(url(hge_ctx), headers={'Origin': origin})
        self.assert_cors_headers(origin, resp)

    def test_cors_foo_bar_sub_sub_domain_fails(self, hge_ctx):
        origin = 'https://inst1.app.foo.bar.com'
        resp = hge_ctx.http.get(url(hge_ctx), headers={'Origin': origin})
        with pytest.raises(AssertionError):
            self.assert_cors_headers(origin, resp)

    def test_cors_localhost_domain_w_port(self, hge_ctx):
        origin = 'http://localhost:3000'
        resp = hge_ctx.http.get(url(hge_ctx), headers={'Origin': origin})
        self.assert_cors_headers(origin, resp)

    def test_cors_localhost_domain(self, hge_ctx):
        origin = 'http://app.localhost'
        resp = hge_ctx.http.get(url(hge_ctx), headers={'Origin': origin})
        self.assert_cors_headers(origin, resp)

    def test_cors_wrong_domain(self, hge_ctx):
        origin = 'https://example.com'
        resp = hge_ctx.http.get(url(hge_ctx), headers={'Origin': origin})
        assert 'Access-Control-Allow-Origin' not in resp.headers
