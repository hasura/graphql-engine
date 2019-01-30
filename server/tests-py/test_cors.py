import pytest

if not pytest.config.getoption("--test-cors"):
    pytest.skip("--test-cors flag is missing, skipping tests", allow_module_level=True)


def url(hge_ctx):
    return hge_ctx.hge_url + '/v1/version'

class TestCors():
    def assert_cors_headers(self, origin, resp):
        headers = resp.headers
        assert 'Access-Control-Allow-Origin' in headers
        assert headers['Access-Control-Allow-Origin'] == origin
        assert 'Access-Control-Allow-Credentials' in headers
        assert headers['Access-Control-Allow-Credentials'] == 'true'
        assert 'Access-Control-Allow-Methods' in headers
        assert headers['Access-Control-Allow-Methods'] == 'GET,POST,PUT,PATCH,DELETE,OPTIONS'

    def test_cors_foo_bar_top_domain(self, hge_ctx):
        resp = hge_ctx.http.get(url(hge_ctx), headers={'Origin': 'foo.bar.com'})
        self.assert_cors_headers('foo.bar.com', resp)

    def test_cors_foo_bar_sub_domain(self, hge_ctx):
        resp = hge_ctx.http.get(url(hge_ctx), headers={'Origin': 'app.foo.bar.com'})
        self.assert_cors_headers('app.foo.bar.com', resp)
    def test_cors_foo_bar_sub_sub_domain_fails(self, hge_ctx):
        resp = hge_ctx.http.get(url(hge_ctx), headers={'Origin': 'inst1.app.foo.bar.com'})
        with pytest.raises(AssertionError):
            self.assert_cors_headers('inst1.app.foo.bar.com', resp)

    def test_cors_localhost_domain(self, hge_ctx):
        resp = hge_ctx.http.get(url(hge_ctx), headers={'Origin': 'localhost'})
        self.assert_cors_headers('localhost', resp)

    def test_cors_wrong_domain(self, hge_ctx):
        resp = hge_ctx.http.get(url(hge_ctx), headers={'Origin': 'example.com'})
        assert 'Access-Control-Allow-Origin' not in resp.headers
