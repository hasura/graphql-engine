import pytest
import yaml
from validate import check_query_f

if not pytest.config.getoption("--test-webhook-insecure"):
    pytest.skip("--test-webhook-https-insecure flag is missing, skipping tests", allow_module_level=True)

class TestHTTPSWebhookInsecure:

    def test_user_select_unpublished_articles_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/user_select_query_unpublished_articles_fail.yaml')

    def test_user_only_other_users_published_articles_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/user_query_other_users_published_articles_fail.yaml')

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        self.dir = 'webhook/insecure'
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp
