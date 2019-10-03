import pytest
from validate import check_query_f
from super_classes import DefaultTestSelectQueries

if not pytest.config.getoption("--test-webhook-insecure"):
    pytest.skip("--test-webhook-https-insecure flag is missing, skipping tests", allow_module_level=True)

class TestHTTPSWebhookInsecure(DefaultTestSelectQueries):

    def test_user_select_unpublished_articles_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/user_select_query_unpublished_articles_fail.yaml')

    def test_user_only_other_users_published_articles_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/user_query_other_users_published_articles_fail.yaml')

    @classmethod
    def dir(cls):
        return 'webhook/insecure'
