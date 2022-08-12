import pytest
from validate import check_query_f
from context import PytestConf

if not PytestConf.config.getoption("--test-webhook-insecure"):
    pytest.skip("--test-webhook-https-insecure flag is missing, skipping tests", allow_module_level=True)

@pytest.mark.usefixtures('per_class_tests_db_state')
class TestHTTPSWebhookInsecure:

    def test_user_select_unpublished_articles_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/user_select_query_unpublished_articles_fail.yaml')

    def test_user_only_other_users_published_articles_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/user_query_other_users_published_articles_fail.yaml')

    @classmethod
    def dir(cls):
        return 'webhook/insecure'
