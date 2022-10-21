import pytest

from validate import check_query_f


@pytest.mark.usefixtures('webhook_server', 'per_class_tests_db_state')
@pytest.mark.admin_secret
@pytest.mark.tls_webhook_server
@pytest.mark.tls_insecure_certificate
class TestHTTPSWebhookInsecure:

    def test_user_select_unpublished_articles_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/user_select_query_unpublished_articles_fail.yaml')

    def test_user_only_other_users_published_articles_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/user_query_other_users_published_articles_fail.yaml')

    @classmethod
    def dir(cls):
        return 'webhook/insecure'
