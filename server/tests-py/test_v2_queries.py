from validate import check_query_f
import pytest

usefixtures = pytest.mark.usefixtures

# use_mutation_fixtures = usefixtures(
#     'per_class_db_schema_for_mutation_tests',
#     'per_method_db_data_for_mutation_tests'
# )

@usefixtures('per_class_tests_db_state')
class TestV2SelectBasic: # Basic RQL Tests on v2/query
    @classmethod
    def dir(cls):
        return 'queries/v2/basic'

    def test_select_query_author(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_article.yaml')

    def test_select_query_author_with_user_role_success(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/select_article_role_success.yaml')

    # TODO: Fix this test for JWT
    # def test_select_query_author_with_user_role_failure(self, hge_ctx):
    #     check_query_f(hge_ctx, self.dir() + '/select_article_role_error.yaml')
