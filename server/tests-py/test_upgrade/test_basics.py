import ruamel.yaml as yaml
import pytest
from validate import check_query_f


@pytest.mark.parametrize("transport", ['http', 'websocket'])
class TestGraphQLQueryBasic():

    def test_select_query_author(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_author.yaml', transport)

    def test_select_query_author_with_skip_directive(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_author_skip_directive.yaml', transport)

    def test_select_query_author_with_include_directive(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_author_include_directive.yaml', transport)

    def test_select_various_postgres_types(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_test_types.yaml', transport)

    def test_select_query_author_quoted_col(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_author_col_quoted.yaml', transport)

    def test_select_query_author_pk(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_author_by_pkey.yaml', transport)

    def test_select_query_where(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_author_where.yaml', transport)

    def test_nested_select_query_article_author(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/nested_select_query_article_author.yaml', transport)

    def test_nested_select_query_deep(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/nested_select_query_deep.yaml', transport)

    def test_nested_select_query_where(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/nested_select_where_query_author_article.yaml', transport)

    def test_nested_select_query_where_on_relationship(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/nested_select_query_article_author_where_on_relationship.yaml', transport)

    def test_select_query_user(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/select_query_user.yaml", transport)

    def test_select_query_non_tracked_table(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/select_query_non_tracked_table_err.yaml", transport)

    def test_select_query_col_not_present_err(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/select_query_author_col_not_present_err.yaml", transport)

    def test_select_query_user_col_change(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/select_query_user_col_change.yaml")

    def test_nested_select_with_foreign_key_alter(self, hge_ctx, transport):
        transport = 'http'
        check_query_f(hge_ctx, self.dir() + "/nested_select_with_foreign_key_alter.yaml", transport)

    def test_select_query_invalid_escape_sequence(self, hge_ctx, transport):
        transport = 'http'
        check_query_f(hge_ctx, self.dir() + "/select_query_invalid_escape_sequence.yaml", transport)

    def test_select_query_batching(self, hge_ctx, transport):
        transport = 'http'
        check_query_f(hge_ctx, self.dir() + "/select_query_batching.yaml", transport)

    def test_select_query_batching_with_mutation(self, hge_ctx, transport):
        transport = 'http'
        check_query_f(hge_ctx, self.dir() + "/select_query_batching_with_mutation.yaml", transport)

    def test_select_query_batching_with_one_error(self, hge_ctx, transport):
        transport = 'http'
        check_query_f(hge_ctx, self.dir() + "/select_query_batching_with_one_error.yaml", transport)

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/basic'
