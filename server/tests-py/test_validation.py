import pytest
from validate import check_query_f

# @pytest.mark.parametrize("transport", ['http', 'websocket'])
# graphql parser can't seem to parse {where: null}, disabling
# websocket till then
@pytest.mark.parametrize("transport", ['http'])
@pytest.mark.usefixtures('per_method_tests_db_state')
class TestGraphQLValidation:

    def test_null_value(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/null_value_err.yaml", transport)

    def test_null_variable_value(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/null_variable_value_err.yaml", transport)

    def test_variable_type_mismatch(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/variable_type_mismatch.yaml", transport)

    def test_json_column_value(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + "/json_column_value.yaml", transport)

    @classmethod
    def dir(cls):
        return "queries/graphql_validation"
