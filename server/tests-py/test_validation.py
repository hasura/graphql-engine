import pytest
import yaml
from validate import check_query_f
from super_classes import GraphQLEngineTest

# @pytest.mark.parametrize("transport", ['http', 'websocket'])
# graphql parser can't seem to parse {where: null}, disabling
# websocket till then
@pytest.mark.parametrize("transport", ['http'])
@pytest.mark.parametrize('endpoint', ['/v1alpha1/graphql', '/v1/graphql'])
class TestGraphQLValidation(GraphQLEngineTest):

    def test_null_value(self, hge_ctx, transport, endpoint):
        check_query_f(hge_ctx, self.dir() + "/null_value_err.yaml", transport, endpoint)

    def test_null_variable_value(self, hge_ctx, transport, endpoint):
        check_query_f(hge_ctx, self.dir() + "/null_variable_value_err.yaml", transport, endpoint)

    @classmethod
    def dir(cls):
        return "queries/graphql_validation"
