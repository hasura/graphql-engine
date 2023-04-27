import pytest

from conftest import extract_server_address_from, use_action_fixtures
from remote_server import NodeGraphQL
from validate import check_query_f

pytestmark = [
    pytest.mark.admin_secret,
    pytest.mark.hge_env('HASURA_GRAPHQL_ENABLE_REMOTE_SCHEMA_PERMISSIONS', 'true'),
]

@pytest.fixture(scope='class')
@pytest.mark.early
def graphql_service(worker_id: str, hge_fixture_env: dict[str, str]):
    (_, port) = extract_server_address_from('GRAPHQL_SERVICE_1')
    server = NodeGraphQL(worker_id, 'remote_schemas/nodejs/remote_schema_perms.js', port=port)
    server.start()
    print(f'{graphql_service.__name__} server started on {server.url}')
    hge_fixture_env['GRAPHQL_SERVICE_1'] = server.url
    yield server
    server.stop()

@pytest.mark.usefixtures('per_class_db_schema_for_mutation_tests', 'per_method_db_data_for_mutation_tests')
class TestGraphQLMutationRolesInheritance:

    @classmethod
    def dir(cls):
        return 'queries/graphql_mutation/roles_inheritance/'

    setup_metadata_api_version = "v2"

    def test_inheritance_from_single_parent(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'inheritance_from_single_parent.yaml')

    def test_inheritance_when_mutation_permissions_conflict(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'resolve_inconsistent_permission.yaml')

    def test_mutation_permission_inheritance_for_nested_roles(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'inherited_mutation_permission_for_nested_roles.yaml')

    def test_defined_permission_should_override_inherited_permission(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'override_inherited_permission.yaml')

@pytest.mark.usefixtures('graphql_service', 'per_class_tests_db_state')
class TestRemoteSchemaPermissionsInheritance:

    @classmethod
    def dir(cls):
        return "queries/remote_schemas/permissions/inheritance/"

    setup_metadata_api_version = "v2"

    def test_inheritance_from_multiple_parents_having_no_conflicts(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'multiple_parents_inheritance.yaml')

    def test_conflicting_parent_permissions(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'conflicting_parent_permissions.yaml')

    def test_override_inherited_permission(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'override_inherited_permission.yaml')

@use_action_fixtures
class TestActionsPermissionInheritance:

    @classmethod
    def dir(cls):
        return "queries/actions/roles_inheritance/"

    setup_metadata_api_version = "2"

    def test_inheritance_from_multiple_parents(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'multiple_parents_inheritance.yaml')

    def test_override_inherited_permission(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'override_inherited_permission.yaml')

@pytest.mark.usefixtures('per_class_db_schema_for_mutation_tests', 'per_method_db_data_for_mutation_tests')
@pytest.mark.hge_env('HASURA_GRAPHQL_INFER_FUNCTION_PERMISSIONS', 'false')
class TestCustomFunctionPermissionsInheritance:

    @classmethod
    def dir(cls):
        return "queries/graphql_mutation/functions/permissions/roles_inheritance/"

    setup_metadata_api_version = "2"

    def test_inheritance_from_multiple_parents(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'multiple_parents_inheritance.yaml')

    def test_override_inherited_permission(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'override_inherited_permission.yaml')
