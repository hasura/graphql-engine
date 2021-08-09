
import pytest

from validate import check_query_f, check_query
from remote_server import NodeGraphQL
from context import PytestConf
from conftest import use_action_fixtures, use_function_permission_fixtures


if not PytestConf.config.getoption('--test-inherited-roles'):
    pytest.skip('--test-inherited-roles is missing, skipping role inheritance tests', allow_module_level=True)

if not PytestConf.config.getoption('--enable-remote-schema-permissions'):
    pytest.skip('--enable-remote-schema-permissions is missing, skipping role inheritance tests', allow_module_level=True)

@pytest.fixture(scope="module")
def graphql_service():
    svc = NodeGraphQL(["node", "remote_schemas/nodejs/remote_schema_perms.js"])
    svc.start()
    yield svc
    svc.stop()

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

use_remote_schema_permissions_inheritance_fixtures = pytest.mark.usefixtures (
    "graphql_service",
    "per_class_tests_db_state"
)

@use_remote_schema_permissions_inheritance_fixtures
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

@use_function_permission_fixtures
class TestCustomFunctionPermissionsInheritance:

    @classmethod
    def dir(cls):
        return "queries/graphql_mutation/functions/permissions/roles_inheritance/"

    setup_metadata_api_version = "2"

    def test_inheritance_from_multiple_parents(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'multiple_parents_inheritance.yaml')

    def test_override_inherited_permission(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + 'override_inherited_permission.yaml')
