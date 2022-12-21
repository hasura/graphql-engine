import pytest

from validate import check_query_f

@pytest.mark.hge_env('HASURA_GRAPHQL_EXPERIMENTAL_FEATURES', 'naming_convention')
class TestNamingConventions:

    @classmethod
    def dir(cls):
        return "queries/naming_conventions"

    @pytest.fixture(scope='class', autouse=True)
    def add_customized_source(self, add_source):
        add_source('pg1', customization={'naming_convention': 'graphql-default'})

    def test_field_name_precedence(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/field_name_precedence.yaml')

    def test_enum_value_convention(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/enum_value_convention.yaml')

@pytest.mark.hge_env('HASURA_GRAPHQL_EXPERIMENTAL_FEATURES', 'naming_convention')
class TestNamingConventionsTypeAndFieldNamesGraphqlDefault:

    @classmethod
    def dir(cls):
        return "queries/naming_conventions"

    @pytest.fixture(scope='class', autouse=True)
    def add_customized_source(self, add_source):
        add_source('pg1', customization={'naming_convention': 'graphql-default'})

    def test_type_and_field_names(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/type_and_field_names_graphql_default.yaml')

@pytest.mark.hge_env('HASURA_GRAPHQL_EXPERIMENTAL_FEATURES', 'naming_convention')
class TestNamingConventionsTypeAndFieldNamesHasuraDefault:

    @classmethod
    def dir(cls):
        return "queries/naming_conventions"

    @pytest.fixture(scope='class', autouse=True)
    def add_customized_source(self, add_source):
        add_source('pg1', customization={'naming_convention': 'hasura-default'})

    def test_type_and_field_names(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/type_and_field_names_hasura_default.yaml')

@pytest.mark.hge_env('HASURA_GRAPHQL_EXPERIMENTAL_FEATURES', 'naming_convention')
class TestNamingConventionsTypeAndFieldNamesGraphqlDefaultWithPrefixAndSuffix:

    @classmethod
    def dir(cls):
        return "queries/naming_conventions"

    @pytest.fixture(scope='class', autouse=True)
    def add_customized_source(self, add_source):
        add_source('pg1', customization={
            'naming_convention': 'graphql-default',
            'root_fields': {
                'prefix': 'test',
                'suffix': 'query',
            },
        })

    def test_type_and_field_names_with_prefix_and_suffix(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/type_and_field_names_graphql_default_with_prefix_and_suffix.yaml')

@pytest.mark.hge_env('HASURA_GRAPHQL_EXPERIMENTAL_FEATURES', 'naming_convention')
class TestNamingConventionsTypeAndFieldNamesHasuraDefaultWithPrefixAndSuffix:

    @classmethod
    def dir(cls):
        return "queries/naming_conventions"

    @pytest.fixture(scope='class', autouse=True)
    def add_customized_source(self, add_source):
        add_source('pg1', customization={
            'naming_convention': 'hasura-default',
            'root_fields': {
                'prefix': 'test_',
                'suffix': '_query',
            },
        })

    def test_type_and_field_names_with_prefix_and_suffix(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/type_and_field_names_hasura_default_with_prefix_and_suffix.yaml')

@pytest.mark.backend('mssql')
@pytest.mark.hge_env('HASURA_GRAPHQL_EXPERIMENTAL_FEATURES', 'naming_convention')
class TestNamingConventionsFailure:
    @classmethod
    def dir(cls):
        return "queries/naming_conventions"

    def test_other_than_pg_db_failure(self, hge_ctx):
        response = hge_ctx.v1metadataq(
            q={
                "type": "mssql_add_source",
                "args": {
                    "name": "mssql_naming_conventions_db",
                    "configuration": {
                        "connection_info": {
                            "connection_string": {
                                "from_env": "HASURA_GRAPHQL_MSSQL_SOURCE_URL",
                            },
                        },
                    },
                    "customization": {
                        "naming_convention": "graphql-default",
                    },
                }
            },
            expected_status_code=400,
        )
        assert response == {
            "code": "not-supported",
            "path": "$.args",
            "error": "sources other than postgres do not support graphql-default as naming convention yet",
        }

@pytest.mark.hge_env('HASURA_GRAPHQL_EXPERIMENTAL_FEATURES', 'naming_convention')
@pytest.mark.hge_env('HASURA_GRAPHQL_DEFAULT_NAMING_CONVENTION', 'graphql-default')
class TestDefaultNamingConvention:

    @classmethod
    def dir(cls):
        return "queries/naming_conventions"

    @pytest.fixture(scope='class', autouse=True)
    def add_another_source(self, add_source):
        add_source('pg1')

    def test_default_global_naming_convention(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/default_global_naming_convention.yaml')

@pytest.mark.hge_env('HASURA_GRAPHQL_EXPERIMENTAL_FEATURES', None)  # must be unset
class TestNamingConventionWithoutExperimentalFeature:

    @classmethod
    def dir(cls):
        return "queries/naming_conventions"

    @pytest.fixture(scope='class', autouse=True)
    def add_customized_source(self, add_source):
        add_source('pg1', customization={'naming_convention': 'graphql-default'})

    def test_naming_convention_without_feature_turned_on(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/naming_convention_without_feature_turned_on.yaml')
