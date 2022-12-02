import ruamel.yaml as yaml
from validate import check_query_f
import pytest
import os

@pytest.mark.hge_env('HASURA_GRAPHQL_EXPERIMENTAL_FEATURES', 'naming_convention')
class TestNamingConventions:

    @classmethod
    def dir(cls):
        return "queries/naming_conventions"

    def test_field_name_precedence(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/field_name_precedence.yaml')

    def test_enum_value_convention(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/enum_value_convention.yaml')

@pytest.mark.hge_env('HASURA_GRAPHQL_EXPERIMENTAL_FEATURES', 'naming_convention')
class TestNamingConventionsTypeAndFieldNamesGraphqlDefault:

    @classmethod
    def dir(cls):
        return "queries/naming_conventions"

    def test_type_and_field_names(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/type_and_field_names_graphql_default.yaml')

@pytest.mark.hge_env('HASURA_GRAPHQL_EXPERIMENTAL_FEATURES', 'naming_convention')
class TestNamingConventionsTypeAndFieldNamesHasuraDefault:

    @classmethod
    def dir(cls):
        return "queries/naming_conventions"

    def test_type_and_field_names(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/type_and_field_names_hasura_default.yaml')

@pytest.mark.hge_env('HASURA_GRAPHQL_EXPERIMENTAL_FEATURES', 'naming_convention')
class TestNamingConventionsTypeAndFieldNamesGraphqlDefaultWithPrefixAndSuffix:

    @classmethod
    def dir(cls):
        return "queries/naming_conventions"

    def test_type_and_field_names_with_prefix_and_suffix(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/type_and_field_names_graphql_default_with_prefix_and_suffix.yaml')

@pytest.mark.hge_env('HASURA_GRAPHQL_EXPERIMENTAL_FEATURES', 'naming_convention')
class TestNamingConventionsTypeAndFieldNamesHasuraDefaultWithPrefixAndSuffix:

    @classmethod
    def dir(cls):
        return "queries/naming_conventions"

    def test_type_and_field_names_with_prefix_and_suffix(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/type_and_field_names_hasura_default_with_prefix_and_suffix.yaml')

@pytest.mark.backend('mssql')
class TestNamingConventionsFailure:
    @classmethod
    def dir(cls):
        return "queries/naming_conventions"

    def test_other_than_pg_db_failure(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/mssql_naming_convention.yaml')

@pytest.mark.hge_env('HASURA_GRAPHQL_EXPERIMENTAL_FEATURES', 'naming_convention')
@pytest.mark.hge_env('HASURA_GRAPHQL_DEFAULT_NAMING_CONVENTION', 'graphql-default')
class TestDefaultNamingConvention:

    @classmethod
    def dir(cls):
        return "queries/naming_conventions"

    def test_default_global_naming_convention(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/default_global_naming_convention.yaml')

@pytest.mark.hge_env('HASURA_GRAPHQL_EXPERIMENTAL_FEATURES', None)  # must be unset
class TestNamingConventionWithoutExperimentalFeature:

    @classmethod
    def dir(cls):
        return "queries/naming_conventions"

    def test_naming_convention_without_feature_turned_on(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/naming_convention_without_feature_turned_on.yaml')
