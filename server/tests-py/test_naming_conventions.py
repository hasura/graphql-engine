import ruamel.yaml as yaml
from validate import check_query_f
import pytest
import os

@pytest.mark.skipif(
    os.getenv('HASURA_GRAPHQL_EXPERIMENTAL_FEATURES') is None or
    not 'naming_convention' in os.getenv('HASURA_GRAPHQL_EXPERIMENTAL_FEATURES'),
    reason="This test expects the (naming_convention) experimental feature turned on")
class TestNamingConventions:

    @classmethod
    def dir(cls):
        return "queries/naming_conventions"

    def test_type_and_field_names(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/type_and_field_names.yaml')

    def test_field_name_precedence(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/field_name_precedence.yaml')
    
    def test_enum_value_convention(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/enum_value_convention.yaml')
    
    def test_type_and_field_names(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/type_and_field_names_with_prefix_and_suffix.yaml')

@pytest.mark.parametrize("backend", ['mssql'])
class TestNamingConventionsFailure:
    @classmethod
    def dir(cls):
        return "queries/naming_conventions"
    
    def test_other_than_pg_db_failure(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/mssql_naming_convention.yaml')

@pytest.mark.skipif(
    os.getenv('HASURA_GRAPHQL_DEFAULT_NAMING_CONVENTION') is None or
    not 'graphql-default' in os.getenv('HASURA_GRAPHQL_DEFAULT_NAMING_CONVENTION'),
    reason="This test expects the HASURA_GRAPHQL_DEFAULT_NAMING_CONVENTION environment variable set to graphql-default")
class TestDefaultNamingConvention:

    @classmethod
    def dir(cls):
        return "queries/naming_conventions"
    
    def test_default_global_naming_convention(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/default_global_naming_convention.yaml')
