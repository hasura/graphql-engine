import yaml
import pytest
from validate import check_query_f
from super_classes import DefaultTestSelectQueries

class TestPGDump(DefaultTestSelectQueries):

    def test_pg_dump_for_public_schema(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/pg_dump_public.yaml')

    @classmethod
    def dir(cls):
        return "pgdump"
