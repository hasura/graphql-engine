import yaml
import pytest
from validate import check_query_f
from super_classes import DefaultTestSelectQueries

class TestPGDump(DefaultTestSelectQueries):

    def test_pg_dump_for_public_schema(self, hge_ctx):
        query_file = self.dir() + '/pg_dump_public.yaml'
        with open(query_file, 'r') as stream:
            q = yaml.safe_load(stream)
            headers = {}
            if hge_ctx.hge_key is not None:
                headers['x-hasura-admin-secret'] = hge_ctx.hge_key
            resp = hge_ctx.http.post(hge_ctx.hge_url + q['url'], json=q['query'], headers=headers)
            body = resp.text
            assert resp.status_code == q['status']
            assert body == q['response']

    @classmethod
    def dir(cls):
        return "pgdump"
