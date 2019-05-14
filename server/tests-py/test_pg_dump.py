import yaml
from super_classes import DefaultTestSelectQueries
import os

resp_pg_version_map = {
    '9_5': 'response_9',
    '9_6': 'response_9',
    '10_6': 'response_10_11',
    '11_1': 'response_10_11',
    'latest': 'response_10_11'
}

class TestPGDump(DefaultTestSelectQueries):

    def test_pg_dump_for_public_schema(self, hge_ctx):
        query_file = self.dir() + '/pg_dump_public.yaml'
        PG_VERSION = os.getenv('PG_VERSION', 'latest')
        with open(query_file, 'r') as stream:
            q = yaml.safe_load(stream)
            headers = {}
            if hge_ctx.hge_key is not None:
                headers['x-hasura-admin-secret'] = hge_ctx.hge_key
            resp = hge_ctx.http.post(hge_ctx.hge_url + q['url'], json=q['query'], headers=headers)
            body = resp.text
            assert resp.status_code == q['status']
            assert body == q[resp_pg_version_map[PG_VERSION]]

    @classmethod
    def dir(cls):
        return "pgdump"
