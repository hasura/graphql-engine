import pytest
from ruamel.yaml import YAML

yaml=YAML(typ='safe', pure=True)

@pytest.mark.usefixtures('per_method_tests_db_state')
class TestPGDump:
    def test_pg_dump_for_public_schema(self, hge_ctx):
        query_file = self.dir() + '/pg_dump_public.yaml'
        with open(query_file, 'r') as stream:
            q = yaml.load(stream)
            headers = q['headers'] or {}
            if hge_ctx.hge_key is not None:
                headers['x-hasura-admin-secret'] = hge_ctx.hge_key
            response = hge_ctx.http.post(hge_ctx.hge_url + q['url'], json=q['query'], headers=headers)
            body = response.text
            assert response.status_code == q['status'], f'Response body: {body}'
            assert body == q['expected_response'], f'Response body: {body}'

    def test_pg_dump_for_public_schema_for_user_role(self, hge_ctx):
        query_file = self.dir() + '/pg_dump_public.yaml'
        with open(query_file, 'r') as stream:
            q = yaml.load(stream)
            headers = q['headers'] or {}
            if hge_ctx.hge_key is not None:
                headers['x-hasura-admin-secret'] = hge_ctx.hge_key
            headers['X-Hasura-Role'] = 'user'
            resp = hge_ctx.http.post(hge_ctx.hge_url + q['url'], json=q['query'], headers=headers)
            body = resp.text
            assert resp.status_code == 400, body

    @classmethod
    def dir(cls):
        return "pgdump"
