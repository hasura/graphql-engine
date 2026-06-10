import pytest


class TestJsonControlCharHandling:
    """Clients sometimes send literal unescaped control characters inside JSON
    string values (illegal per RFC 8259). See 'parseBody' for details.

    This is expected to fail after the upgrade to aeson 2.2.4.0 and pass again
    after our subsequent fix to parseBody"""

    def test_literal_newline_in_json_string(self, hge_ctx):
        headers = {'Content-Type': 'application/json'}
        if hge_ctx.hge_key:
            headers['X-Hasura-Admin-Secret'] = hge_ctx.hge_key

        # \n here is a literal 0x0A byte, NOT the two-byte escape sequence \n
        # (0x5C 0x6E). requests' data= sends raw bytes unchanged.
        # The literal newline falls between two Name tokens ("__typename" and
        # "query_root" would merge into one if stripped rather than replaced
        # with a space, so this also validates replace-vs-strip behaviour).
        body = b'{"query": "{__typename\n__typename}"}'

        resp = hge_ctx.http.post(
            hge_ctx.hge_url + '/v1/graphql',
            data=body,
            headers=headers,
        )
        resp_json = resp.json()
        # Previously: "Unexpected control character while parsing string literal"
        assert resp.status_code == 200, resp_json
        assert resp_json.get('data', {}).get('__typename') == 'query_root', resp_json


def v1qCompat(hge_ctx, q):
        h = {'X-Hasura-Access-Key': hge_ctx.hge_key}
        resp = hge_ctx.http.post(
            hge_ctx.hge_url + "/v1/query",
            json=q,
            headers=h
        )
        return resp.status_code, resp.json()

@pytest.mark.admin_secret
class TestGraphQLCompatAccessKey():

    export_metadata = {
       "type" : "export_metadata",
       "args" : {}
    }

    def test_compact_access_key_export_metadata(self, hge_ctx):
        code, resp = v1qCompat(hge_ctx, self.export_metadata)
        assert code == 200, resp
