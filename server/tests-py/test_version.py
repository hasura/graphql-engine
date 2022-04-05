#!/usr/bin/env python3

import pytest
import re

class TestServerVersion(object):

    def test_version(self, hge_ctx):
        resp = hge_ctx.http.get(
            hge_ctx.hge_url + '/v1/version'
        )
        assert resp.status_code == 200, resp
        my_json = resp.json()
        assert isinstance(my_json, dict), my_json

        # The magic number here means we're compiling for local development and
        # this test can be ignored:
        if my_json['version'] != '12345':
        # The tree may be dirty because we're developing tests locally while
        # graphql-engine was built previously when tree was clean. If we're
        # modifying graphql-engine too then both of these will be tagged dirty,
        # since a rebuild would necessarily be forced:
            assert my_json['version'] in (hge_ctx.version, re.sub('-dirty$', '', hge_ctx.version)), my_json
