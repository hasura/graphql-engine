#!/usr/bin/env python3

import subprocess
import re

class TestServerVersion(object):

    def test_version(self, hge_ctx):
        resp = hge_ctx.http.get(
            hge_ctx.hge_url + '/v1/version'
        )
        assert resp.status_code == 200, resp
        version_json = resp.json()
        assert isinstance(version_json, dict), version_json

        server_version = version_json['version']

        # The magic number here means we're compiling for local development and
        # this test can be ignored:
        if server_version == '12345':
            return

        # Grab the Git details so that we know why things changed.
        def error_message():
            git_status = subprocess.run(['git', 'status', '--porcelain'], stdout=subprocess.PIPE, stderr=subprocess.STDOUT, encoding='utf8').stdout
            git_diff = subprocess.run(['git', 'diff-index', '-p', 'HEAD', '--'], stdout=subprocess.PIPE, stderr=subprocess.STDOUT, encoding='utf8').stdout
            return f'Version JSON:\n{version_json}\n\nGit status:\n{git_status}\n\nGit diff:{git_diff}\n'

        # The tree may be dirty because we're developing tests locally while
        # graphql-engine was built previously when tree was clean. If we're
        # modifying graphql-engine too then both of these will be tagged dirty,
        # since a rebuild would necessarily be forced:
        assert server_version in (hge_ctx.version, re.sub('-dirty$', '', hge_ctx.version)), error_message()
