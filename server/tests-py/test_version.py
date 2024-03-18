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
