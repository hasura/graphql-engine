#!/usr/bin/env python3

import pytest


class TestServerVersion(object):

    def test_version(self, hge_ctx):
        resp = hge_ctx.http.get(
            hge_ctx.hge_url + '/v1/version'
        )
        my_json = resp.json()
        assert my_json['version'] == hge_ctx.version, my_json
