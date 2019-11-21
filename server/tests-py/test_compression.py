#!/usr/bin/env python3

import pytest
import ruamel.yaml as yaml
import jsondiff

from super_classes import DefaultTestSelectQueries

class TestCompression(DefaultTestSelectQueries):

    gzip_header = {'Accept-Encoding': 'gzip'}
    brotli_header = {'Accept-Encoding': 'br'}
    gzip_brotli_header = {'Accept-Encoding': 'gzip, br'}

    def _make_post(self, hge_ctx, u, q, h):
        if hge_ctx.hge_key is not None:
            h['X-Hasura-Admin-Secret'] = hge_ctx.hge_key
        resp = hge_ctx.http.post(
            hge_ctx.hge_url + u,
            json=q,
            headers=h
        )
        return resp

    def _get_config(self, f):
        with open(f) as c:
            conf = yaml.safe_load(c)
            return conf['url'], conf['query'], conf['response']

    def _assert_status_code_200(self, resp):
        assert resp.status_code == 200, resp.json()

    def _assert_encoding(self, headers, encoding):
        assert 'Content-Encoding' in headers, headers
        assert headers['Content-Encoding'] == encoding, headers

    def _assert_resp(self, resp, exp_resp):
        json_resp = resp.json()
        assert json_resp == exp_resp, yaml.dump({
            'response': json_resp,
            'expected': exp_resp,
            'diff': jsondiff.diff(exp_resp, json_resp)
        })

    def _assert_gzip(self, resp, exp_resp):
        self._assert_status_code_200(resp)
        self._assert_encoding(resp.headers, 'gzip')
        self._assert_resp(resp, exp_resp)

    def _assert_brotli(self, resp, exp_resp):
        self._assert_status_code_200(resp)
        self._assert_encoding(resp.headers, 'br')
        self._assert_resp(resp, exp_resp)

    def test_gzip_compression_graphql(self, hge_ctx):
        url, q, exp_resp = self._get_config(self.dir() + '/graphql_query.yaml')
        resp = self._make_post(hge_ctx, url, q, self.gzip_header)
        self._assert_gzip(resp, exp_resp)

    def test_gzip_compression_v1_query(self, hge_ctx):
        url, q, exp_resp = self._get_config(self.dir() + '/v1_query.yaml')
        resp = self._make_post(hge_ctx, url, q, self.gzip_header)
        self._assert_gzip(resp, exp_resp)


    def test_gzip_brotli_graphql_query(self, hge_ctx):
        url, q, exp_resp = self._get_config(self.dir() + '/graphql_query.yaml')
        resp = self._make_post(hge_ctx, url, q, self.gzip_brotli_header)
        self._assert_gzip(resp, exp_resp)

    def test_gzip_brotli_v1_query(self, hge_ctx):
        url, q, exp_resp = self._get_config(self.dir() + '/v1_query.yaml')
        resp = self._make_post(hge_ctx, url, q, self.gzip_brotli_header)
        self._assert_gzip(resp, exp_resp)

    @classmethod
    def dir(cls):
        return 'queries/compression'
