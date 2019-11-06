#!/usr/bin/env python3

import pytest
import ruamel.yaml as yaml
import jsondiff

from super_classes import DefaultTestSelectQueries

class TestETag(DefaultTestSelectQueries):

    @classmethod
    def dir(cls):
        return 'queries/etag'

    etag_header_name = 'ETag'

    def _make_post(self, hge_ctx, u, q, h={}):
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

    def _assert_etag(self, resp):
        assert self.etag_header_name in resp.headers, "ETag header is expected"

    def _assert_no_etag(self, resp):
        assert self.etag_header_name not in resp.headers, "ETag header is not expected"

    def _assert_status_code_200(self, resp):
        assert resp.status_code == 200, resp.json()

    def _assert_resp(self, resp, exp_resp):
        json_resp = resp.json()
        assert json_resp == exp_resp, yaml.dump({
            'response': json_resp,
            'expected': exp_resp,
            'diff': jsondiff.diff(exp_resp, json_resp)
        })

    def test_graphql_query(self, hge_ctx):
        url, q, exp_resp = self._get_config(self.dir() + '/graphql_query.yaml')

        # Make GraphQL query for first time
        resp_1 = self._make_post(hge_ctx, url, q)
        self._assert_status_code_200(resp_1)
        self._assert_resp(resp_1, exp_resp)
        self._assert_etag(resp_1)

        # Make GraphQL query with 'If-None-Match' header
        etag = resp_1.headers[self.etag_header_name]
        if_none_match_header = {'If-None-Match': etag}
        resp_2 = self._make_post(hge_ctx, url, q, if_none_match_header)
        assert resp_2.status_code == 304, "Status 304 Not Modified expected"
        self._assert_etag(resp_2)
        assert resp_2.headers[self.etag_header_name] == etag, "ETag values don't match"

    def test_v1_query(self, hge_ctx):
        url = "/v1/query"
        q = {'type': 'export_metadata', 'args': {}}

        resp = self._make_post(hge_ctx, url, q)
        self._assert_status_code_200(resp)
        self._assert_no_etag(resp)

    def test_graphql_mutation(self, hge_ctx):
        url, q, exp_resp = self._get_config(self.dir() + '/graphql_mutation.yaml')

        resp = self._make_post(hge_ctx, url, q)
        self._assert_status_code_200(resp)
        self._assert_resp(resp, exp_resp)
        self._assert_no_etag(resp)
