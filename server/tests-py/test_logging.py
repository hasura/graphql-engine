#!/usr/bin/env python3

import string
import random
import yaml
import json
import queue
import requests
import os

import pytest

from validate import check_query_f, check_query


def mk_add_remote_q(name, url, headers=None, client_hdrs=False):
    return {
        "type": "add_remote_schema",
        "args": {
            "name": name,
            "comment": "testing " + name,
            "definition": {
                "url": url,
                "headers": headers,
                "forward_client_headers": client_hdrs
            }
        }
    }

def mk_delete_remote_q(name):
    return {
        "type" : "remove_remote_schema",
        "args" : {
            "name": name
        }
    }


class TestVerboseLogging():
    """ basic => no hasura tables are tracked """

    # teardown = {"type": "clear_metadata", "args": {}}
    # dir = 'queries/remote_schemas'

    # @pytest.fixture(autouse=True)
    # def transact(self, hge_ctx):
    #     q = mk_add_remote_q('simple 1', 'http://localhost:5000/hello-graphql')
    #     st_code, resp = hge_ctx.v1q(q)
    #     assert st_code == 200, resp
    #     yield
    #     hge_ctx.v1q(self.teardown)

    def test_startup_logs(self, hge_ctx):
        test_type = os.getenv('TEST_TYPE', None)
        tixfile = os.getenv('TIX_FILE_INDEX', None)
        output_folder = os.getenv('OUTPUT_FOLDER', None)
        # "$OUTPUT_FOLDER/graphql-engine-${i}-${TEST_TYPE}.log"
        log_file = "{}/graphql-engine-{}-{}.log".format(output_folder, tixfile, test_type)
        print(log_file)
        assert False
