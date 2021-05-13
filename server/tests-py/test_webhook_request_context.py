import pytest
import time
import json
import http
import queue
import socket
from context import (
    HGECtx,
    HGECtxError,
    ActionsWebhookServer,
    EvtsWebhookServer,
    HGECtxGQLServer,
    GQLWsClient,
    PytestConf,
)
import threading
import random
from datetime import datetime
import sys
import os
from collections import OrderedDict
from validate import check_query

if not PytestConf.config.getoption("--test-webhook-request-context"):
    pytest.skip("--test-webhook-https-request-context flag is missing, skipping tests", allow_module_level=True)


class QueryEchoWebhookHandler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        self.log_message("get")
        self.send_response(http.HTTPStatus.OK)
        self.end_headers()

    def do_POST(self):
        self.log_message("post")
        content_len = self.headers.get("Content-Length")
        req_body = self.rfile.read(int(content_len)).decode("utf-8")
        req_json = json.loads(req_body)
        req_headers = self.headers
        print(json.dumps(req_json))
        user_id_header = req_json["headers"]["auth-user-id"]
        req_path = self.path
        h = {
            "x-hasura-role":"user",
            "x-hasura-user-id": user_id_header
        }
        self.send_response(http.HTTPStatus.OK)
        self.send_header('Content-Type', 'application/json')
        self.end_headers()
        self.server.resp_queue.put({"request": req_json["request"]})
        self.wfile.write(json.dumps(h).encode('utf-8'))


class QueryEchoWebhookServer(http.server.HTTPServer):
    def __init__(self, server_address):
        # TODO why maxsize=1
        self.resp_queue = queue.Queue(maxsize=1)
        super().__init__(server_address, QueryEchoWebhookHandler)

    def server_bind(self):
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.socket.bind(self.server_address)

    def get_event(self, timeout):
        return self.resp_queue.get(timeout=timeout)

    def teardown(self):
        self.evt_trggr_httpd.shutdown()
        self.evt_trggr_httpd.server_close()
        graphql_server.stop_server(self.graphql_server)
        self.gql_srvr_thread.join()
        self.evt_trggr_web_server.join()


@pytest.fixture(scope="class")
def query_echo_webhook(request):
    # TODO(swann): is this the right port?
    webhook_httpd = QueryEchoWebhookServer(server_address=("127.0.0.1", 5594))
    web_server = threading.Thread(target=webhook_httpd.serve_forever)
    web_server.start()
    yield webhook_httpd
    webhook_httpd.shutdown()
    webhook_httpd.server_close()
    web_server.join()


@pytest.mark.usefixtures("per_method_tests_db_state")
class TestWebhookRequestContext(object):
    @classmethod
    def dir(cls):
        return "queries/webhooks/request_context"

    def test_query(self, hge_ctx, query_echo_webhook):
        query = """
          query allUsers {
            users {
              id
              name
            }
          }
        """
        query_obj = {
            "query": query,
            "operationName": "allUsers"
        }
        headers = dict()
        headers['auth-user-id'] = '1'
        code, resp, _ = hge_ctx.anyq('/v1/graphql', query_obj, headers)
        assert code == 200, resp

        ev_full = query_echo_webhook.get_event(3)
        exp_result = {"request": query_obj}
        assert ev_full['request'] == query_obj

    def test_mutation_with_vars(self, hge_ctx, query_echo_webhook):
        query = """
          mutation insert_single_user($id: Int!, $name: String!) {
            insert_users_one(
              object: {
                id: $id,
                name: $name
              }
            ) {
              id
              name
            }
          }
        """
        variables = {"id": 4, "name": "danish"}
        query_obj = {"query": query, "variables": variables, "operationName": "insert_single_user"}
        headers = dict()
        headers['auth-user-id'] = '4'
        code, resp, _ = hge_ctx.anyq('/v1/graphql', query_obj, headers)
        assert code == 200, resp

        ev_full = query_echo_webhook.get_event(3)
        exp_result = {"request": query_obj}
        assert ev_full['request'] == query_obj
