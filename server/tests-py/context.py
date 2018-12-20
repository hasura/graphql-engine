#!/usr/bin/env python3

from http import HTTPStatus
from urllib.parse import urlparse
# import socketserver
import threading
import http.server
import json
import queue
import socket
import subprocess

import yaml
import requests
import websocket
from sqlalchemy import create_engine
from sqlalchemy.schema import MetaData
import graphql_server


class HGECtxError(Exception):
    pass


class WebhookHandler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        self.send_response(HTTPStatus.OK)
        self.end_headers()

    def do_POST(self):
        content_len = self.headers.get('Content-Length')
        req_body = self.rfile.read(int(content_len)).decode("utf-8")
        req_json = json.loads(req_body)
        req_headers = self.headers
        req_path = self.path
        self.log_message(json.dumps(req_json))
        if req_path == "/fail":
            self.send_response(HTTPStatus.INTERNAL_SERVER_ERROR)
            self.end_headers()
            self.server.error_queue.put({"path": req_path,
                                         "body": req_json,
                                         "headers": req_headers})
        else:
            self.send_response(HTTPStatus.NO_CONTENT)
            self.end_headers()
            self.server.resp_queue.put({"path": req_path,
                                        "body": req_json,
                                        "headers": req_headers})


class WebhookServer(http.server.HTTPServer):
    def __init__(self, resp_queue, error_queue, server_address):
        self.resp_queue = resp_queue
        self.error_queue = error_queue
        super().__init__(server_address, WebhookHandler)

    def server_bind(self):
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.socket.bind(self.server_address)


class HGECtx:
    def __init__(self, hge_url, pg_url, hge_key, hge_webhook, hge_jwt_key_file, webhook_insecure):
        server_address = ('0.0.0.0', 5592)

        self.resp_queue = queue.Queue(maxsize=1)
        self.error_queue = queue.Queue()
        self.ws_queue = queue.Queue(maxsize=-1)
        self.httpd = WebhookServer(self.resp_queue, self.error_queue, server_address)
        self.web_server = threading.Thread(target=self.httpd.serve_forever)
        self.web_server.start()

        self.pg_url = pg_url
        self.engine = create_engine(self.pg_url)
        self.meta = MetaData()

        self.http = requests.Session()
        self.hge_url = hge_url
        self.hge_key = hge_key
        self.hge_webhook = hge_webhook
        if hge_jwt_key_file is None:
            self.hge_jwt_key = None
        else:
            with open(hge_jwt_key_file) as f:
                self.hge_jwt_key = f.read()
        self.webhook_insecure = webhook_insecure
        self.may_skip_test_teardown = False

        self.ws_url = urlparse(hge_url)
        self.ws_url = self.ws_url._replace(scheme='ws')
        self.ws_url = self.ws_url._replace(path='/v1alpha1/graphql')
        self.ws = websocket.WebSocketApp(self.ws_url.geturl(), on_message=self._on_message)
        self.wst = threading.Thread(target=self.ws.run_forever)
        self.wst.daemon = True
        self.wst.start()

        # start the graphql server
        self.graphql_server = graphql_server.create_server('127.0.0.1', 5000)
        self.gql_srvr_thread = threading.Thread(target=self.graphql_server.serve_forever)
        self.gql_srvr_thread.start()

        result = subprocess.run(['../../scripts/get-version.sh'], shell=False, stdout=subprocess.PIPE, check=True)
        self.version = result.stdout.decode('utf-8').strip()
        try:
            st_code, resp = self.v1q_f('queries/clear_db.yaml')
        except requests.exceptions.RequestException as e:
            self.teardown()
            raise HGECtxError(repr(e))
        assert st_code == 200, resp

    def _on_message(self, message):
        my_json = json.loads(message)
        if my_json['type'] != 'ka':
            self.ws_queue.put(message)

    def get_event(self, timeout):
        return self.resp_queue.get(timeout=timeout)

    def get_error_queue_size(self):
        sz = 0
        while not self.error_queue.empty():
            self.error_queue.get()
            sz = sz + 1
        return sz

    def get_ws_event(self, timeout):
        return json.loads(self.ws_queue.get(timeout=timeout))

    def reflect_tables(self):
        self.meta.reflect(bind=self.engine)

    def anyq(self, u, q, h):
        resp = self.http.post(
            self.hge_url + u,
            json=q,
            headers=h
        )
        return resp.status_code, resp.json()

    def v1q(self, q):
        h = dict()
        if self.hge_key is not None:
            h['X-Hasura-Access-Key'] = self.hge_key
        resp = self.http.post(
            self.hge_url + "/v1/query",
            json=q,
            headers=h
        )
        return resp.status_code, resp.json()

    def v1q_f(self, fn):
        with open(fn) as f:
            return self.v1q(yaml.load(f))

    def teardown(self):
        self.http.close()
        self.engine.dispose()
        self.httpd.shutdown()
        self.httpd.server_close()
        self.ws.close()
        self.web_server.join()
        self.wst.join()
        graphql_server.stop_server(self.graphql_server)
        self.gql_srvr_thread.join()
