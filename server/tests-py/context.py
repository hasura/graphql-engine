#!/usr/bin/env python3

import socketserver
import threading
import http.server
import json
import yaml
import queue
import requests
import socket
import websocket
import subprocess

from http import HTTPStatus
from urllib.parse import urlparse

from sqlalchemy import create_engine
from sqlalchemy.schema import MetaData

class HGECtxError(Exception):
    pass

class WebhookHandler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        self.send_response(HTTPStatus.OK)
        self.end_headers()

    def do_POST(self):
        contentLen = self.headers.get('Content-Length')
        reqBody = self.rfile.read(int(contentLen)).decode("utf-8")
        reqJson = json.loads(reqBody)
        reqHeaders = self.headers
        reqPath = self.path
        self.log_message(json.dumps(reqJson))
        if reqPath == "/fail":
            self.send_response(HTTPStatus.INTERNAL_SERVER_ERROR)
            self.end_headers()
            self.server.error_queue.put({"path": reqPath, "body": reqJson, "headers": reqHeaders})
        else:
            self.send_response(HTTPStatus.NO_CONTENT)
            self.end_headers()
            self.server.resp_queue.put({"path": reqPath, "body": reqJson, "headers": reqHeaders})


class WebhookServer(http.server.HTTPServer):
    def __init__(self, resp_queue, error_queue, server_address):
        self.resp_queue = resp_queue
        self.error_queue = error_queue
        super().__init__(server_address, WebhookHandler)

    def server_bind(self):
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.socket.bind(self.server_address)

class HGECtx:
    def __init__(self, hge_url, pg_url):
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

        self.ws_url = urlparse(hge_url)
        self.ws_url = self.ws_url._replace(scheme='ws')
        self.ws_url = self.ws_url._replace(path='/v1alpha1/graphql')
        self.ws = websocket.WebSocketApp(self.ws_url.geturl(), on_message=self._on_message)
        self.wst = threading.Thread(target=self.ws.run_forever)
        self.wst.daemon = True
        self.wst.start()

        result = subprocess.run(['../../scripts/get-version.sh'], shell=True, stdout=subprocess.PIPE, check=True)
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

    def anyq(self, u,  q, h):
        resp = self.http.post(
            self.hge_url + u,
            json=q,
            headers=h
        )
        return resp.status_code, resp.json()

    def v1q(self, q):
        resp = self.http.post(
            self.hge_url + "/v1/query",
            json=q
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
