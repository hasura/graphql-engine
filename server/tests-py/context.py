#!/usr/bin/env python3

import socketserver
import threading
import http.server
import json
import yaml
import queue
import requests

from http import HTTPStatus

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
        self.log_message(json.dumps(reqJson))
        self.send_response(HTTPStatus.NO_CONTENT)
        self.end_headers()
        self.server.resp_queue.put(reqJson)

class WebhookServer(http.server.HTTPServer):
    def __init__(self, resp_queue, server_address):
        self.resp_queue = resp_queue
        super().__init__(server_address, WebhookHandler)

class HGECtx:
    def __init__(self, hge_url, pg_url):
        server_address = ('0.0.0.0', 5000)

        self.resp_queue = queue.Queue(maxsize=1)
        self.httpd = WebhookServer(self.resp_queue, server_address)
        self.web_server = threading.Thread(target=self.httpd.serve_forever)
        self.web_server.start()

        self.pg_url = pg_url
        self.engine = create_engine(self.pg_url)
        self.meta = MetaData()

        self.http = requests.Session()
        self.hge_url = hge_url

        try:
            st_code, resp = self.v1q_f('queries/clear_db.yaml')
        except requests.exceptions.RequestException as e:
            self.teardown()
            raise HGECtxError(repr(e))
        assert st_code == 200, resp

    def get_event(self, timeout):
        return self.resp_queue.get(timeout=timeout)

    def reflect_tables(self):
        self.meta.reflect(bind=self.engine)

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
        self.web_server.join()
