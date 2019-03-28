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
import time
import uuid
import string
import random

import yaml
import requests
import websocket
from sqlalchemy import create_engine
from sqlalchemy.schema import MetaData
import graphql_server


class HGECtxError(Exception):
    pass

class GQLWsClient:

    def __init__(self, hge_ctx):
        self.hge_ctx = hge_ctx
        self.ws_queue = queue.Queue(maxsize=-1)
        self.ws_url = urlparse(hge_ctx.hge_url)
        self.ws_url = self.ws_url._replace(scheme='ws')
        self.ws_url = self.ws_url._replace(path='/v1alpha1/graphql')
        self.ws = websocket.WebSocketApp(self.ws_url.geturl(), on_message=self._on_message, on_close=self._on_close)
        self.wst = threading.Thread(target=self.ws.run_forever)
        self.wst.daemon = True
        self.wst.start()
        self.remote_closed = False
        self.init_done = False
        self.op_ids= set()

    def get_ws_event(self, timeout):
        return json.loads(self.ws_queue.get(timeout=timeout))

    def init(self):
        payload = {'type': 'connection_init', 'payload': {}}
        if self.hge_ctx.hge_key:
            payload['payload']['headers'] = {
                'x-hasura-admin-secret': self.hge_ctx.hge_key
            }
        self.ws.send(json.dumps(payload))
        ev = self.get_ws_event(3)
        assert ev['type'] == 'connection_ack', ev
        self.init_done = True

    def stop(self, _id):
        data = {'id': _id, 'type': 'stop'}
        self.ws.send(json.dumps(data))
        ev = self.get_ws_event(3)
        assert ev['type'] == 'complete', ev
        self.op_ids.remove(_id)

    def gen_id(self, size=6, chars=string.ascii_letters + string.digits):
        newId = ''.join(random.choice(chars) for _ in range(size))
        if newId in self.op_ids:
            return gen_id(self,size,chars)
        else:
            return newId

    def send_query(self, query, _id=None):
        if not self.init_done:
            self.init()
        if _id == None:
            _id = self.gen_id()
        frame = {
            'id': _id,
            'type': 'start',
            'payload': {'query': query},
        }
        self.op_ids.add(_id)
        if self.hge_ctx.hge_key:
            frame['payload']['headers'] = {
                'x-hasura-admin-secret': self.hge_ctx.hge_key
            }
        self.ws.send(json.dumps(frame))
        return _id

    def _on_message(self, message):
        my_json = json.loads(message)
        if my_json['type'] != 'ka':
            self.ws_queue.put(message)

    def _on_close(self):
        self.remote_closed = True

    def teardown(self):
        if not self.remote_closed:
            self.ws.close()
        self.wst.join()

class EvtsWebhookHandler(http.server.BaseHTTPRequestHandler):
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
        elif req_path == "/timeout_short":
            time.sleep(5)
            self.send_response(HTTPStatus.NO_CONTENT)
            self.end_headers()
            self.server.error_queue.put({"path": req_path,
                                         "body": req_json,
                                         "headers": req_headers})
        elif req_path == "/timeout_long":
            time.sleep(5)
            self.send_response(HTTPStatus.NO_CONTENT)
            self.end_headers()
            self.server.resp_queue.put({"path": req_path,
                                        "body": req_json,
                                        "headers": req_headers})
        else:
            self.send_response(HTTPStatus.NO_CONTENT)
            self.end_headers()
            self.server.resp_queue.put({"path": req_path,
                                        "body": req_json,
                                        "headers": req_headers})

class EvtsWebhookServer(http.server.HTTPServer):
    def __init__(self, server_address):
        self.resp_queue = queue.Queue(maxsize=1)
        self.error_queue = queue.Queue()
        super().__init__(server_address, EvtsWebhookHandler)

    def server_bind(self):
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.socket.bind(self.server_address)

    def get_event(self, timeout):
        return self.resp_queue.get(timeout=timeout)

    def get_error_queue_size(self):
        sz = 0
        while not self.error_queue.empty():
            self.error_queue.get()
            sz = sz + 1
        return sz

    def teardown(self):
        self.evt_trggr_httpd.shutdown()
        self.evt_trggr_httpd.server_close()
        graphql_server.stop_server(self.graphql_server)
        self.gql_srvr_thread.join()
        self.evt_trggr_web_server.join()

class HGECtxGQLServer:
    def __init__(self):
        # start the graphql server
        self.graphql_server = graphql_server.create_server('127.0.0.1', 5000)
        self.gql_srvr_thread = threading.Thread(target=self.graphql_server.serve_forever)
        self.gql_srvr_thread.start()

    def teardown(self):
        graphql_server.stop_server(self.graphql_server)
        self.gql_srvr_thread.join()


class HGECtx:

#    def choose_urls(self, hge_url_list, pg_url_list):
#        self.hge_url = None
#        self.pge_url = None
#        for i, hge_url in enumerate(hge_url_list):
#            self.hge_url = hge_url_list[i]
#            self.pg_url  = pg_url_list[i]
#            st_code, resp = self.v1q_f('queries/create_type.yaml')
#            if st_code == 200:
#                return True
#        return False
        
  
    def __init__(self, hge_url, pg_url, hge_key, hge_webhook, webhook_insecure,
                 hge_jwt_key_file, hge_jwt_conf, metadata_disabled, ws_read_cookie, hge_scale_url):
        #hge_url_list = [x.strip() for x in hge_urls.split(',')]
        #pg_url_list  = [x.strip() for x in pg_urls.split(',')]

        self.http = requests.Session()
        self.hge_key = hge_key
        self.hge_url = hge_url
        self.pg_url = pg_url
        self.hge_webhook = hge_webhook
        if hge_jwt_key_file is None:
            self.hge_jwt_key = None
        else:
            with open(hge_jwt_key_file) as f:
                self.hge_jwt_key = f.read()
        self.hge_jwt_conf = hge_jwt_conf
        self.webhook_insecure = webhook_insecure
        self.metadata_disabled = metadata_disabled
        self.may_skip_test_teardown = False

        self.engine = create_engine(self.pg_url)
        self.meta = MetaData()

        self.ws_read_cookie = ws_read_cookie

        self.hge_scale_url = hge_scale_url

        result = subprocess.run(['../../scripts/get-version.sh'], shell=False, stdout=subprocess.PIPE, check=True)
        self.version = result.stdout.decode('utf-8').strip()
        if not self.metadata_disabled:
          try:
              st_code, resp = self.v1q_f('queries/clear_db.yaml')
          except requests.exceptions.RequestException as e:
              self.teardown()
              raise HGECtxError(repr(e))
          assert st_code == 200, resp

    def reflect_tables(self):
        self.meta.reflect(bind=self.engine)

    def anyq(self, u, q, h):
        resp = self.http.post(
            self.hge_url + u,
            json=q,
            headers=h
        )
        return resp.status_code, resp.json()

    def sql(self, q):
        conn = self.engine.connect()
        res  = conn.execute(q)
        conn.close()
        return res

    def v1q(self, q, headers = {}):
        h = headers.copy()
        if self.hge_key is not None:
            h['X-Hasura-Admin-Secret'] = self.hge_key
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
#        self.v1q_f('queries/drop_type.yaml')
        self.http.close()
        self.engine.dispose()
