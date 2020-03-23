#!/usr/bin/env python3

from http import HTTPStatus
from socketserver import ThreadingMixIn
from urllib.parse import urlparse
from ruamel.yaml.comments import CommentedMap as OrderedDict # to avoid '!!omap' in yaml
import threading
import http.server
import json
import queue
import socket
import subprocess
import time
import string
import random
import os
import re

import ruamel.yaml as yaml
import requests
import websocket
from sqlalchemy import create_engine
from sqlalchemy.schema import MetaData
import graphql_server
import graphql

# pytest has removed the global pytest.config
# As a solution to this we are going to store it in PyTestConf.config
class PytestConf():
    pass

class HGECtxError(Exception):
    pass

class GQLWsClient():

    def __init__(self, hge_ctx, endpoint):
        self.hge_ctx = hge_ctx
        self.ws_queue = queue.Queue(maxsize=-1)
        self.ws_url = urlparse(hge_ctx.hge_url)._replace(scheme='ws',
                                                         path=endpoint)
        self.create_conn()

    def create_conn(self):
        self.ws_queue.queue.clear()
        self.ws_id_query_queues = dict()
        self.ws_active_query_ids = set()

        self.connected_event = threading.Event()
        self.init_done = False
        self.is_closing = False
        self.remote_closed = False

        self._ws = websocket.WebSocketApp(self.ws_url.geturl(),
            on_open=self._on_open, on_message=self._on_message, on_close=self._on_close)
        self.wst = threading.Thread(target=self._ws.run_forever)
        self.wst.daemon = True
        self.wst.start()

    def recreate_conn(self):
        self.teardown()
        self.create_conn()

    def wait_for_connection(self, timeout=10):
        assert not self.is_closing
        assert self.connected_event.wait(timeout=timeout)

    def get_ws_event(self, timeout):
        return self.ws_queue.get(timeout=timeout)

    def has_ws_query_events(self, query_id):
        return not self.ws_id_query_queues[query_id].empty()

    def get_ws_query_event(self, query_id, timeout):
        return self.ws_id_query_queues[query_id].get(timeout=timeout)

    def send(self, frame):
        self.wait_for_connection()
        if frame.get('type') == 'stop':
            self.ws_active_query_ids.discard( frame.get('id') )
        elif frame.get('type') == 'start' and 'id' in frame:
            self.ws_id_query_queues[frame['id']] = queue.Queue(maxsize=-1)
        self._ws.send(json.dumps(frame))

    def init_as_admin(self):
        headers={}
        if self.hge_ctx.hge_key:
            headers = {'x-hasura-admin-secret': self.hge_ctx.hge_key}
        self.init(headers)

    def init(self, headers={}):
        payload = {'type': 'connection_init', 'payload': {}}

        if headers and len(headers) > 0:
            payload['payload']['headers'] = headers

        self.send(payload)
        ev = self.get_ws_event(3)
        assert ev['type'] == 'connection_ack', ev
        self.init_done = True

    def stop(self, query_id):
        data = {'id': query_id, 'type': 'stop'}
        self.send(data)
        self.ws_active_query_ids.discard(query_id)

    def gen_id(self, size=6, chars=string.ascii_letters + string.digits):
        new_id = ''.join(random.choice(chars) for _ in range(size))
        if new_id in self.ws_active_query_ids:
            return self.gen_id(size, chars)
        return new_id

    def send_query(self, query, query_id=None, headers={}, timeout=60):
        graphql.parse(query['query'])
        if headers and len(headers) > 0:
            #Do init If headers are provided
            self.init(headers)
        elif not self.init_done:
            self.init()
        if query_id == None:
            query_id = self.gen_id()
        frame = {
            'id': query_id,
            'type': 'start',
            'payload': query,
        }
        self.ws_active_query_ids.add(query_id)
        self.send(frame)
        while True:
            yield self.get_ws_query_event(query_id, timeout)

    def _on_open(self):
        if not self.is_closing:
            self.connected_event.set()

    def _on_message(self, message):
        # NOTE: make sure we preserve key ordering so we can test the ordering
        # properties in the graphql spec properly
        json_msg = json.loads(message, object_pairs_hook=OrderedDict)
        if 'id' in json_msg:
            query_id = json_msg['id']
            if json_msg.get('type') == 'stop':
                #Remove from active queries list
                self.ws_active_query_ids.discard( query_id )
            if not query_id in self.ws_id_query_queues:
                self.ws_id_query_queues[json_msg['id']] = queue.Queue(maxsize=-1)
            #Put event in the correponding query_queue
            self.ws_id_query_queues[query_id].put(json_msg)
        elif json_msg['type'] != 'ka':
            #Put event in the main queue
            self.ws_queue.put(json_msg)

    def _on_close(self):
        self.remote_closed = True
        self.init_done = False

    def teardown(self):
        self.is_closing = True
        if not self.remote_closed:
            self._ws.close()
        self.wst.join()


class ActionsWebhookHandler(http.server.BaseHTTPRequestHandler):

    def do_GET(self):
        self.send_response(HTTPStatus.OK)
        self.end_headers()

    def do_POST(self):
        content_len = self.headers.get('Content-Length')
        req_body = self.rfile.read(int(content_len)).decode("utf-8")
        self.req_json = json.loads(req_body)
        req_headers = self.headers
        req_path = self.path
        self.log_message(json.dumps(self.req_json))

        if req_path == "/create-user":
            resp, status = self.create_user()
            self._send_response(status, resp)

        elif req_path == "/create-users":
            resp, status = self.create_users()
            self._send_response(status, resp)

        elif req_path == "/invalid-response":
            self._send_response(HTTPStatus.OK, "some-string")

        elif req_path == "/mirror-action":
            resp, status = self.mirror_action()
            self._send_response(status, resp)

        else:
            self.send_response(HTTPStatus.NO_CONTENT)
            self.end_headers()

    def create_user(self):
        email_address = self.req_json['input']['email']
        name = self.req_json['input']['name']

        if not self.check_email(email_address):
            response = {
                'message': 'Given email address is not valid',
                'code': 'invalid-email'
            }
            return response, HTTPStatus.BAD_REQUEST

        gql_query = '''
        mutation ($email: String! $name: String!) {
          insert_user_one(object: {email: $email, name: $name}){
            id
          }
        }
        '''
        query = {
            'query': gql_query,
            'variables': {
                'email': email_address,
                'name': name
            }
        }
        code, resp = self.execute_query(query)
        if code != 200 or 'data' not in resp:
            response = {
                'message': 'GraphQL query execution failed',
                'code': 'unexpected'
            }
            return response, HTTPStatus.BAD_REQUEST

        response = resp['data']['insert_user_one']
        return response, HTTPStatus.OK

    def create_users(self):
        inputs = self.req_json['input']['users']
        for input in inputs:
            email_address = input['email']
            if not self.check_email(email_address):
                response = {
                    'message': 'Email address is not valid: ' + email_address,
                    'code': 'invalid-email'
                }
                return response, HTTPStatus.BAD_REQUEST

        gql_query = '''
        mutation ($insert_inputs: [user_insert_input!]!){
          insert_user(objects: $insert_inputs){
            returning{
              id
            }
          }
        }
        '''
        query = {
            'query': gql_query,
            'variables': {
                'insert_inputs': inputs
            }
        }
        code, resp = self.execute_query(query)
        if code != 200 or 'data' not in resp:
            response = {
                'message': 'GraphQL query execution failed',
                'code': 'unexpected'
            }
            return response, HTTPStatus.BAD_REQUEST

        response = resp['data']['insert_user']['returning']
        return response, HTTPStatus.OK

    def mirror_action(self):
        response = self.req_json['input']['arg']
        return response, HTTPStatus.OK


    def check_email(self, email):
        regex = '^\w+([\.-]?\w+)*@\w+([\.-]?\w+)*(\.\w{2,3})+$'
        return re.search(regex,email)

    def execute_query(self, query):
        headers = {}
        admin_secret = self.hge_ctx.hge_key
        if admin_secret is not None:
            headers['X-Hasura-Admin-Secret'] = admin_secret
        code, resp, _ = self.hge_ctx.anyq('/v1/graphql', query, headers)
        self.log_message(json.dumps(resp))
        return code, resp

    def _send_response(self, status, body):
        self.send_response(status)
        self.send_header('Content-Type', 'application/json')
        self.send_header('Set-Cookie', 'abcd')
        self.end_headers()
        self.wfile.write(json.dumps(body).encode("utf-8"))


class ActionsWebhookServer(http.server.HTTPServer):
    def __init__(self, hge_ctx, server_address):
        handler = ActionsWebhookHandler
        handler.hge_ctx = hge_ctx
        super().__init__(server_address, handler)

    def server_bind(self):
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.socket.bind(self.server_address)

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

# A very slightly more sane/performant http server.
# See: https://stackoverflow.com/a/14089457/176841
#
# TODO use this elsewhere, or better yet: use e.g. bottle + waitress
class ThreadedHTTPServer(ThreadingMixIn, http.server.HTTPServer):
    """Handle requests in a separate thread."""

class EvtsWebhookServer(ThreadedHTTPServer):
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
    def __init__(self, hge_urls, port=5000):
        # start the graphql server
        self.port = port
        self._hge_urls = hge_urls
        self.is_running = False
        self.start_server()

    def start_server(self):
        if not self.is_running:
            self.graphql_server = graphql_server.create_server('127.0.0.1', self.port)
            self.hge_urls = graphql_server.set_hge_urls(self._hge_urls)
            self.gql_srvr_thread = threading.Thread(target=self.graphql_server.serve_forever)
            self.gql_srvr_thread.start()
        self.is_running = True

    def teardown(self):
        self.stop_server()

    def stop_server(self):
        if self.is_running:
            graphql_server.stop_server(self.graphql_server)
            self.gql_srvr_thread.join()
        self.is_running = False

class HGECtx:

    def __init__(self, hge_url, pg_url, config):

        self.http = requests.Session()
        self. hge_key = config.getoption('--hge-key')
        self.hge_url = hge_url
        self.pg_url = pg_url
        self.hge_webhook = config.getoption('--hge-webhook')
        hge_jwt_key_file = config.getoption('--hge-jwt-key-file')
        if hge_jwt_key_file is None:
            self.hge_jwt_key = None
        else:
            with open(hge_jwt_key_file) as f:
                self.hge_jwt_key = f.read()
        self.hge_jwt_conf = config.getoption('--hge-jwt-conf')
        self.webhook_insecure = config.getoption('--test-webhook-insecure')
        self.metadata_disabled = config.getoption('--test-metadata-disabled')
        self.may_skip_test_teardown = False

        self.engine = create_engine(self.pg_url)
        self.meta = MetaData()

        self.ws_read_cookie = config.getoption('--test-ws-init-cookie')

        self.hge_scale_url = config.getoption('--test-hge-scale-url')
        self.avoid_err_msg_checks = config.getoption('--avoid-error-message-checks')

        self.ws_client = GQLWsClient(self, '/v1/graphql')

        # HGE version
        result = subprocess.run(['../../scripts/get-version.sh'], shell=False, stdout=subprocess.PIPE, check=True)
        env_version = os.getenv('VERSION')
        self.version = env_version if env_version else result.stdout.decode('utf-8').strip()
        if not self.metadata_disabled and not config.getoption('--skip-schema-setup'):
          try:
              st_code, resp = self.v1q_f('queries/clear_db.yaml')
          except requests.exceptions.RequestException as e:
              self.teardown()
              raise HGECtxError(repr(e))
          assert st_code == 200, resp

        # Postgres version
        pg_version_text = self.sql('show server_version_num').fetchone()['server_version_num']
        self.pg_version = int(pg_version_text)


    def reflect_tables(self):
        self.meta.reflect(bind=self.engine)

    def anyq(self, u, q, h):
        resp = self.http.post(
            self.hge_url + u,
            json=q,
            headers=h
        )
        # NOTE: make sure we preserve key ordering so we can test the ordering
        # properties in the graphql spec properly
        # Returning response headers to get the request id from response
        return resp.status_code, resp.json(object_pairs_hook=OrderedDict), resp.headers

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
        # NOTE: make sure we preserve key ordering so we can test the ordering
        # properties in the graphql spec properly
        return resp.status_code, resp.json(object_pairs_hook=OrderedDict)

    def v1q_f(self, fn):
        with open(fn) as f:
            # NOTE: preserve ordering with ruamel
            yml = yaml.YAML()
            return self.v1q(yml.load(f))

    def teardown(self):
        self.http.close()
        self.engine.dispose()
