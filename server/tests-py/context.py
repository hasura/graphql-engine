#!/usr/bin/env python3

import graphql
from http import HTTPStatus
import http.server
import json
import os
import queue
import random
import re
import requests
import ruamel.yaml as yaml
from ruamel.yaml.comments import CommentedMap as OrderedDict # to avoid '!!omap' in yaml
import socketserver
import sqlalchemy
import sqlalchemy.schema
import string
import subprocess
import threading
import time
from typing import Any, NamedTuple, Optional
from urllib.parse import urlparse
import websocket

import fixtures.tls
import graphql_server
import ports

# pytest has removed the global pytest.config
# As a solution to this we are going to store it in PyTestConf.config
class PytestConf():
    config: Any
    pass

class HGECtxError(Exception):
    pass

# NOTE: use this to generate a GraphQL client that uses the `Apollo`(subscription-transport-ws) sub-protocol
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

    def send(self, frame, count=0):
        self.wait_for_connection()
        if frame.get('type') == 'stop':
            self.ws_active_query_ids.discard( frame.get('id') )
        elif frame.get('type') == 'start' and 'id' in frame:
            self.ws_id_query_queues[frame['id']] = queue.Queue(maxsize=-1)
        try:
            self._ws.send(json.dumps(frame))
        except websocket.WebSocketConnectionClosedException:
            if count > 2:
                raise websocket.WebSocketConnectionClosedException("Connection is already closed and cannot be recreated even after 3 attempts")
            # Connection closed, try to recreate the connection and send the frame again
            self.recreate_conn()
            self.send(frame, count+1)

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

    def get_conn_close_state(self):
        return self.remote_closed or self.is_closing

    def teardown(self):
        self.is_closing = True
        if not self.remote_closed:
            self._ws.close()
        self.wst.join()

# NOTE: use this to generate a GraphQL client that uses the `graphql-ws` sub-protocol
class GraphQLWSClient():

    def __init__(self, hge_ctx, endpoint):
        self.hge_ctx = hge_ctx
        self.ws_queue = queue.Queue(maxsize=-1)
        self.ws_url = urlparse(hge_ctx.hge_url)._replace(scheme='ws',
                                                         path=endpoint)
        self.create_conn()

    def get_queue(self):
        return self.ws_queue.queue

    def clear_queue(self):
        self.ws_queue.queue.clear()

    def create_conn(self):
        self.ws_queue.queue.clear()
        self.ws_id_query_queues = dict()
        self.ws_active_query_ids = set()

        self.connected_event = threading.Event()
        self.init_done = False
        self.is_closing = False
        self.remote_closed = False

        self._ws = websocket.WebSocketApp(self.ws_url.geturl(),
            on_open=self._on_open, on_message=self._on_message, on_close=self._on_close, subprotocols=["graphql-transport-ws"])
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
        print("HELLO", self.ws_active_query_ids)
        return self.ws_id_query_queues[query_id].get(timeout=timeout)

    def send(self, frame):
        self.wait_for_connection()
        if frame.get('type') == 'complete':
            self.ws_active_query_ids.discard( frame.get('id') )
        elif frame.get('type') == 'subscribe' and 'id' in frame:
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
        ev = self.get_ws_event(5)
        assert ev['type'] == 'connection_ack', ev
        self.init_done = True

    def stop(self, query_id):
        data = {'id': query_id, 'type': 'complete'}
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
            self.clear_queue()
            self.init(headers)
        elif not self.init_done:
            self.init()
        if query_id == None:
            query_id = self.gen_id()
        frame = {
            'id': query_id,
            'type': 'subscribe',
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
        if json_msg['type'] == 'ping':
            new_msg = json_msg
            new_msg['type'] = 'pong'
            # Decline to reflect the payload of the ping, because the
            # graphql-ws specification does not require it
            new_msg.pop('payload')
            self.send(json.dumps(new_msg))
            return

        if 'id' in json_msg:
            query_id = json_msg['id']
            if json_msg.get('type') == 'complete':
                #Remove from active queries list
                self.ws_active_query_ids.discard( query_id )
            if not query_id in self.ws_id_query_queues:
                self.ws_id_query_queues[json_msg['id']] = queue.Queue(maxsize=-1)
            #Put event in the correponding query_queue
            self.ws_id_query_queues[query_id].put(json_msg)

        if json_msg['type'] != 'ping':
            self.ws_queue.put(json_msg)

    def _on_close(self):
        self.remote_closed = True
        self.init_done = False

    def get_conn_close_state(self):
        return self.remote_closed or self.is_closing

    def teardown(self):
        self.is_closing = True
        if not self.remote_closed:
            self._ws.close()
        self.wst.join()

class ActionsWebhookHandler(http.server.BaseHTTPRequestHandler):
    hge_url: str
    hge_key: Optional[str]

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

        elif req_path == "/create-user-timeout":
            time.sleep(3)
            resp, status = self.create_user()
            self._send_response(status, resp)

        elif req_path == "/create-users":
            resp, status = self.create_users()
            self._send_response(status, resp)

        elif req_path == "/create-user-nested":
            resp, status = self.create_user_nested()
            self._send_response(status, resp)

        elif req_path == "/mirror-action":
            resp, status = self.mirror_action()
            self._send_response(status, resp)

        elif req_path == "/mirror-headers":
            resp, status = self.mirror_headers()
            self._send_response(status, resp)

        elif req_path == "/get-user-by-email":
            resp, status = self.get_users_by_email(True)
            self._send_response(status, resp)

        elif req_path == "/get-user-by-email-nested":
            resp, status = self.get_users_by_email_nested(True)
            self._send_response(status, resp)

        elif req_path == "/get-users-by-email":
            resp, status = self.get_users_by_email(False)
            self._send_response(status, resp)

        elif req_path == "/get-users-by-email-nested":
            resp, status = self.get_users_by_email_nested(False)
            self._send_response(status, resp)

        elif req_path == "/intentional-error":
            resp, status = self.intentional_error()
            self._send_response(status, resp)

        elif req_path == "/null-response":
            resp, status = self.null_response()
            self._send_response(status, resp)
        
        elif req_path == "/omitted-response-field":
            self._send_response(
                HTTPStatus.OK,
                self.get_omitted_response_field()
            )

        elif req_path == "/scalar-response":
            self._send_response(HTTPStatus.OK, "some-string")

        elif req_path == "/json-response":
            resp, status = self.json_response()
            self._send_response(status, resp)

        elif req_path == "/custom-scalar-array-response":
            resp, status = self.custom_scalar_array_response()
            self._send_response(status, resp)

        elif req_path == "/scalar-array-response":
            self._send_response(HTTPStatus.OK, ["foo", "bar", None])

        elif req_path == "/recursive-output":
            resp, status = self.recursive_output()
            self._send_response(status, resp)

        elif req_path == "/get-results":
            resp, status = self.get_results()
            self._send_response(status, resp)

        elif req_path == "/typed-nested-null":
            self._send_response(
                HTTPStatus.OK,
                self.get_typed_nested_null()
            )

        elif req_path == "/typed-nested-null-wrong-field":
            self._send_response(
                HTTPStatus.OK,
                self.get_typed_nested_null_wrong_field()
            )

        elif req_path == "/get_messages":
            resp, status = self.get_messages()
            self._send_response(status, resp)

        else:
            self.send_response(HTTPStatus.NO_CONTENT)
            self.end_headers()

    def intentional_error(self):
        blob = self.req_json['input']['blob']
        return blob, HTTPStatus.BAD_REQUEST

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

    def create_user_nested(self):
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

        response = {
            'userObj': resp['data']['insert_user_one']
        }
        return response, HTTPStatus.OK

    def get_messages(self):
        response = [
            { "content": "baz", "user_name": "foo"},
            { "content": "foo", "user_name": "bar"}
        ]
        return response, HTTPStatus.OK

    def mirror_action(self):
        response = self.req_json['input']['arg']
        return response, HTTPStatus.OK

    def mirror_headers(self):
        response = {
            'headers': list(map(lambda header: { 'name': header[0], 'value': header[1] }, self.headers.items()))
        }
        return response, HTTPStatus.OK

    def get_users_by_email(self, singleUser = False):
        email = self.req_json['input']['email']
        if not self.check_email(email):
            response = {
                'message': 'Given email address is not valid',
                'code': 'invalid-email'
            }
            return response, HTTPStatus.BAD_REQUEST
        gql_query = '''
        query get_user($email:String!) {
           user(where:{email:{_eq:$email}},order_by: {id: asc}) {
            id
        }
        }
        '''
        query = {
            'query': gql_query,
            'variables':{
                'email':email
            }
        }
        code,resp = self.execute_query(query)
        if code != 200 or 'data' not in resp:
            response = {
                'message': 'GraphQL query execution failed',
                'code': 'unexpected'
            }
            return response, HTTPStatus.BAD_REQUEST
        if singleUser:
            return resp['data']['user'][0], HTTPStatus.OK
        else:
            return resp['data']['user'], HTTPStatus.OK

    def get_users_by_email_nested(self, singleUser = False):
        resp, status = self.get_users_by_email(singleUser)
        def make_nested_out_object(outObj):
            address = { 'city': 'New York', 'country': 'USA'}
            outObj['address'] = address
            addresses = [{'city': 'Bangalore', 'country': 'India'}, {'city': 'Melbourne', 'country': 'Australia'}]
            outObj['addresses'] = addresses
            outObj['user_id'] = { 'id': outObj['id']}
            return outObj

        if status != HTTPStatus.OK:
            return resp, status
        if singleUser:
            return make_nested_out_object(resp), status
        else:
            return map(make_nested_out_object, resp), status

    def get_typed_nested_null(self):
        return {
            'id': 1,
            'child': None
        }
    
    def get_omitted_response_field(self):
        return {
            'country': 'India'
        }

    def get_typed_nested_null_wrong_field(self):
        return {
            'id': None,
            'child': None
        }

    def null_response(self):
        response = None
        return response, HTTPStatus.OK

    def json_response(self):
        response = {
            'foo': 'bar'
        }
        return response, HTTPStatus.OK

    def custom_scalar_array_response(self):
        response = [{
            'foo': 'bar'
        }]
        return response, HTTPStatus.OK

    def recursive_output(self):
        return {
            'direct': {'id': 1, 'this': {'id': 2, 'this': {'id': 3 }}},
            'list': {'id': 1, 'these': [{'id': 2, 'these': [{'id': 3}]}, {'id': 4}]},
            'mutual': {'id': 1, 'that': {'id': 2, 'other': {'id': 3, 'that': {'id': 4}}}}
        }, HTTPStatus.OK

    def get_results(self):
        return {
            'result_ids': [1,2,3,4]
        }, HTTPStatus.OK

    def check_email(self, email):
        regex = '^\\w+([\\.-]?\\w+)*@\\w+([\\.-]?\\w+)*(\\.\\w{2,3})+$'
        return re.search(regex,email)

    def execute_query(self, query):
        headers = {}
        admin_secret = self.hge_key
        if admin_secret is not None:
            headers['X-Hasura-Admin-Secret'] = admin_secret
        resp = requests.post(
            self.hge_url + '/v1/graphql',
            json=query,
            headers=headers,
            timeout=60,
        )
        data = resp.json(object_pairs_hook=OrderedDict)
        self.log_message(json.dumps(data))
        return resp.status_code, data

    def _send_response(self, status, body):
        self.log_request(status)
        self.send_response_only(status)
        self.send_header('Content-Type', 'application/json')
        self.send_header('Set-Cookie', 'abcd')
        self.end_headers()
        self.wfile.write(json.dumps(body).encode("utf-8"))


class ActionsWebhookServer(http.server.HTTPServer):
    def __init__(self, hge_url, hge_key, server_address):
        handler = ActionsWebhookHandler
        handler.hge_url = hge_url
        handler.hge_key = hge_key
        super().__init__(server_address, handler)

    @property
    def url(self):
        return f'http://{self.server_address[0]}:{self.server_address[1]}'

class EvtsWebhookHandler(http.server.BaseHTTPRequestHandler):
    server: 'EvtsWebhookServer'

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
        # This endpoint just sleeps for 2 seconds:
        elif req_path == "/sleep_2s":
            time.sleep(2)
            self.send_response(HTTPStatus.NO_CONTENT)
            self.end_headers()
        # This is like a sleep endpoint above, but allowing us to decide
        # externally when the webhook can return, with unblock()
        elif req_path == "/block":
            if not self.server.unblocked:
                self.server.blocked_count += 1
                with self.server.unblocked_wait:
                    # We expect this timeout never to be reached, but if
                    # something goes wrong the main thread will block forever:
                    self.server.unblocked_wait.wait(timeout=60)
                self.server.blocked_count -= 1
            self.send_response(HTTPStatus.NO_CONTENT)
            self.end_headers()
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
class ThreadedHTTPServer(socketserver.ThreadingMixIn, http.server.HTTPServer):
    """Handle requests in a separate thread."""

    @property
    def url(self):
        return f'http://{self.server_name}:{self.server_port}'

class EvtsWebhookServer(ThreadedHTTPServer):
    def __init__(self, server_address):
        # Data received from hasura by our web hook, pushed after it returns to the client:
        self.resp_queue = queue.Queue()
        # We use these two vars to coordinate unblocking in the /block route
        self.unblocked = False
        self.unblocked_wait = threading.Condition()
        # ...and this for bookkeeping open blocked requests; this becomes
        # meaningless after the first call to unblock()
        self.blocked_count = 0

        super().__init__(server_address, EvtsWebhookHandler)

    # Unblock all webhook requests to /block. Idempotent.
    def unblock(self):
        self.unblocked = True
        with self.unblocked_wait:
            # NOTE: this only affects currently wait()-ing threads, future
            # wait()s will block again (hence the simple self.unblocked flag)
            self.unblocked_wait.notify_all()

    def get_event(self, timeout):
        return self.resp_queue.get(timeout=timeout)

    def is_queue_empty(self):
        return self.resp_queue.empty

class HGECtxGQLServer:
    def __init__(self, server_address: tuple[str, int], tls_ca_configuration: Optional[fixtures.tls.TLSCAConfiguration] = None, hge_urls: list[str] = []):
        self.server_address = server_address
        self.tls_ca_configuration = tls_ca_configuration
        self.server: Optional[http.server.HTTPServer] = None

    def start_server(self):
        if not self.server:
            self.server = graphql_server.create_server(self.server_address, self.tls_ca_configuration)
            self.thread = threading.Thread(target=self.server.serve_forever)
            self.thread.start()
        # If the port is specified as 0, we will get a different,
        # dynamically-allocated port whenever we restart. This captures the
        # actual assigned port so that we re-use it.
        self.server_address = self.server.server_address
        ports.wait_for_port(self.port)

    def stop_server(self):
        if self.server:
            graphql_server.stop_server(self.server)
            self.thread.join()
            self.server = None

    @property
    def url(self):
        scheme = 'https' if self.tls_ca_configuration else 'http'
        return f'{scheme}://{self.host}:{self.port}'

    @property
    def host(self):
        # We must use 'localhost' and not `self.server.server_address[0]`
        # because when using TLS, we need a domain name, not an IP address.
        return 'localhost'

    @property
    def port(self):
        if not self.server:
            raise Exception('The server is not started.')
        return self.server.server_address[1]

class HGECtxWebhook(NamedTuple):
    tls_trust: Optional[fixtures.tls.TLSTrust]

class HGECtx:

    def __init__(
        self,
        hge_url: str,
        metadata_schema_url: str,
        hge_key: Optional[str],
        webhook: Optional[HGECtxWebhook],
        enabled_apis: Optional[set[str]],
        clear_dbs: bool,
        config,
    ):
        self.http = requests.Session()
        self.timeout = 120  # BigQuery can take a while

        self.hge_url = hge_url
        self.metadata_schema_url = metadata_schema_url
        self.hge_key = hge_key
        self.webhook = webhook
        self.may_skip_test_teardown = False

        # This will be GC'd, but we also explicitly dispose() in teardown()
        self.engine = sqlalchemy.create_engine(self.metadata_schema_url)
        self.meta = sqlalchemy.schema.MetaData()

        self.hge_scale_url = config.getoption('--test-hge-scale-url')
        self.pro_tests = config.getoption('--pro-tests')

        self.ws_client = GQLWsClient(self, '/v1/graphql')
        self.ws_client_v1alpha1 = GQLWsClient(self, '/v1alpha1/graphql')
        self.ws_client_relay = GQLWsClient(self, '/v1beta1/relay')
        self.ws_client_graphql_ws = GraphQLWSClient(self, '/v1/graphql')

        self.backend = config.getoption('--backend')
        self.default_backend = 'postgres'
        self.is_default_backend = self.backend == self.default_backend

        env_version = os.getenv('VERSION')
        if env_version:
            self.version = env_version
        else:
            # HGE version
            result = subprocess.run(['../../scripts/get-version.sh'], shell=False, stdout=subprocess.PIPE, check=True)
            self.version = result.stdout.decode('utf-8').strip()
        # TODO: remove once parallelization work is completed
        if clear_dbs and self.is_default_backend and (not enabled_apis or 'metadata' in enabled_apis):
          try:
              self.v2q_f("queries/" + self.backend_suffix("clear_db")+ ".yaml")
          except requests.exceptions.RequestException as e:
              self.teardown()
              raise HGECtxError(repr(e))

    def reflect_tables(self):
        self.meta.reflect(bind=self.engine)

    def anyq(self, u, q, h, b = None, v = None):

        resp = None
        if v == 'GET':
          resp = self.http.get(
              self.hge_url + u,
              headers=h,
              timeout=self.timeout,
          )
        elif v == 'POSTJSON' and b:
          resp = self.http.post(
              self.hge_url + u,
              json=b,
              headers=h,
              timeout=self.timeout,
          )
        elif v == 'POST' and b:
          # TODO: Figure out why the requests are failing with a byte object passed in as `data`
          resp = self.http.post(
              self.hge_url + u,
              data=b,
              headers=h,
              timeout=self.timeout,
          )
        elif v == 'PATCH' and b:
          resp = self.http.patch(
              self.hge_url + u,
              data=b,
              headers=h,
              timeout=self.timeout,
           )
        elif v == 'PUT' and b:
          resp = self.http.put(
              self.hge_url + u,
              data=b,
              headers=h,
              timeout=self.timeout,
           )
        elif v == 'DELETE':
          resp = self.http.delete(
              self.hge_url + u,
              headers=h,
              timeout=self.timeout,
           )
        else:
          resp = self.http.post(
              self.hge_url + u,
              json=q,
              headers=h,
              timeout=self.timeout,
           )
        # NOTE: make sure we preserve key ordering so we can test the ordering
        # properties in the graphql spec properly
        # Returning response headers to get the request id from response
        return resp.status_code, resp.json(object_pairs_hook=OrderedDict), resp.headers

    # Executes a query, but does not return the result.
    def sql(self, q):
        with self.engine.connect() as conn:
            conn.execute(q)

    def execute_query(self, q, url_path, headers = {}, expected_status_code = 200):
        h = headers.copy()
        if self.hge_key is not None and 'X-Hasura-Admin-Secret' not in headers:
            h['X-Hasura-Admin-Secret'] = self.hge_key
        resp = self.http.post(
            self.hge_url + url_path,
            json=q,
            headers=h,
            timeout=self.timeout,
        )
        # NOTE: make sure we preserve key ordering so we can test the ordering
        # properties in the graphql spec properly
        # Don't assume `resp` is JSON object
        resp_obj = {} if resp.status_code == 500 else resp.json(object_pairs_hook=OrderedDict)
        if expected_status_code:
            assert \
                resp.status_code == expected_status_code, \
                f'Expected {resp.status_code} to be {expected_status_code}.\nRequest:\n{json.dumps(q, indent=2)}\nResponse:\n{json.dumps(resp_obj, indent=2)}'
        return resp_obj

    def v1q(self, q, headers = {}, expected_status_code = 200):
        return self.execute_query(q, "/v1/query", headers, expected_status_code)

    def v1q_f(self, filepath, headers = {}, expected_status_code = 200):
        with open(filepath) as f:
            # NOTE: preserve ordering with ruamel
            yml = yaml.YAML()
            return self.v1q(yml.load(f), headers, expected_status_code)

    def v2q(self, q, headers = {}, expected_status_code = 200):
        return self.execute_query(q, "/v2/query", headers, expected_status_code)

    def v2q_f(self, filepath, headers = {}, expected_status_code = 200):
        with open(filepath) as f:
            # NOTE: preserve ordering with ruamel
            yml = yaml.YAML()
            return self.v2q(yml.load(f), headers, expected_status_code)

    def backend_suffix(self, filename):
        if self.is_default_backend:
            return filename
        else:
            return filename + "_" + self.backend

    def v1metadataq(self, q, headers = {}, expected_status_code = 200):
        return self.execute_query(q, "/v1/metadata", headers, expected_status_code)

    def v1metadataq_f(self, filepath, headers = {}, expected_status_code = 200):
        with open(filepath) as f:
            # NOTE: preserve ordering with ruamel
            yml = yaml.YAML()
            return self.v1metadataq(yml.load(f), headers, expected_status_code)

    def v1graphqlq(self, q, headers = {}, expected_status_code = 200):
        return self.execute_query(q, "/v1/graphql", headers, expected_status_code)

    def v1graphql_f(self, filepath, headers = {}, expected_status_code = 200):
        with open(filepath) as f:
            # NOTE: preserve ordering with ruamel
            yml = yaml.YAML()
            return self.v1graphqlq(yml.load(f), headers, expected_status_code)

    def teardown(self):
        self.http.close()
        self.engine.dispose()
        # Close websockets:
        self.ws_client.teardown()
        self.ws_client_v1alpha1.teardown()
        self.ws_client_relay.teardown()
        self.ws_client_graphql_ws.teardown()

    def v1GraphqlExplain(self, q, headers = {}, expected_status_code = 200):
        return self.execute_query(q, '/v1/graphql/explain', headers, expected_status_code)
