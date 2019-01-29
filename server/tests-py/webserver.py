# -*- coding: utf-8 -*-

"""
    Helper module which exposes abstractions to write webservers easily
"""

from abc import ABC, abstractmethod
import socket
import http.server as http
from http import HTTPStatus
from urllib.parse import parse_qs, urlparse
import json


class Response():
    """ Represents a HTTP `Response` object """
    def __init__(self, status, body=None, headers=None):
        if not isinstance(status, HTTPStatus):
            raise TypeError('status has to be of type http.HTTPStatus')
        if body and not isinstance(body, (str, dict)):
            raise TypeError('body has to be of type str or dict')
        if headers and not isinstance(headers, dict):
            raise TypeError('headers has to be of type dict')
        self.status = status
        self.body = body
        self.headers = headers

    def get_body(self):
        if not self.body:
            return ''
        if isinstance(self.body, dict):
            return json.dumps(self.body)
        return self.body

class Request():
    """ Represents a HTTP `Request` object """
    def __init__(self, path, qs=None, body=None, json=None, headers=None):
        self.path = path
        self.qs = qs
        self.body = body
        self.json = json
        self.headers = headers


class RequestHandler(ABC):
    """
    The class that users should sub-class and provide implementation. Each of
    these functions **should** return an instance of the `Response` class
    """
    @abstractmethod
    def get(self, request):
        pass
    @abstractmethod
    def post(self, request):
        pass


def MkHandlers(handlers):
    class HTTPHandler(http.BaseHTTPRequestHandler):
        def not_found(self):
            self.send_response(HTTPStatus.NOT_FOUND)
            self.end_headers()
            self.wfile.write('<h1> Not Found </h1>'.encode('utf-8'))

        def parse_path(self):
            return urlparse(self.path)

        def append_headers(self, headers):
            for k, v in headers.items():
                self.send_header(k, v)

        def do_GET(self):
            try:
                raw_path = self.parse_path()
                path = raw_path.path
                handler = handlers[path]()
                qs = parse_qs(raw_path.query)
                req = Request(path, qs, None, None, self.headers)
                resp = handler.get(req)
                self.send_response(resp.status)
                if resp.headers:
                    self.append_headers(resp.headers)
                self.end_headers()
                self.wfile.write(resp.get_body().encode('utf-8'))
            except KeyError:
                self.not_found()

        def do_POST(self):
            try:
                raw_path = self.parse_path()
                path = raw_path.path
                handler = handlers[path]()
                content_len = self.headers.get('Content-Length')
                qs = None
                req_body = self.rfile.read(int(content_len)).decode("utf-8")
                req_json = None
                if self.headers.get('Content-Type') == 'application/json':
                    req_json = json.loads(req_body)
                req = Request(self.path, qs, req_body, req_json, self.headers)
                resp = handler.post(req)
                self.send_response(resp.status)
                if resp.headers:
                    self.append_headers(resp.headers)
                #Required for graphiql to work with the graphQL test server
                self.send_header('Access-Control-Allow-Origin', self.headers['Origin'])
                self.send_header('Access-Control-Allow-Credentials', 'true')
                self.send_header('Access-Control-Allow-Methods', 'GET,POST,PUT,PATCH,DELETE,OPTIONS')
                self.end_headers()
                self.wfile.write(resp.get_body().encode('utf-8'))
            except KeyError:
                self.not_found()

        def do_OPTIONS(self):
            self.send_response(204)
            #Required for graphiql to work with the graphQL test server
            self.send_header('Access-Control-Allow-Methods', 'GET, POST, OPTIONS')
            self.send_header('Access-Control-Max-Age', '1728000')
            self.send_header('Access-Control-Allow-Headers', 'content-type,x-apollo-tracing')
            self.send_header('Content-Type', 'text/plain charset=UTF-8')
            self.send_header('Access-Control-Allow-Credentials', 'true')
            self.send_header('Access-Control-Allow-Origin', self.headers['Origin'])
            self.send_header('Access-Control-Allow-Methods', 'GET,POST,PUT,PATCH,DELETE,OPTIONS')
            self.end_headers()

    return HTTPHandler


class WebServer(http.HTTPServer):
    def __init__(self, server_address, handler):
        super().__init__(server_address, handler)

    def server_bind(self):
        print('Running http server on {0}:{1}'.format(self.server_address[0],
                                                      self.server_address[1]))
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.socket.bind(self.server_address)
