#!/usr/bin/env python
"""
Webhook for GraphQL engine
For successful authentication
1) Add header X-Hasura-Auth-From: webhook to the list of  headers
2) Base64 encode the required headers (including X-Hasura-Auth-From)
3) Pass it as the bearer token with the Authorization header
"""
import base64
import json
import ssl
import http.server
import traceback
import sys

class Handler(http.server.BaseHTTPRequestHandler):


    def handle_headers(self, headers):
        if 'Authorization' in headers:
            auth = headers['Authorization']
            h = dict()
            if auth.startswith("Bearer "):
                try:
                    h = json.loads(base64.b64decode(auth[7:]).decode("utf-8"))
                    if h.get('X-Hasura-Auth-Mode') == 'webhook':
                        print (h)
                        self.send_response(200)
                        self.send_header('Content-Type', 'application/json')
                        self.end_headers()
                        self.wfile.write(json.dumps(h).encode('utf-8'))
                    else:
                        print ('forbidden')
                        self.send_response(401)
                        self.end_headers()
                        self.wfile.write(b'{}')
                except  Exception as e:
                    print ('forbidden')
                    self.send_response(401)
                    self.end_headers()
                    print("type error: " + str(e))
                    print(traceback.format_exc())
        else:
            self.send_response(401)
            self.end_headers()
            print ('forbidden')

    def do_GET(self):
        self.handle_headers(self.headers)

    def do_POST(self):
        content_len = self.headers.get('Content-Length')
        req_body = self.rfile.read(int(content_len)).decode("utf-8")
        req_json = json.loads(req_body)
        self.handle_headers(req_json.get('headers', {}))
