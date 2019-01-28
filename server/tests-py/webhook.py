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

class S(http.server.BaseHTTPRequestHandler):


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
                        self.wfile.write('{}')
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
        if 'headers' in req_json:
            self.handle_headers(req_json['headers'])
        else:
            self.handler_headers({})

def run(keyfile, certfile, server_class=http.server.HTTPServer, handler_class=S, port=9090):
    server_address = ('', port)
    httpd = server_class(server_address, handler_class)
    httpd.socket = ssl.wrap_socket (
        httpd.socket,
        certfile=certfile,
        keyfile=keyfile,
        server_side=True,
        ssl_version=ssl.PROTOCOL_SSLv23)
    print('Starting httpd...')
    httpd.serve_forever()

if __name__ == "__main__":

    if len(sys.argv) != 4:
        print("Usage: python webhook.py port keyfile certfile")
        sys.exit(1)
    run(keyfile=sys.argv[2],certfile=sys.argv[3], port=int(sys.argv[1]))
