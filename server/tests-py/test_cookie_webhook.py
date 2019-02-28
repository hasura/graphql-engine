"""
Sample auth webhook to receive a cookie and respond
"""
from http import HTTPStatus
from webserver import RequestHandler, WebServer, MkHandlers, Response

class CookieAuth(RequestHandler):
    def get(self, request):
        headers = {k.lower(): v for k, v in request.headers.items()}
        print(headers)
        if 'cookie' in headers and headers['cookie']:
            res = {'x-hasura-role': 'admin'}
            return Response(HTTPStatus.OK, res)
        return Response(HTTPStatus.UNAUTHORIZED)

    def post(self, request):
        headers = {k.lower(): v for k, v in request.json['headers'].items()}
        print(headers)
        if 'cookie' in headers and headers['cookie']:
            res = {'x-hasura-role': 'admin'}
            return Response(HTTPStatus.OK, res)
        return Response(HTTPStatus.UNAUTHORIZED)


handlers = MkHandlers({
    '/auth': CookieAuth,
})

def create_server(host='127.0.0.1', port=9876):
    return WebServer((host, port), handlers)

def stop_server(server):
    server.shutdown()
    server.server_close()

if __name__ == '__main__':
    s = create_server(host='0.0.0.0')
    s.serve_forever()
