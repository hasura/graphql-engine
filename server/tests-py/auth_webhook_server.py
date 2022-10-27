"""
Sample auth webhook to receive a cookie and respond
"""
from http import HTTPStatus

from context import ThreadedHTTPServer
from webserver import MkHandlers, RequestHandler, Response

class CookieAuth(RequestHandler):
    def get(self, request):
        print('auth GET request')
        headers = {k.lower(): v for k, v in request.headers.items()}

        print(headers)
        cookieHdrs = []
        if 'cookie' in headers and headers['cookie']:
            res = {'x-hasura-role': 'admin'}

            for k, v in headers.items():
                if 'response-set-cookie' in k:
                    hdr = ('Set-Cookie', v)
                    cookieHdrs.append(hdr)

            print('auth response: OK')
            return Response(HTTPStatus.OK, res, cookieHdrs)
        print('auth response: Unauthorized')
        return Response(HTTPStatus.UNAUTHORIZED)

    def post(self, request):
        print('auth POST request')
        headers = {k.lower(): v for k, v in request.json['headers'].items()}
        cookieHdrs = []

        if 'cookie' in headers and headers['cookie']:
            res = {'x-hasura-role': 'admin'}

            for k, v in headers.items():
                if 'response-set-cookie' in k:
                    hdr = ('Set-Cookie', v)
                    cookieHdrs.append(hdr)

            print('auth response: OK')
            return Response(HTTPStatus.OK, res, headers)
        print('auth response: Unauthorized')
        return Response(HTTPStatus.UNAUTHORIZED)


handlers = MkHandlers({
    '/auth': CookieAuth,
})

def create_server(server_address):
    return ThreadedHTTPServer(server_address, handlers)

def stop_server(server):
    server.shutdown()
    server.server_close()
