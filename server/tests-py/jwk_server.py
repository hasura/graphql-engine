# A "fake" JWK server. Which returns `Cache-Control` and `Expires` headers in its response
# This is useful for testing our `jwk_url` behaviour

import datetime
from http import HTTPStatus
import http.server
import requests

from webserver import MkHandlers, RequestHandler, Response

def mkJSONResp(json_result):
    return Response(HTTPStatus.OK, json_result, {'Content-Type': 'application/json'})

# fetch a valid JWK from google servers - this seemed easier than
# generating key pairs and then constructing a JWK JSON response
jwk_url = 'https://www.googleapis.com/service_accounts/v1/jwk/securetoken@system.gserviceaccount.com'
state = {
    'cache-control': 0,
    'expires': 0
}

class JwkExpiresHandler(RequestHandler):
    def post(self, request):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)

    def get(self, request):
        jwk_resp = requests.get(jwk_url)
        res = jwk_resp.json()

        resp = mkJSONResp(res)
        if request.qs and 'error' in request.qs and 'true' in request.qs['error']:
            resp.headers['Expires'] = 'invalid-value'
        else:
            if request.qs and 'seconds' in request.qs and len(request.qs['seconds']) > 0:
                expires_in_secs = int(request.qs['seconds'][0])
            else:
                expires_in_secs = 3
            expiry = datetime.datetime.utcnow() + datetime.timedelta(seconds=expires_in_secs)
            resp.headers['Expires'] = datetime.datetime.strftime(expiry, "%a, %d %b %Y %T GMT")

        state['expires'] += 1
        return resp

class JwkCacheControlHandler(RequestHandler):
    def post(self, request):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)

    def get(self, request):
        jwk_resp = requests.get(jwk_url)
        res = jwk_resp.json()
        header_vals = []
        if request.qs:
            for param in request.qs:
                if len(request.qs[param]) > 0:
                    val = request.qs[param][0]
                    if val == 'true':
                        header_vals.append(param)
                    elif val.isnumeric():
                        header_vals.append(param + "=" + val)

        resp = mkJSONResp(res)
        resp.headers['Cache-Control'] = ", ".join(header_vals)
        # HGE should always prefer Cache-Control over Expires header
        expiry = datetime.datetime.utcnow() + datetime.timedelta(seconds=600)
        resp.headers['Expires'] = datetime.datetime.strftime(expiry, "%a, %d %b %Y %T GMT")
        state['cache-control'] += 1
        return resp

class StateHandler(RequestHandler):
    def post(self, request):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)

    def get(self, request):
        resp = mkJSONResp(state)
        return resp

class ResetStateHandler(RequestHandler):
    def post(self, request):
        state['cache-control'] = 0
        state['expires'] = 0
        return Response(HTTPStatus.OK)

    def get(self, request):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)

handlers = MkHandlers({
    # sending query string: `?field=smaxage`, will return Cache-Control with s-maxage, else with max-age
    # sending query string: `error=true` will respond with invalid header value
    '/jwk-cache-control': JwkCacheControlHandler,
    # sending query string: `error=true` will respond with invalid header value
    '/jwk-expires': JwkExpiresHandler,
    # API so that testing can be done
    '/state': StateHandler,
    # Resets the state back to zeros
    '/reset-state': ResetStateHandler
})

def create_server(server_address):
    return http.server.HTTPServer(server_address, handlers)

def stop_server(server):
    server.shutdown()
    server.server_close()

# if you want to run this module to emulate a JWK server during development
if __name__ == '__main__':
    s = create_server(('localhost', 5001))
    s.serve_forever()
    stop_server(s)
