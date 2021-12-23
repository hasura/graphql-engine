# A "fake" JWK server. Which returns `Cache-Control` and `Expires` headers in its response
# This is useful for testing our `jwk_url` behaviour

import datetime
import requests
from http import HTTPStatus

from webserver import RequestHandler, WebServer, MkHandlers, Response

def mkJSONResp(json_result):
    return Response(HTTPStatus.OK, json_result, {'Content-Type': 'application/json'})

state = {
    'cache-control': 0,
    'expires': 0
}

class JwkExpiresHandler(RequestHandler):
    expires_in_secs = 3
    jwk_url = 'https://www.googleapis.com/service_accounts/v1/jwk/securetoken@system.gserviceaccount.com'
    def post(self, request):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)

    def get(self, request):
        # fetch a valid JWK from google servers - this seemed easier than
        # generating key pairs and then constructing a JWK JSON response
        jwk_resp = requests.get(self.jwk_url)
        res = jwk_resp.json()
        expiry = datetime.datetime.utcnow() + datetime.timedelta(seconds=self.expires_in_secs)
        resp = mkJSONResp(res)
        if request.qs and 'error' in request.qs and 'true' in request.qs['error']:
            resp.headers['Expires'] = 'invalid-value'
        else:
            resp.headers['Expires'] = datetime.datetime.strftime(expiry, "%a, %d %b %Y %T GMT")
        state['expires'] += 1
        return resp

class JwkCacheControlHandler(RequestHandler):
    expires_in_secs = str(3)
    jwk_url = 'https://www.googleapis.com/service_accounts/v1/jwk/securetoken@system.gserviceaccount.com'
    def post(self, request):
        return Response(HTTPStatus.METHOD_NOT_ALLOWED)

    def get(self, request):
        # fetch a valid JWK from google servers - this seemed easier than
        # generating key pairs and then constructing a JWK JSON response
        jwk_resp = requests.get(self.jwk_url)
        res = jwk_resp.json()
        header_val = 'max-age=' + self.expires_in_secs
        # see if query string contains 'smaxage', then we return `s-maxage` else `maxage`
        # https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Cache-Control
        if request.qs:
            if 'error' in request.qs and 'true' in request.qs['error']:
                header_val = 'invalid-header-value=42'
            elif 'nocache' in request.qs:
                header_val = 'no-cache'
            elif 'nomaxage' in request.qs:
                header_val = 'public, must-revalidate=123, no-transform'
            elif 'field' in request.qs and 'smaxage' in request.qs['field']:
                header_val = 's-maxage=' + self.expires_in_secs
            if 'field' in request.qs and 'smaxage' in request.qs['field']:
                header_val = 's-maxage=' + self.expires_in_secs
        resp = mkJSONResp(res)
        resp.headers['Cache-Control'] = header_val
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

handlers = MkHandlers({
    # sending query string: `?field=smaxage`, will return Cache-Control with s-maxage, else with max-age
    # sending query string: `error=true` will respond with invalid header value
    '/jwk-cache-control': JwkCacheControlHandler,
    # sending query string: `error=true` will respond with invalid header value
    '/jwk-expires': JwkExpiresHandler,
    # API so that testing can be done
    '/state': StateHandler
})

def create_server(host='127.0.0.1', port=5001):
    return WebServer((host, port), handlers)

def stop_server(server):
    server.shutdown()
    server.server_close()

# if you want to run this module to emulate a JWK server during development
if __name__ == '__main__':
    s = create_server(port=5001)
    s.serve_forever()
    stop_server(s)
