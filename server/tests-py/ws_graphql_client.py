import string
import random
import json
import websocket
import time
import threading


class GraphQLClient():
    """
    A simple GraphQL client that works over Websocket as the transport
    protocol, instead of HTTP.
    This follows the Apollo protocol.
    https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md
    """

    def __init__(self, hge_ctx):
        #self.ws_url = url
        #self._conn = websocket.create_connection(self.ws_url, on_message=self._on_message)
        #self._conn.on_message = self._on_message
        self.ctx = hge_ctx
        self._subscription_running = False
        self._st_id = None

    def conn_init(self, headers=None):
        payload = {
            'type': 'connection_init',
            'payload': {'headers': headers}
        }
        self.ctx.ws.send(json.dumps(payload))

    def start(self, query, id=None, headers={}, variables={}):
        _id = id if id else gen_id()
        payload = {'headers': headers, 'query': query, 'variables': variables}
        frame = {'id': _id, 'type': 'start', 'payload': payload}
        self.ctx.ws.send(json.dumps(frame))
        return _id

    def stop(self, _id):
        payload = {'id': _id, 'type': 'stop'}
        self.ctx.ws.send(json.dumps(payload))

    def query(self, query, variables=None, headers=None):
        self.conn_init(headers)
        payload = {'headers': headers, 'query': query, 'variables': variables}
        _id = self.start(payload)
        #res = self._stop(_id)
        #print(dir(self._conn))
        return self.ctx.get_ws_event(3)

    def close(self):
        self.ctx.ws.close()


# generate random alphanumeric id
def gen_id(size=6, chars=string.ascii_letters + string.digits):
    return ''.join(random.choice(chars) for _ in range(size))
