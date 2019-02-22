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

    def __init__(self, url):
        self.ws_url = url
        self._conn = websocket.create_connection(self.ws_url, on_message=self._on_message)
        self._conn.on_message = self._on_message
        self._subscription_running = False
        self._st_id = None

    # def _ws_init(self, on_open):
    #     self._conn = websocket.WebSocketApp(
    #         self.ws_url,
    #         on_message=self._on_message,
    #         on_open=on_open
    #     )

    # def run(self):
    #     self._conn.run_forever()

    def _on_message(self, ws, message):
        print('recvd msg: ', message)
        data = json.loads(message)
        if data['type'] != 'ka':
            print('type is not ka: ', message)

    def _conn_init(self, headers=None):
        payload = {
            'type': 'connection_init',
            'payload': {'headers': headers}
        }
        self._conn.send(json.dumps(payload))
        self._conn.recv()

    def _start(self, payload, id=None):
        _id = id if id else gen_id()
        frame = {'id': _id, 'type': 'start', 'payload': payload}
        self._conn.send(json.dumps(frame))
        self._conn.recv()
        return _id

    def _stop(self, _id):
        payload = {'id': _id, 'type': 'stop'}
        self._conn.send(json.dumps(payload))
        return self._conn.recv()

    def query(self, query, variables=None, headers=None):
        self._conn_init(headers)
        payload = {'headers': headers, 'query': query, 'variables': variables}
        _id = self._start(payload)
        self._conn.recv()
        res = self._stop(_id)
        #print(dir(self._conn))
        return res

    def subscribe(self, query, variables=None, headers=None, callback=None):
        self._conn_init(headers)
        payload = {'headers': headers, 'query': query, 'variables': variables}
        _cc = self._on_message if not callback else callback
        _id = self._start(payload)
        def subs(_cc):
            self._subscription_running = True
            while self._subscription_running:
                r = json.loads(self._conn.recv())
                if r['type'] == 'error' or r['type'] == 'complete':
                    print(r)
                    self.stop_subscribe(_id)
                    break
                elif r['type'] != 'ka':
                    _cc(_id, r)
                time.sleep(1)

        self._st_id = threading.Thread(target=subs, args=(_cc,))
        self._st_id.start()
        return _id

    def stop_subscribe(self, _id):
        self._subscription_running = False
        self._st_id.join()
        self._stop(_id)

    def close(self):
        self._conn.close()


# generate random alphanumeric id
def gen_id(size=6, chars=string.ascii_letters + string.digits):
    return ''.join(random.choice(chars) for _ in range(size))
