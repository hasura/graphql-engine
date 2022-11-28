import http
import http.server
import json
import pytest
import queue
import threading

from conftest import extract_server_address_from


class QueryEchoWebhookServer(http.server.HTTPServer):
    def __init__(self, server_address):
        # TODO why maxsize=1
        self.resp_queue = queue.Queue(maxsize=1)
        super().__init__(server_address, QueryEchoWebhookHandler)

    def get_event(self, timeout):
        return self.resp_queue.get(timeout=timeout)


class QueryEchoWebhookHandler(http.server.BaseHTTPRequestHandler):
    server: QueryEchoWebhookServer

    def do_GET(self):
        self.log_message("get")
        self.send_response(http.HTTPStatus.OK)
        self.end_headers()

    def do_POST(self):
        self.log_message("post")
        content_len = self.headers.get("Content-Length")
        req_body = self.rfile.read(int(content_len)).decode("utf-8")
        req_json = json.loads(req_body)
        print(json.dumps(req_json))
        user_id_header = req_json["headers"]["auth-user-id"]
        user_vars = {
            "x-hasura-role":"user",
            "x-hasura-user-id": user_id_header
        }
        self.send_response(http.HTTPStatus.OK)
        self.send_header('Content-Type', 'application/json')
        self.end_headers()
        self.server.resp_queue.put({
            "request": req_json["request"],
            "headers": user_vars,
        })
        self.wfile.write(json.dumps(user_vars).encode('utf-8'))


@pytest.fixture(scope='class')
@pytest.mark.early
def query_echo_webhook(hge_fixture_env: dict[str, str]):
    server_address = extract_server_address_from('HASURA_GRAPHQL_AUTH_HOOK')
    server = QueryEchoWebhookServer(server_address)
    thread = threading.Thread(target=server.serve_forever)
    thread.start()
    url = f'http://{server.server_address[0]}:{server.server_address[1]}'
    print(f'{query_echo_webhook.__name__} server started on {url}')
    hge_fixture_env['HASURA_GRAPHQL_AUTH_HOOK'] = url + '/'
    hge_fixture_env['HASURA_GRAPHQL_AUTH_HOOK_MODE'] = 'POST'
    yield server
    server.shutdown()
    server.server_close()
    thread.join()


@pytest.mark.usefixtures('per_method_tests_db_state')
@pytest.mark.admin_secret
class TestWebhookRequestContext(object):
    @classmethod
    def dir(cls):
        return "queries/webhooks/request_context"

    def test_query(self, hge_ctx, query_echo_webhook):
        query = """
          query allUsers {
            users {
              id
              name
            }
          }
        """
        query_obj = {
            "query": query,
            "operationName": "allUsers"
        }
        headers = dict()
        headers['auth-user-id'] = '1'
        code, resp, _ = hge_ctx.anyq('/v1/graphql', query_obj, headers)
        assert code == 200, resp

        ev_full = query_echo_webhook.get_event(3)
        assert ev_full['request'] == query_obj

    def test_query_invalid(self, hge_ctx, query_echo_webhook):
        """
        Even when an invalid query is sent, the webhook should still resolve
        the user id header correctly
        """
        query_obj = "invalid-query"
        user_id_header = '1'
        headers = dict()
        headers['auth-user-id'] = user_id_header
        code, resp, _ = hge_ctx.anyq('/v1/graphql', query_obj, headers)
        assert code == 200, resp

        ev_full = query_echo_webhook.get_event(3)
        assert ev_full['headers']['x-hasura-user-id'] == user_id_header

    def test_mutation_with_vars(self, hge_ctx, query_echo_webhook):
        query = """
          mutation insert_single_user($id: Int!, $name: String!) {
            insert_users_one(
              object: {
                id: $id,
                name: $name
              }
            ) {
              id
              name
            }
          }
        """
        variables = {"id": 4, "name": "danish"}
        query_obj = {"query": query, "variables": variables, "operationName": "insert_single_user"}
        headers = dict()
        headers['auth-user-id'] = '4'
        code, resp, _ = hge_ctx.anyq('/v1/graphql', query_obj, headers)
        assert code == 200, resp

        ev_full = query_echo_webhook.get_event(3)
        exp_result = {"request": query_obj}
        assert ev_full['request'] == query_obj
