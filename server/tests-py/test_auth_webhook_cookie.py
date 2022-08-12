import json
import threading
from urllib.parse import urlparse

import websocket
import pytest
from validate import check_query
from context import PytestConf

if not PytestConf.config.getoption("--hge-webhook"):
    pytest.skip("--hge-webhook flag is missing, skipping tests", allow_module_level=True)

if not PytestConf.config.getoption("--test-auth-webhook-header"):
    pytest.skip("--test-auth-webhook-header flag is missing, skipping tests", allow_module_level=True)

@pytest.mark.usefixtures('per_class_tests_db_state')
class TestWebhookHeaderCookie(object):
    '''
        To run the test, run an instance of the auth_webhook server using `python3 auth_webhook_server.py`
    '''
    @classmethod
    def dir(cls):
        return 'webhook/insecure'

    def test_single_set_cookie_header_in_response(self, hge_ctx):
        query = """
          query allUsers {
            author {
              id
              name
            }
          }
        """

        query_obj = {
            "query": query,
            "operationName": "allUsers"
        }

        headers = {}

        headers['cookie'] = "Test"
        headers['response-set-cookie-1'] = "__Host-id=1; Secure; Path=/; Domain=example.com"

        code, resp, respHeaders = hge_ctx.anyq('/v1/graphql', query_obj, headers)
        print("Status Code: ", code)
        print("Response: ", resp)
        print("Headers: ", respHeaders)
        
        assert 'Set-Cookie' in respHeaders
        assert respHeaders['Set-Cookie'] == "__Host-id=1; Secure; Path=/; Domain=example.com"

    def test_duplicate_set_cookie_header_in_response(self, hge_ctx):
        query = """
          query allUsers {
            author {
              id
              name
            }
          }
        """

        query_obj = {
            "query": query,
            "operationName": "allUsers"
        }

        headers = {}

        headers['cookie'] = "Test"
        headers['response-set-cookie-1'] = "__Host-id=1; Secure; Path=/; Domain=example1.com"
        headers['response-set-cookie-2'] = "__Host-id=2; Secure; Path=/; Domain=example2.com"

        code, resp, respHeaders = hge_ctx.anyq('/v1/graphql', query_obj, headers)
        print("Status Code: ", code)
        print("Response: ", resp)
        print("Headers: ", respHeaders)
        
        assert 'Set-Cookie' in respHeaders

        # In python, multiple headers with the same key are concatenated with a comma and
        # then sent back in the response. Refer to: https://github.com/psf/requests/issues/4520
        assert respHeaders['Set-Cookie'] == "__Host-id=2; Secure; Path=/; Domain=example2.com, __Host-id=1; Secure; Path=/; Domain=example1.com"