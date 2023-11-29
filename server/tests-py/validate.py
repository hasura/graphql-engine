#!/usr/bin/env python3

import base64
import copy
import graphql
import json
import jsondiff
import os
import pytest
import queue
import random
import ruamel.yaml as yaml
from ruamel.yaml.comments import CommentedMap
from ruamel.yaml.compat import ordereddict, StringIO
import textwrap
import warnings
import PortToHaskell

from context import HGECtx, PytestConf
from fixtures.tls import TLSTrust

def check_keys(keys, obj):
    for k in keys:
        assert k in obj, obj


def check_ev_payload_shape(ev_payload):
    top_level_keys = ["created_at", "event", "id", "table", "trigger"]
    check_keys(top_level_keys, ev_payload)

    event_keys = ["data", "op"]
    check_keys(event_keys, ev_payload['event'])

    trigger_keys = ["name"]
    check_keys(trigger_keys, ev_payload['trigger'])


def validate_event_payload(ev_payload, trig_name, table):
    check_ev_payload_shape(ev_payload)
    assert ev_payload['table'] == table, ev_payload
    assert ev_payload['trigger']['name'] == trig_name, ev_payload


def validate_event_headers(ev_headers, headers):
    for key, value in headers.items():
        v = ev_headers.get(key)
        assert v == value, (key, v)

def validate_removed_event_headers (ev_headers, headers):
    for key in headers:
        v = ev_headers.get(key)
        assert (not v), (key, v)

def validate_event_webhook(ev_webhook_path, webhook_path):
    assert ev_webhook_path == webhook_path

def validate_session_variables(hge_ctx, ev, session_variables):
    # TODO: Naveen: MSSQL ET does not yet contain session variables in it's
    # payload body. Hence ignoring that for now. Remove this guard when payload
    # has it
    if (hge_ctx.backend == "postgres"):
        assert ev['session_variables'] == session_variables, ev
    elif (hge_ctx.backend == "mssql"):
        with pytest.raises(KeyError):
            assert ev['session_variables'] == session_variables, ev

def validate_event(hge_ctx,
                   trig_name,
                   table,
                   operation,
                   ev_full,
                   exp_ev_data,
                   headers = {},
                   webhook_path = '/',
                   session_variables = {'x-hasura-role': 'admin'},
                   retry = 0,
):
    ev = ev_full['body']['event']
    validate_event_webhook(ev_full['path'], webhook_path)
    validate_event_headers(ev_full['headers'], headers)
    validate_event_payload(ev_full['body'], trig_name, table)
    assert ev['op'] == operation, ev
    validate_session_variables(hge_ctx, ev, session_variables)
    assert ev['data'] == exp_ev_data, ev
    assert ev_full['body']['delivery_info']['current_retry'] == retry

# Make some assertions on a single event recorded by webhook. Waits up to 3
# seconds by default for an event to appear
def check_event(hge_ctx,
                evts_webhook,
                trig_name,
                table,
                operation,
                exp_ev_data,
                headers = {},
                webhook_path = '/',
                session_variables = {'x-hasura-role': 'admin'},
                retry = 0,
                get_timeout = 3
):
    ev_full = evts_webhook.get_event(get_timeout)
    validate_event(hge_ctx, trig_name, table, operation, ev_full, exp_ev_data, headers, webhook_path, session_variables, retry)

# Compares the list of expected event data with the one received from webhook. Waits
# up to 3 seconds by default for an event to appear.
# This is useful when the list of events that are generated have no order, so you
# cannot use 'check_event' to check each event one after another.
def check_events(hge_ctx,
                evts_webhook,
                trig_name,
                table,
                operation,
                num_of_events,
                exp_ev_datas,
                headers = {},
                webhook_path = '/',
                session_variables = {'x-hasura-role': 'admin'},
                retry = 0,
                get_timeout = 3
):
    events_payloads_webhook = []

    # Get all the expected number of events
    for i in range(num_of_events):
        ev_full = evts_webhook.get_event(get_timeout)
        ev_payload_data = ev_full['body']['event']['data']
        events_payloads_webhook.append((ev_payload_data, ev_full))

    # If there are still some events present in queue after we get the expected number
    # of events, then it means some stray events got generated.
    if (not evts_webhook.is_queue_empty()):
        assert False, "expected number of event payload not equal to the actual number of event payload generated"

    # Check if the payload we get from event webhook is present in the expected datas
    # If no such data is present, then error else validate the data.
    for (ev_payload_data, ev_full) in events_payloads_webhook:
        if ev_payload_data in exp_ev_datas:
            # Since ev_payload_data is present in exp_ev_datas, is means that it is
            # the exp_ev_data for that particular event payload (ev_full)
            exp_ev_data = ev_payload_data
            validate_event(hge_ctx, trig_name, table, operation, ev_full, exp_ev_data, headers, webhook_path, session_variables, retry)
        else:
            assert False, ("Received event data: \n" + json.dumps(ev_payload_data) + "\n not present in expected event data from webhook")

def check_event_transformed(hge_ctx,
                            evts_webhook,
                            exp_payload,
                            headers = {},
                            webhook_path = '/',
                            session_variables = {'x-hasura-role': 'admin'},
                            retry = 0,
                            get_timeout = 3,
                            removedHeaders = []):
    ev_full = evts_webhook.get_event(get_timeout)
    validate_event_webhook(ev_full['path'], webhook_path)
    validate_event_headers(ev_full['headers'], headers)
    validate_removed_event_headers(ev_full['headers'], removedHeaders)
    assert ev_full['body'] == exp_payload


def test_forbidden_when_admin_secret_reqd(hge_ctx, conf):
    if conf['url'] == '/v1/graphql' or conf['url'] == '/v1beta1/relay':
        if conf['status'] == 404:
            status = [404]
        else:
            status = [200]
    else:
        status = [401, 404]

    headers = {}
    if 'headers' in conf:
        # Convert header values to strings, as the YAML parser might give us an internal class.
        headers = {name: str(value) for name, value in conf['headers'].items()}

    # Test without admin secret
    code, resp, resp_hdrs = hge_ctx.anyq(conf['url'], conf['query'], headers)
    #assert code in [401,404], "\n" + yaml.dump({
    assert code in status, "\n" + yaml.dump({
        "expected": "Should be access denied as admin secret is not provided",
        "actual": {
            "code": code,
            "response": resp
        },
        'request id': resp_hdrs.get('x-request-id')
    })

    # Test with random admin secret
    headers['X-Hasura-Admin-Secret'] = base64.b64encode(os.urandom(30)).decode()
    code, resp, resp_hdrs = hge_ctx.anyq(conf['url'], conf['query'], headers)
    #assert code in [401,404], "\n" + yaml.dump({
    assert code in status, "\n" + yaml.dump({
        "expected": "Should be access denied as an incorrect admin secret is provided",
        "actual": {
            "code": code,
            "response": resp
        },
        'request id': resp_hdrs.get('x-request-id')
    })


def test_forbidden_webhook(hge_ctx, conf):
    if conf['url'] == '/v1/graphql' or conf['url'] == '/v1beta1/relay':
        if conf['status'] == 404:
            status = [404]
        else:
            status = [200]
    else:
        status = [401, 404]

    h = {'Authorization': 'Bearer ' + base64.b64encode(base64.b64encode(os.urandom(30))).decode('utf-8')}
    code, resp, resp_hdrs = hge_ctx.anyq(conf['url'], conf['query'], h)
    #assert code in [401,404], "\n" + yaml.dump({
    assert code in status, "\n" + yaml.dump({
        "expected": "Should be access denied as it is denied from webhook",
        "actual": {
            "code": code,
            "response": resp
        },
        'request id': resp_hdrs.get('x-request-id')
    })

# Returns the response received and a bool indicating whether the test passed
# or not (this will always be True unless we are `--accepting`)
def check_query(hge_ctx: HGECtx, conf, transport='http', add_auth=True, gqlws=False):
    headers = {}
    if 'headers' in conf:
        # Convert header values to strings, as the YAML parser might give us an internal class.
        headers = {name: str(value) for name, value in conf['headers'].items()}

    # No headers in conf => Admin role
    # Set the X-Hasura-Role header randomly
    # If header is set, jwt/webhook auth will happen
    # Otherwise admin-secret will be set
    if len(headers) == 0 and random.choice([True, False]):
        headers['X-Hasura-Role'] = 'admin'

    if add_auth:
        # Use the hasura role specified in the test case, and create an authorization token which will be verified by webhook
        if hge_ctx.webhook and len(headers) > 0:
            if hge_ctx.webhook.tls_trust != TLSTrust.INSECURE:
                # Check whether the output is also forbidden when webhook returns forbidden
                test_forbidden_webhook(hge_ctx, conf)
            headers = authorize_for_webhook(headers)

        # The case as admin with admin-secret and webhook
        elif hge_ctx.webhook \
             and hge_ctx.hge_key is not None \
             and len(headers) == 0:
            headers['X-Hasura-Admin-Secret'] = hge_ctx.hge_key

        # The case as admin with only admin-secret
        elif hge_ctx.hge_key is not None and not hge_ctx.webhook:
            # Test whether it is forbidden when incorrect/no admin_secret is specified
            test_forbidden_when_admin_secret_reqd(hge_ctx, conf)
            headers['X-Hasura-Admin-Secret'] = hge_ctx.hge_key

    assert transport in ['http', 'websocket', 'subscription'], "Unknown transport type " + transport
    if transport == 'http':
        print('running on http')
        if 'allowed_responses' in conf:
            return validate_http_anyq_with_allowed_responses(hge_ctx, conf['url'], conf['query'], headers,
                                      conf['status'], conf.get('allowed_responses'), body=conf.get('body'), method=conf.get('method'))
        else:
            return validate_http_anyq(hge_ctx, conf['url'], conf['query'], headers,
                                      conf['status'], conf.get('response'), conf.get('resp_headers'), body=conf.get('body'), method=conf.get('method'))
    elif transport == 'websocket':
        print('running on websocket')
        return validate_gql_ws_q(hge_ctx, conf, headers, retry=True, gqlws=gqlws)
    elif transport == 'subscription':
        print('running via subscription')
        return validate_gql_ws_q(hge_ctx, conf, headers, retry=True, via_subscription=True, gqlws=gqlws)


def authorize_for_webhook(headers: dict[str, str]) -> dict[str, str]:
    headers['X-Hasura-Auth-Mode'] = 'webhook'
    headers_new = dict()
    headers_new['Authorization'] = 'Bearer ' + base64.b64encode(json.dumps(headers).encode('utf-8')).decode('utf-8')
    return headers_new


def validate_gql_ws_q(hge_ctx, conf, headers, retry=False, via_subscription=False, gqlws=False):
    assert 'response' in conf
    assert conf['url'].endswith('/graphql') or conf['url'].endswith('/relay')
    endpoint = conf['url']
    query = conf['query']
    exp_http_response = conf['response']

    if via_subscription:
        query_text = query['query']
        assert query_text.startswith('query '), query_text
        # make the query into a subscription and add the
        # _multiple_subscriptions directive that enables having more
        # than 1 root field in a subscription
        query['query'] = 'subscription' + query_text[len('query'):].replace("{"," @_multiple_top_level_fields {",1)

    if endpoint == '/v1alpha1/graphql':
        ws_client = hge_ctx.ws_client_v1alpha1
    elif endpoint == '/v1beta1/relay':
        ws_client = hge_ctx.ws_client_relay
    elif gqlws: # for `graphQL-ws` clients
        ws_client = hge_ctx.ws_client_graphql_ws
    else:
        ws_client = hge_ctx.ws_client
    print(ws_client.ws_url)
    if not headers or len(headers) == 0:
        ws_client.init({})

    if ws_client.remote_closed or ws_client.is_closing:
        ws_client.create_conn()
        if not headers or len(headers) == 0 or hge_ctx.hge_key is None:
            ws_client.init()
        else:
            ws_client.init_as_admin()

    query_resp = ws_client.send_query(query, query_id='hge_test', headers=headers, timeout=15)
    resp = next(query_resp)
    print('websocket resp: ', resp)

    if resp.get('type') == 'complete':
        if retry:
            #Got query complete before payload. Retry once more
            print("Got query complete before getting query response payload. Retrying")
            ws_client.recreate_conn()
            return validate_gql_ws_q(hge_ctx, query, headers, exp_http_response, False)
        else:
            assert resp['type'] in ['data', 'error', 'next'], resp

    if 'errors' in exp_http_response or 'error' in exp_http_response:
        if gqlws:
            resp['payload'] = {'errors':resp['payload']}
        assert resp['type'] in ['data', 'error', 'next'], resp
    else:
        assert resp['type'] == 'data' or resp['type'] == 'next', resp
    assert 'payload' in resp, resp

    if via_subscription:
        if not gqlws:
            ws_client.send({ 'id': 'hge_test', 'type': 'stop' })
        else:
            ws_client.send({ 'id': 'hge_test', 'type': 'complete' })
        if not gqlws: # NOTE: for graphql-ws, we have some elements that are left in the queue especially after a 'next' message.
            with pytest.raises(queue.Empty):
                ws_client.get_ws_event(0)
    else:
        resp_done = next(query_resp)
        assert resp_done['type'] == 'complete'

    return assert_graphql_resp_expected(resp['payload'], exp_http_response, query)

def assert_response_code(url, query, code, exp_code, resp, body=None):
    assert code == exp_code, \
        f"""
When querying {url},
Got response code {code}, expected {exp_code}.

Request query:
{textwrap.indent(json.dumps(query, indent=2), '  ')}

Request body:
{textwrap.indent(json.dumps(body, indent=2), '  ')}

Response body:
{textwrap.indent(json.dumps(resp, indent=2), '  ')}
        """

def validate_http_anyq(hge_ctx, url, query, headers, exp_code, exp_response, exp_resp_hdrs, body = None, method = None):
    code, resp, resp_hdrs = hge_ctx.anyq(url, query, headers, body, method)
    assert_response_code(url, query, code, exp_code, resp, body)

    if exp_response:
        return assert_graphql_resp_expected(resp, exp_response, query, resp_hdrs, exp_resp_hdrs=exp_resp_hdrs)
    else:
        return resp, True

def validate_http_anyq_with_allowed_responses(hge_ctx, url, query, headers, exp_code, allowed_responses, body = None, method = None):
    code, resp, resp_hdrs = hge_ctx.anyq(url, query, headers, body, method)
    assert_response_code(url, query, code, exp_code, resp, body)

    if isinstance(allowed_responses, list) and len(allowed_responses) > 0:
        resp_res = {}
        test_passed = False

        for response in allowed_responses:
            dict_resp = json.loads(json.dumps(response))
            exp_resp = dict_resp['response']
            exp_resp_hdrs = dict_resp.get('resp_headers')
            resp_result, pass_test = assert_graphql_resp_expected(resp, exp_resp, query, resp_hdrs, True, exp_resp_hdrs)
            if pass_test == True:
                test_passed = True
                resp_res = resp_result
                break

        if test_passed == True:
            return resp_res, test_passed
        else:
            # test should fail if none of the allowed responses work
            raise Exception("allowed_responses did not contain the response that was expected. Please check your allowed_responses")
    else:
        raise Exception("allowed_responses was not a list of permissible responses")

# Check the actual graphql response is what we expected, also taking into
# consideration the ordering of keys that we expect to be preserved, based on
# 'query'.
#
# Returns 'resp' and a bool indicating whether the test passed or not (this
# will always be True unless we are `--accepting`)
def assert_graphql_resp_expected(resp_orig, exp_response_orig, query, resp_hdrs={}, skip_assertion=False, exp_resp_hdrs={}):
    print('Reponse Headers: ', resp_hdrs)
    print(exp_resp_hdrs)

    # Prepare actual and expected responses so comparison takes into
    # consideration only the ordering that we care about:
    resp         = collapse_order_not_selset(resp_orig,         query)
    exp_response = collapse_order_not_selset(exp_response_orig, query)
    matched      = equal_CommentedMap(resp, exp_response) and (exp_resp_hdrs or {}).items() <= resp_hdrs.items()

    if PytestConf.config.getoption("--accept"):
        print('skipping assertion since we chose to --accept new output')
    else:
        yml = yaml.YAML()
        # https://yaml.readthedocs.io/en/latest/example.html#output-of-dump-as-a-string  :
        dump_str = StringIO()
        test_output = {
            # Keep strict received order when displaying errors:
            'response': resp_orig,
            'expected': exp_response_orig,
            'diff':
              (lambda diff:
                 "(results differ only in their order of keys)" if diff == {} else diff)
              (stringify_keys(jsondiff.diff(exp_response, resp))),
              'query': query
        }
        if 'x-request-id' in resp_hdrs:
            test_output['request id'] = resp_hdrs['x-request-id']
        if exp_resp_hdrs:
            diff_hdrs = {key: val for key, val in resp_hdrs.items() if key in exp_resp_hdrs}
            test_output['headers'] = {
                'actual': dict(resp_hdrs),
                'expected': exp_resp_hdrs,
                'diff': (stringify_keys(jsondiff.diff(exp_resp_hdrs, diff_hdrs)))
            }
        yml.dump(test_output, stream=dump_str)
        if matched:
            return resp, matched
        else:
            def is_err_msg(msg):
                return any(msg.get(x) for x in ['error','errors'])
            def as_list(x):
                return x if isinstance(x, list) else [x]
            # If it is a batch GraphQL query, compare each individual response separately
            for (exp, out) in zip(as_list(exp_response), as_list(resp)):
                matched_ = equal_CommentedMap(exp, out)
                if is_err_msg(exp) and is_err_msg(out):
                    if not matched_:
                        warnings.warn("Response does not have the expected error message\n" + dump_str.getvalue())
                        return resp, matched
                else:
                    if skip_assertion:
                        return resp, matched_
                    else:
                        assert matched_, '\n' + dump_str.getvalue()
    return resp, matched  # matched always True unless --accept

# This really sucks; newer ruamel made __eq__ ignore ordering:
#   https://bitbucket.org/ruamel/yaml/issues/326/commentedmap-equality-no-longer-takes-into
def equal_CommentedMap(m1, m2):
    if isinstance(m1, list) and isinstance(m2, list):
        return (len(m1) == len(m2) and all(equal_CommentedMap(x,y) for x,y in zip(m1,m2)))
    elif isinstance(m1, dict) and isinstance(m2, dict):
        # (see collapse_order_not_selset):
        if isinstance(m1, (ordereddict, CommentedMap)) and \
           isinstance(m2, (ordereddict, CommentedMap)):
            m1_l = list(m1.items())
            m2_l = list(m2.items())
        else:
            m1_l = sorted(list(m1.items()))
            m2_l = sorted(list(m2.items()))
        return (len(m1_l) == len(m2_l) and
                all(k1 == k2 and equal_CommentedMap(v1,v2)
                    for (k1,v1),(k2,v2) in zip(m1_l,m2_l)))
    # else this is a scalar:
    else:
        return m1 == m2

# Parse test case YAML file
def get_conf_f(f):
    with open(f, 'r+') as c:
        return yaml.YAML().load(c)

def check_query_f(hge_ctx, f, transport='http', add_auth=True, gqlws = False):
    hge_ctx.may_skip_test_teardown = False
    should_write_back = False

    def add_spec(file, conf):
        spec = None
        response = conf.get('response')

        status = conf.get('status')
        if status is None:
            status = 200

        query = conf["query"]
        if "query" in query:
            query = query["query"]

        headers = conf.get("headers")

        spec = PortToHaskell.PostSpec(
          conf.get("description"),
          file,
          conf["url"],
          headers,
          query, # TODO: handle variables
          status,
          response)

        PortToHaskell.with_test(hge_ctx.request.cls.__qualname__).add_spec(
            hge_ctx.request._pyfuncitem.originalname,
            spec)

    with open(f, 'r+') as c:
        # ruamel will preserve order so that we can test the JSON ordering
        # property conforms to YAML spec.  It also lets us write back the yaml
        # nicely when we `--accept.`
        yml = yaml.YAML()

        # NOTE: preserve ordering with ruamel
        conf = yml.load(c)

        if isinstance(conf, list):
            for ix, sconf in enumerate(conf):
              if PytestConf.config.getoption("--port-to-haskell"):
                  add_spec(f + " [" + str(ix) + "]", sconf)
              else:
                  actual_resp, matched = check_query(hge_ctx, sconf, transport, add_auth, gqlws)
                  if PytestConf.config.getoption("--accept") and not matched:
                      conf[ix]['response'] = actual_resp
                      should_write_back = True
        else:
            if PytestConf.config.getoption("--port-to-haskell"):
                add_spec(f, conf)
            else:
                if conf['status'] != 200:
                    hge_ctx.may_skip_test_teardown = True
                actual_resp, matched = check_query(hge_ctx, conf, transport, add_auth, gqlws)
                # If using `--accept` write the file back out with the new expected
                # response set to the actual response we got:
                if PytestConf.config.getoption("--accept") and not matched:
                    conf['response'] = actual_resp
                    should_write_back = True

        if should_write_back:
            warnings.warn(
                "\nRecording formerly failing case as correct in: " + f +
                "\n   NOTE: if this case was marked 'xfail' this won't be correct!"
            )
            c.seek(0)
            yml.dump(conf, stream=c)
            c.truncate()


# Return a new dict that discards the object key ordering properties of
# 'result' where the key is not part of the selection set. This lets us compare
# expected and actual results properly with respect to the graphql spec's
# ordering requirements.
def collapse_order_not_selset(result_inp, query):
  # Collapse to unordered dict recursively by roundtripping through json
  def collapse(x):
    return json.loads(json.dumps(x))

  result = copy.deepcopy(result_inp)
  try:
    if 'query' in query:
      gql_query_str = query['query']
      # We don't support multiple operations in the same query yet:
      selset0 = graphql.parse(gql_query_str).definitions[0].selection_set
      def go(result_node, selset):
          for field in selset.selections:
            fname = field.name.value

            # If field has no subfields then all its values can be recursively stripped of ordering.
            # Also if it's an array for some reason (like in 'returning')
            if field.selection_set is None or not isinstance(result_node[fname], (dict, list)):
              result_node[fname] = collapse(result_node[fname])
            elif isinstance(result_node[fname], list):
                for node in result_node[fname]:
                    go(node, field.selection_set)
            else:
              go(result_node[fname], field.selection_set)

      if 'data' in result:
          go(result['data'], selset0)
      # errors is unordered
      if 'errors' in result:
        result['errors'] = collapse(result['errors'])
      # and finally remove ordering at just the topmost level
      return dict(result)
    else:
      # this isn't a graphql query, collapse ordering
      return collapse(result_inp)

  except Exception as e:
    print("Bailing out and collapsing all ordering, due to: ", e)
    return collapse(result)


# Use this since jsondiff seems to produce object/dict structures that can't
# always be serialized to json.
def stringify_keys(d):
    """Recursively convert a dict's keys to strings."""
    if not isinstance(d, dict): return d

    def decode(k):
        if isinstance(k, str): return k
        try:
            return k.decode("utf-8")
        except Exception:
            return repr(k)

    return { decode(k): stringify_keys(v) for k, v in d.items() }
