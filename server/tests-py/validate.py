#!/usr/bin/env python3

import ruamel.yaml as yaml
from ruamel.yaml.compat import ordereddict, StringIO
from ruamel.yaml.comments import CommentedMap
import json
import copy
import graphql
import os
import base64
import jsondiff
import jwt
import random
import warnings

from context import GQLWsClient, PytestConf

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


def validate_event_webhook(ev_webhook_path, webhook_path):
    assert ev_webhook_path == webhook_path


def check_event(hge_ctx, evts_webhook, trig_name, table, operation, exp_ev_data,
                headers = {},
                webhook_path = '/',
                session_variables = {'x-hasura-role': 'admin'}
):
    ev_full = evts_webhook.get_event(3)
    validate_event_webhook(ev_full['path'], webhook_path)
    validate_event_headers(ev_full['headers'], headers)
    validate_event_payload(ev_full['body'], trig_name, table)
    ev = ev_full['body']['event']
    assert ev['op'] == operation, ev
    assert ev['session_variables'] == session_variables, ev
    assert ev['data'] == exp_ev_data, ev


def test_forbidden_when_admin_secret_reqd(hge_ctx, conf):
    if conf['url'] == '/v1/graphql':
        if conf['status'] == 404:
            status = [404]
        else:
            status = [200]
    else:
        status = [401, 404]

    headers = {}
    if 'headers' in conf:
        headers = conf['headers']

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
    headers['X-Hasura-Admin-Secret'] = base64.b64encode(os.urandom(30))
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
    if conf['url'] == '/v1/graphql':
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
def check_query(hge_ctx, conf, transport='http', add_auth=True):
    hge_ctx.tests_passed = True
    headers = {}
    if 'headers' in conf:
        headers = conf['headers']

    # No headers in conf => Admin role
    # Set the X-Hasura-Role header randomly
    # If header is set, jwt/webhook auth will happen
    # Otherwise admin-secret will be set
    if len(headers) == 0 and random.choice([True, False]):
        headers['X-Hasura-Role'] = 'admin'

    if add_auth:
        #Use the hasura role specified in the test case, and create a JWT token
        if hge_ctx.hge_jwt_key is not None and len(headers) > 0 and 'X-Hasura-Role' in headers:
            hClaims = dict()
            hClaims['X-Hasura-Allowed-Roles'] = [headers['X-Hasura-Role']]
            hClaims['X-Hasura-Default-Role'] = headers['X-Hasura-Role']
            for key in headers:
                if key != 'X-Hasura-Role':
                    hClaims[key] = headers[key]
            claim = {
                "sub": "foo",
                "name": "bar",
                "https://hasura.io/jwt/claims": hClaims
            }
            headers['Authorization'] = 'Bearer ' + jwt.encode(claim, hge_ctx.hge_jwt_key, algorithm='RS512').decode(
                'UTF-8')

        #Use the hasura role specified in the test case, and create an authorization token which will be verified by webhook
        if hge_ctx.hge_webhook is not None and len(headers) > 0:
            if not hge_ctx.webhook_insecure:
            #Check whether the output is also forbidden when webhook returns forbidden
                test_forbidden_webhook(hge_ctx, conf)
            headers['X-Hasura-Auth-Mode'] = 'webhook'
            headers_new = dict()
            headers_new['Authorization'] = 'Bearer ' + base64.b64encode(json.dumps(headers).encode('utf-8')).decode(
                'utf-8')
            headers = headers_new

        #The case as admin with admin-secret and jwt/webhook
        elif (
                hge_ctx.hge_webhook is not None or hge_ctx.hge_jwt_key is not None) and hge_ctx.hge_key is not None and len(
                headers) == 0:
            headers['X-Hasura-Admin-Secret'] = hge_ctx.hge_key

        #The case as admin with only admin-secret
        elif hge_ctx.hge_key is not None and hge_ctx.hge_webhook is None and hge_ctx.hge_jwt_key is None:
            #Test whether it is forbidden when incorrect/no admin_secret is specified
            test_forbidden_when_admin_secret_reqd(hge_ctx, conf)
            headers['X-Hasura-Admin-Secret'] = hge_ctx.hge_key

    assert transport in ['websocket', 'http'], "Unknown transport type " + transport
    if transport == 'websocket':
        assert 'response' in conf
        assert conf['url'].endswith('/graphql')
        print('running on websocket')
        return validate_gql_ws_q(
            hge_ctx,
            conf['url'],
            conf['query'],
            headers,
            conf['response'],
            True
        )
    elif transport == 'http':
        print('running on http')
        return validate_http_anyq(hge_ctx, conf['url'], conf['query'], headers,
                                  conf['status'], conf.get('response'))



def validate_gql_ws_q(hge_ctx, endpoint, query, headers, exp_http_response, retry=False):
    if endpoint == '/v1alpha1/graphql':
        ws_client = GQLWsClient(hge_ctx, '/v1alpha1/graphql')
    else:
        ws_client = hge_ctx.ws_client
    print(ws_client.ws_url)
    if not headers or len(headers) == 0:
        ws_client.init({})

    query_resp = ws_client.send_query(query, headers=headers, timeout=15)
    resp = next(query_resp)
    print('websocket resp: ', resp)

    if resp.get('type') == 'complete':
        if retry:
            #Got query complete before payload. Retry once more
            print("Got query complete before getting query response payload. Retrying")
            ws_client.recreate_conn()
            return validate_gql_ws_q(hge_ctx, query, headers, exp_http_response, False)
        else:
            assert resp['type'] in ['data', 'error'], resp

    if 'errors' in exp_http_response or 'error' in exp_http_response:
        assert resp['type'] in ['data', 'error'], resp
    else:
        assert resp['type'] == 'data', resp

    assert 'payload' in resp, resp
    resp_done = next(query_resp)
    assert resp_done['type'] == 'complete'

    return assert_graphql_resp_expected(resp['payload'], exp_http_response, query, skip_if_err_msg=hge_ctx.avoid_err_msg_checks)


def validate_http_anyq(hge_ctx, url, query, headers, exp_code, exp_response):
    code, resp, resp_hdrs = hge_ctx.anyq(url, query, headers)
    print(headers)
    assert code == exp_code, resp
    print('http resp: ', resp)
    if exp_response:
        return assert_graphql_resp_expected(resp, exp_response, query, resp_hdrs, hge_ctx.avoid_err_msg_checks)
    else:
        return resp, True

# Check the actual graphql response is what we expected, also taking into
# consideration the ordering of keys that we expect to be preserved, based on
# 'query'.
#
# Returns 'resp' and a bool indicating whether the test passed or not (this
# will always be True unless we are `--accepting`)
def assert_graphql_resp_expected(resp_orig, exp_response_orig, query, resp_hdrs={}, skip_if_err_msg=False):
    # Prepare actual and respected responses so comparison takes into
    # consideration only the ordering that we care about:
    resp         = collapse_order_not_selset(resp_orig,         query)
    exp_response = collapse_order_not_selset(exp_response_orig, query)
    matched = equal_CommentedMap(resp, exp_response)

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
        yml.dump(test_output, stream=dump_str)
        if not skip_if_err_msg:
            assert matched, '\n' + dump_str.getvalue()
        elif matched:
            return resp, matched
        else:
            def is_err_msg(msg):
                return any(msg.get(x) for x in ['error','errors'])
            def as_list(x):
                return x if isinstance(x, list) else [x]
            # If it is a batch GraphQL query, compare each individual response separately
            for (exp, out) in zip(as_list(exp_response), as_list(resp)):
                matched_ = equal_CommentedMap(exp, out)
                if is_err_msg(exp):
                    if not matched_:
                        warnings.warn("Response does not have the expected error message\n" + dump_str.getvalue())
                        return resp, matched
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

def check_query_f(hge_ctx, f, transport='http', add_auth=True):
    print("Test file: " + f)
    hge_ctx.may_skip_test_teardown = False
    print ("transport="+transport)
    with open(f, 'r+') as c:
        # For `--accept`:
        should_write_back = False

        # ruamel will preserve order so that we can test the JSON ordering
        # property conforms to YAML spec.  It also lets us write back the yaml
        # nicely when we `--accept.`
        yml = yaml.YAML()
        conf = yml.load(c)
        if isinstance(conf, list):
            for ix, sconf in enumerate(conf):
                actual_resp, matched = check_query(hge_ctx, sconf, transport, add_auth)
                if PytestConf.config.getoption("--accept") and not matched:
                    conf[ix]['response'] = actual_resp
                    should_write_back = True
        else:
            if conf['status'] != 200:
                hge_ctx.may_skip_test_teardown = True
            actual_resp, matched = check_query(hge_ctx, conf, transport, add_auth)
            # If using `--accept` write the file back out with the new expected
            # response set to the actual response we got:
            if PytestConf.config.getoption("--accept") and not matched:
                conf['response'] = actual_resp
                should_write_back = True

        # TODO only write back when this test is not xfail. I'm stumped on how
        # best to do this. Where the 'request' fixture comes into scope we can
        # do : `request.node.get_closest_marker("xfail")` but don't want to
        # require that everywhere...
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
            # Also if it's an array for some reason (like in 'returning') TODO make this better
            if field.selection_set is None or not isinstance(result_node[fname], (dict, list)):
              result_node[fname] = collapse(result_node[fname])
            elif isinstance(result_node[fname], list):
                for node in result_node[fname]:
                    go(node, field.selection_set)
            else:
              go(result_node[fname], field.selection_set)

      if 'data' in result:
          go(result['data'], selset0)
      # errors is unordered I guess
      if 'errors' in result:
        result['errors'] = collapse(result['errors'])
      # and finally remove ordering at just the topmost level:
      return dict(result)
    else:
      # this isn't a graphql query, collapse ordering, I guess:
      return collapse(result_inp)

  # Bail out here for any number of reasons. TODO improve me
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
