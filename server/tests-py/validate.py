#!/usr/bin/env python3

import yaml
import json
import os
import base64
import jsondiff
import jwt
import random
import time

from context import GQLWsClient

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
    code, resp = hge_ctx.anyq(conf['url'], conf['query'], headers)
    #assert code in [401,404], "\n" + yaml.dump({
    assert code in status, "\n" + yaml.dump({
        "expected": "Should be access denied as admin secret is not provided",
        "actual": {
            "code": code,
            "response": resp
        }
    })

    # Test with random admin secret
    headers['X-Hasura-Admin-Secret'] = base64.b64encode(os.urandom(30))
    code, resp = hge_ctx.anyq(conf['url'], conf['query'], headers)
    #assert code in [401,404], "\n" + yaml.dump({
    assert code in status, "\n" + yaml.dump({
        "expected": "Should be access denied as an incorrect admin secret is provided",
        "actual": {
            "code": code,
            "response": resp
        }
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
    code, resp = hge_ctx.anyq(conf['url'], conf['query'], h)
    #assert code in [401,404], "\n" + yaml.dump({
    assert code in status, "\n" + yaml.dump({
        "expected": "Should be access denied as it is denied from webhook",
        "actual": {
            "code": code,
            "response": resp
        }
    })


def check_query(hge_ctx, conf, transport='http', add_auth=True):
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

    exp_ws_response = exp_http_response

    assert 'payload' in resp, resp
    assert resp['payload'] == exp_ws_response, yaml.dump({
        'response': resp['payload'],
        'expected': exp_ws_response,
        'diff': jsondiff.diff(json.dumps(exp_ws_response), json.dumps(resp['payload']))
    })
    resp_done = next(query_resp)
    assert resp_done['type'] == 'complete'
    return resp['payload']


def validate_http_anyq(hge_ctx, url, query, headers, exp_code, exp_response):
    code, resp = hge_ctx.anyq(url, query, headers)
    print(headers)
    assert code == exp_code, resp
    print('http resp: ', resp)
    if exp_response:
        assert json_ordered(resp) == json_ordered(exp_response), yaml.dump({
            'response': resp,
            'expected': exp_response,
            'diff': jsondiff.diff(exp_response, resp)
        })
    return resp

def check_query_f(hge_ctx, f, transport='http', add_auth=True):
    print("Test file: " + f)
    hge_ctx.may_skip_test_teardown = False
    print ("transport="+transport)
    with open(f) as c:
        conf = yaml.safe_load(c)
        if isinstance(conf, list):
            for sconf in conf:
                check_query(hge_ctx, sconf, transport, add_auth)
        else:
            if conf['status'] != 200:
                hge_ctx.may_skip_test_teardown = True
            check_query(hge_ctx, conf, transport, add_auth)


def json_ordered(obj):
    if isinstance(obj, dict):
        return sorted((k, json_ordered(v)) for k, v in obj.items())
    if isinstance(obj, list):
        return list(json_ordered(x) for x in obj)
    else:
        return obj
