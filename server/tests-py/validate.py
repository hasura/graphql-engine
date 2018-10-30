#!/usr/bin/env python3

import yaml
import json
import os
import base64
import jsondiff
import jwt


def check_keys(keys, obj):
    for k in keys:
        assert k in obj, obj


def check_ev_payload_shape(ev_payload):
    top_level_keys = ["created_at", "event", "id", "table", "trigger"]
    check_keys(top_level_keys, ev_payload)

    event_keys = ["data", "op"]
    check_keys(event_keys, ev_payload['event'])

    trigger_keys = ["id", "name"]
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


def check_event(hge_ctx, trig_name, table, operation, exp_ev_data, headers, webhook_path):
    ev_full = hge_ctx.get_event(3)
    validate_event_webhook(ev_full['path'], webhook_path)
    validate_event_headers(ev_full['headers'], headers)
    validate_event_payload(ev_full['body'], trig_name, table)
    ev = ev_full['body']['event']
    assert ev['op'] == operation, ev
    assert ev['data'] == exp_ev_data, ev


def test_forbidden_when_access_key_reqd(hge_ctx, conf):
    headers = {}
    if 'headers' in conf:
        headers = conf['headers']

    # Test without access key
    code, resp = hge_ctx.anyq(conf['url'], conf['query'], headers)
    assert code == 401, "\n" + yaml.dump({
        "expected": "Should be access denied as access key is not provided",
        "actual": {
            "code": code,
            "response": resp
        }
    })

    # Test with random access key
    headers['X-Hasura-Access-Key'] = base64.b64encode(os.urandom(30))
    code, resp = hge_ctx.anyq(conf['url'], conf['query'], headers)
    assert code == 401, "\n" + yaml.dump({
        "expected": "Should be access denied as an incorrect access key is provided",
        "actual": {
            "code": code,
            "response": resp
        }
    })


def test_forbidden_webhook(hge_ctx, conf):
    h = {'Authorization': 'Bearer ' + base64.b64encode(base64.b64encode(os.urandom(30))).decode('utf-8')}
    code, resp = hge_ctx.anyq(conf['url'], conf['query'], h)
    assert code == 401, "\n" + yaml.dump({
        "expected": "Should be access denied as it is denied from webhook",
        "actual": {
            "code": code,
            "response": resp
        }
    })


def check_query(hge_ctx, conf, add_auth=True):
    headers = {}
    if 'headers' in conf:
        headers = conf['headers']

    if add_auth:
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

        if hge_ctx.hge_webhook is not None and len(headers) > 0:
            if not hge_ctx.webhook_insecure:
                test_forbidden_webhook(hge_ctx, conf)
            headers['X-Hasura-Auth-Mode'] = 'webhook'
            headers_new = dict()
            headers_new['Authorization'] = 'Bearer ' + base64.b64encode(json.dumps(headers).encode('utf-8')).decode(
                'utf-8')
            headers = headers_new

        elif (
                hge_ctx.hge_webhook is not None or hge_ctx.hge_jwt_key is not None) and hge_ctx.hge_key is not None and len(
                headers) == 0:
            headers['X-Hasura-Access-Key'] = hge_ctx.hge_key

        elif hge_ctx.hge_key is not None and hge_ctx.hge_webhook is None and hge_ctx.hge_jwt_key is None:
            test_forbidden_when_access_key_reqd(hge_ctx, conf)
            headers['X-Hasura-Access-Key'] = hge_ctx.hge_key

    code, resp = hge_ctx.anyq(conf['url'], conf['query'], headers)
    print(headers)
    assert code == conf['status'], resp
    if 'response' in conf:
        assert json_ordered(resp) == json_ordered(conf['response']), yaml.dump({
            'response': resp,
            'expected': conf['response'],
            'diff': jsondiff.diff(conf['response'], resp)
        })
    return code, resp


def check_query_f(hge_ctx, f, add_auth=True):
    hge_ctx.may_skip_test_teardown = False
    with open(f) as c:
        conf = yaml.safe_load(c)
        if isinstance(conf, list):
            for sconf in conf:
                check_query(hge_ctx, sconf)
        else:
            if conf['status'] != 200:
                hge_ctx.may_skip_test_teardown = True
            check_query(hge_ctx, conf, add_auth)


def json_ordered(obj):
    if isinstance(obj, dict):
        return sorted((k, json_ordered(v)) for k, v in obj.items())
    if isinstance(obj, list):
        return list(json_ordered(x) for x in obj)
    else:
        return obj
