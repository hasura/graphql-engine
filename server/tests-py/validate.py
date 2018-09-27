#!/usr/bin/env python3

import yaml

def check_keys(keys, obj):
    for k in keys:
        assert k in obj, obj

def check_ev_payload_shape(ev_payload):

    top_level_keys = [ "created_at", "event", "id", "table", "trigger"]
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

def check_query(hge_ctx, conf):
    headers={}
    if 'headers' in conf:
        headers = conf['headers']
    code, resp = hge_ctx.anyq( conf['url'], conf['query'], headers)
    assert code == conf['status'], resp
    if 'response' in conf:
        print ('response\n', yaml.dump(resp))
        print ('expected\n', yaml.dump(conf['response']))
        assert json_ordered(resp) == json_ordered(conf['response'])

def check_query_f(hge_ctx, f):
    with open(f) as c:
        conf = yaml.load(c)
        if isinstance(conf, list):
            for sconf in conf:
                check_query( hge_ctx, sconf)
        else:
           check_query( hge_ctx, conf )

def json_ordered(obj):
    if isinstance(obj, dict):
        return sorted((k, json_ordered(v)) for k, v in obj.items())
    if isinstance(obj, list):
        return list(json_ordered(x) for x in obj)
    else:
        return obj
