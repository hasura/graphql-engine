#!/usr/bin/env python3

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

def get_event_of_query(hge_ctx, q):
    st_code, resp = hge_ctx.v1q(q)
    assert st_code == 200, resp
    return hge_ctx.get_event(3)

def validate_event_payload(ev_payload, trig_name, table):
    check_ev_payload_shape(ev_payload)
    assert ev_payload['table'] == table, ev_payload
    assert ev_payload['trigger']['name'] == trig_name, ev_payload

    return ev_payload['event']

def check_insert(hge_ctx, trig_name, table, row, exp_ev_data):
    query = {
        "type": "insert",
        "args": {
            "table": table,
            "objects": [row]
        }
    }

    ev_payload = get_event_of_query(hge_ctx,query)
    ev = validate_event_payload(ev_payload, trig_name, table)

    # insert specific assertions
    assert ev['op'] == "INSERT", ev_payload
    assert ev['data'] == exp_ev_data, ev_payload

def check_update(hge_ctx, trig_name, table, old_row, where, set_exp, exp_ev_data):
    query = {
        "type": "update",
        "args": {
            "table": table,
            "where": where,
            "$set": set_exp
        }
    }

    ev_payload = get_event_of_query(hge_ctx,query)
    ev = validate_event_payload(ev_payload, trig_name, table)

    # update specific assertions
    assert ev['op'] == "UPDATE", ev_payload
    assert ev['data'] == exp_ev_data, ev_payload

def check_delete(hge_ctx, trig_name, table, where_exp, exp_ev_data):
    query = {
        "type": "delete",
        "args": {
            "table": table,
            "where": where_exp
        }
    }

    ev_payload = get_event_of_query(hge_ctx,query)
    ev = validate_event_payload(ev_payload, trig_name, table)

    assert ev['op'] == "DELETE", ev_payload
    assert ev['data'] == exp_ev_data, ev_payload
