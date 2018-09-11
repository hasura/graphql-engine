#!/usr/bin/env python3

import pytest
import queue
import yaml
from validate import check_delete, check_update, check_insert

def test_basic(hge_ctx):
    st_code, resp = hge_ctx.v1q_f('queries/basic/setup.yaml')
    assert st_code == 200, resp

    table = {"schema" : "hge_tests", "name": "test_t1"}

    init_row = {"c1" : 1, "c2" : "hello"}
    exp_ev_data = {
        "old": None,
        "new": init_row
    }
    check_insert(hge_ctx, "t1_all", table, init_row, exp_ev_data)

    where_exp = {"c1": 1}
    set_exp = {"c2" : "world"}
    exp_ev_data = {
        "old": init_row,
        "new": {"c1" : 1, "c2" : "world"}
    }
    check_update(hge_ctx, "t1_all", table, init_row, where_exp, set_exp, exp_ev_data)

    exp_ev_data = {
        "old": {"c1" : 1, "c2" : "world"},
        "new": None
    }
    check_delete(hge_ctx, "t1_all", table, where_exp, exp_ev_data)

    st_code, resp = hge_ctx.v1q_f('queries/basic/teardown.yaml')
    assert st_code == 200, resp

def test_basic_dep(hge_ctx):
    st_code, resp = hge_ctx.v1q_f('queries/basic/setup.yaml')
    assert st_code == 200, resp

    st_code, resp = hge_ctx.v1q({
        "type": "run_sql",
        "args": {
            "sql": "alter table hge_tests.test_t1 drop column c1"
        }
    })
    assert st_code == 400, resp
    assert resp['code'] == "dependency-error", resp

    st_code, resp = hge_ctx.v1q_f('queries/basic/teardown.yaml')
    assert st_code == 200, resp

def test_selected_cols(hge_ctx):
    st_code, resp = hge_ctx.v1q_f('queries/selected_cols/setup.yaml')
    assert st_code == 200, resp

    table = {"schema" : "hge_tests", "name": "test_t1"}

    init_row = {"c1" : 1, "c2" : "hello"}
    exp_ev_data = {
        "old": None,
        "new": {"c2": "hello"}
    }
    check_insert(hge_ctx, "t1_all", table, init_row, exp_ev_data)

    where_exp = {"c1": 1}
    set_exp = {"c2" : "world"}
    exp_ev_data = {
        "old": {"c1" : 1},
        "new": {"c1" : 1}
    }
    check_update(hge_ctx, "t1_all", table, init_row, where_exp, set_exp, exp_ev_data)

    exp_ev_data = {
        "old": {"c1" : 1, "c2" : "world"},
        "new": None
    }
    check_delete(hge_ctx, "t1_all", table, where_exp, exp_ev_data)

    st_code, resp = hge_ctx.v1q_f('queries/selected_cols/teardown.yaml')
    assert st_code == 200, resp

def test_selected_cols_dep(hge_ctx):
    st_code, resp = hge_ctx.v1q_f('queries/selected_cols/setup.yaml')
    assert st_code == 200, resp

    st_code, resp = hge_ctx.v1q({
        "type": "run_sql",
        "args": {
            "sql": "alter table hge_tests.test_t1 drop column c1"
        }
    })
    assert st_code == 400, resp
    assert resp['code'] == "dependency-error", resp

    st_code, resp = hge_ctx.v1q({
        "type": "run_sql",
        "args": {
            "sql": "alter table hge_tests.test_t1 drop column c2"
        }
    })
    assert st_code == 400, resp
    assert resp['code'] == "dependency-error", resp

    st_code, resp = hge_ctx.v1q({
        "type": "run_sql",
        "args": {
            "sql": "alter table hge_tests.test_t1 drop column c3"
        }
    })
    assert st_code == 200, resp

    st_code, resp = hge_ctx.v1q_f('queries/selected_cols/teardown.yaml')
    assert st_code == 200, resp

def test_empty_cols(hge_ctx):
    st_code, resp = hge_ctx.v1q_f('queries/empty_cols/setup.yaml')
    assert st_code == 200, resp

    table = {"schema" : "hge_tests", "name": "test_t1"}

    init_row = {"c1" : 1, "c2" : "hello"}
    exp_ev_data = {
        "old": None,
        "new": {}
    }
    check_insert(hge_ctx, "t1_all", table, init_row, exp_ev_data)

    where_exp = {"c1": 1}
    set_exp = {"c2" : "world"}
    exp_ev_data = {
        "old": {},
        "new": {}
    }
    check_update(hge_ctx, "t1_all", table, init_row, where_exp, set_exp, exp_ev_data)

    exp_ev_data = {
        "old": {},
        "new": None
    }
    check_delete(hge_ctx, "t1_all", table, where_exp, exp_ev_data)

    st_code, resp = hge_ctx.v1q_f('queries/empty_cols/teardown.yaml')
    assert st_code == 200, resp

def test_insert_only(hge_ctx):
    st_code, resp = hge_ctx.v1q_f('queries/insert_only/setup.yaml')
    assert st_code == 200, resp

    table = {"schema" : "hge_tests", "name": "test_t1"}

    init_row = {"c1" : 1, "c2" : "hello"}
    exp_ev_data = {
        "old": None,
        "new": init_row
    }
    check_insert(hge_ctx, "t1_all", table, init_row, exp_ev_data)

    where_exp = {"c1": 1}
    set_exp = {"c2" : "world"}
    exp_ev_data = {
        "old": init_row,
        "new": {"c1" : 1, "c2" : "world"}
    }
    with pytest.raises(queue.Empty):
        check_update(hge_ctx, "t1_all", table, init_row, where_exp, set_exp, exp_ev_data)

    exp_ev_data = {
        "old": {"c1" : 1, "c2" : "world"},
        "new": None
    }
    with pytest.raises(queue.Empty):
        check_delete(hge_ctx, "t1_all", table, where_exp, exp_ev_data)

    st_code, resp = hge_ctx.v1q_f('queries/insert_only/teardown.yaml')
    assert st_code == 200, resp
