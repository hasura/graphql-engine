#!/usr/bin/env python3

import pytest
import queue
import yaml
from validate import check_delete, check_update, check_insert

def to_dict(rows):
    return list(map(lambda r: dict(r), rows))

def compare(s, t, isSubset = False):
    t = list(t)   # make a mutable copy
    try:
        for elem in s:
            t.remove(elem)
    except ValueError:
        return False
    return isSubset or not t

class TestCreateEvtQuery(object):

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        print ("In setup method")
        st_code, resp = hge_ctx.v1q_f('queries/advanced/create-setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f('queries/advanced/create-teardown.yaml')
        assert st_code == 200, resp

    def test_insert_trigger(self, hge_ctx):
        conn = hge_ctx.engine.connect()

        # test t1_all
        res = conn.execute("select name, type, schema_name, table_name, definition, query, webhook, num_retries, retry_interval \
        from hdb_catalog.event_triggers \
        where name = 't1_all'")
        resp = to_dict(res)
        exp_resp = [{
              "name": "t1_all"
            , "type": "table"
            , "schema_name": "hge_tests"
            , "table_name": "test_t1"
            , "definition": {"insert": {"columns": "*"}, "update": {"columns": "*"}, "delete": {"columns": "*"} }
            , "query": None
            , "webhook": "http://127.0.0.1:5592"
            , "num_retries": 0
            , "retry_interval": 10
        }]
        assert resp == exp_resp, resp
        res = conn.execute("select trigger_name \
        from information_schema.triggers \
        where event_object_schema='hge_tests' and event_object_table='test_t1'")
        resp = to_dict(res)
        exp_resp = [
            {
            "trigger_name" : "notify_hasura_t1_all_insert"
            },
            {
            "trigger_name" : "notify_hasura_t1_all_update"
            },
             {
            "trigger_name" : "notify_hasura_t1_all_delete"
            }
        ]

        assert compare(exp_resp, resp, True), resp

        # test t1_cols
        res = conn.execute("select name, type, schema_name, table_name, definition, query, webhook, num_retries, retry_interval \
        from hdb_catalog.event_triggers \
        where name = 't1_cols'")
        resp = to_dict(res)
        exp_resp = [{
              "name": "t1_cols"
            , "type": "table"
            , "schema_name": "hge_tests"
            , "table_name": "test_t1"
            , "definition": {"insert": {"columns": ["c2"]}, "update": {"columns": ["c1"]} }
            , "query": None
            , "webhook": "http://127.0.0.1:5592"
            , "num_retries": 5
            , "retry_interval": 5
        }]
        assert resp == exp_resp, resp
        res = conn.execute("select trigger_name \
        from information_schema.triggers \
        where event_object_schema='hge_tests' and event_object_table='test_t1'")
        resp = to_dict(res)
        exp_resp = [
            {
            "trigger_name" : "notify_hasura_t1_cols_insert"
            },
            {
            "trigger_name" : "notify_hasura_t1_cols_update"
            }
        ]

        not_exp_resp = [{
            "trigger_name" : "notify_hasura_t1_cols_delete"
        }]

        assert compare(exp_resp, resp, True), resp
        assert not compare(exp_resp, not_exp_resp, True), resp

class TestUpdateEvtQuery(object):

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        print ("In setup method")
        st_code, resp = hge_ctx.v1q_f('queries/advanced/create-setup.yaml')
        assert st_code == 200, resp
        st_code, resp = hge_ctx.v1q_f('queries/advanced/update-setup.yaml')
        assert st_code == 200, '{}'.format(resp)
        yield
        st_code, resp = hge_ctx.v1q_f('queries/advanced/create-teardown.yaml')
        assert st_code == 200, resp

    def test_update_trigger(self, hge_ctx):
        conn = hge_ctx.engine.connect()

        # test t1_all
        res = conn.execute("select name, type, schema_name, table_name, definition, query, webhook, num_retries, retry_interval \
        from hdb_catalog.event_triggers \
        where name = 't1_all'")
        resp = to_dict(res)
        exp_resp = [{
              "name": "t1_all"
            , "type": "table"
            , "schema_name": "hge_tests"
            , "table_name": "test_t1"
            , "definition": {"insert": {"columns": "*"}, "update": {"columns": "*"}, "delete": {"columns": "*"} }
            , "query": None
            , "webhook": "mywebhook.com"
            , "num_retries": 0
            , "retry_interval": 10
        }]
        assert resp == exp_resp, resp
        res = conn.execute("select trigger_name \
        from information_schema.triggers \
        where event_object_schema='hge_tests' and event_object_table='test_t1'")
        resp = to_dict(res)
        exp_resp = [
            {
            "trigger_name" : "notify_hasura_t1_all_insert"
            },
            {
            "trigger_name" : "notify_hasura_t1_all_update"
            },
             {
            "trigger_name" : "notify_hasura_t1_all_delete"
            }
        ]

        assert compare(exp_resp, resp, True), resp

        # test t1_cols
        res = conn.execute("select name, type, schema_name, table_name, definition, query, webhook, num_retries, retry_interval \
        from hdb_catalog.event_triggers \
        where name = 't1_cols'")
        resp = to_dict(res)
        exp_resp = [{
              "name": "t1_cols"
            , "type": "table"
            , "schema_name": "hge_tests"
            , "table_name": "test_t1"
            , "definition": {"insert": {"columns": ["c2"]}, "update": {"columns": ["c1"]}, "delete": {"columns": "*"} }
            , "query": None
            , "webhook": "http://127.0.0.1:5592"
            , "num_retries": 5
            , "retry_interval": 5
        }]
        assert resp == exp_resp, resp
        res = conn.execute("select trigger_name \
        from information_schema.triggers \
        where event_object_schema='hge_tests' and event_object_table='test_t1'")
        resp = to_dict(res)
        exp_resp = [
            {
            "trigger_name" : "notify_hasura_t1_cols_insert"
            },
            {
            "trigger_name" : "notify_hasura_t1_cols_update"
            },
            {
            "trigger_name" : "notify_hasura_t1_cols_delete"
            }
        ]

        assert compare(exp_resp, resp, True), resp

class TestDeleteEvtQuery(object):

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        print ("In setup method")
        st_code, resp = hge_ctx.v1q_f('queries/advanced/create-setup.yaml')
        assert st_code == 200, resp
        st_code, resp = hge_ctx.v1q_f('queries/advanced/delete-setup.yaml')
        assert st_code == 200, '{}'.format(resp)
        yield
        st_code, resp = hge_ctx.v1q_f('queries/advanced/delete-teardown.yaml')
        assert st_code == 200, resp

    def test_update_trigger(self, hge_ctx):
        conn = hge_ctx.engine.connect()

        # test t1_all
        res = conn.execute("select name, type, schema_name, table_name, definition, query, webhook, num_retries, retry_interval \
        from hdb_catalog.event_triggers \
        where name = 't1_all'")
        resp = to_dict(res)
        exp_resp = []
        assert resp == exp_resp, resp
        res = conn.execute("select trigger_name \
        from information_schema.triggers \
        where event_object_schema='hge_tests' and event_object_table='test_t1'")
        resp = to_dict(res)
        exp_resp = []

        assert resp == exp_resp, resp

class TestEvtBasic(object):

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        print ("In setup method")
        st_code, resp = hge_ctx.v1q_f('queries/basic/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f('queries/basic/teardown.yaml')
        assert st_code == 200, resp


    def test_basic(self,hge_ctx):

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


    def test_basic_dep(self,hge_ctx):

        st_code, resp = hge_ctx.v1q({
            "type": "run_sql",
            "args": {
                "sql": "alter table hge_tests.test_t1 drop column c1"
            }
        })
        assert st_code == 400, resp
        assert resp['code'] == "dependency-error", resp



@pytest.mark.usefixtures('hge_ctx')
class TestEvtSelCols:

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        print ("In setup method")
        st_code, resp = hge_ctx.v1q_f('queries/selected_cols/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f('queries/selected_cols/teardown.yaml')
        assert st_code == 200, resp

    def test_selected_cols(self, hge_ctx):

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


    def test_selected_cols_dep(self, hge_ctx):

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

class TestEvtEmptyCols:

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        print ("In setup method")
        st_code, resp = hge_ctx.v1q_f('queries/empty_cols/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f('queries/empty_cols/teardown.yaml')
        assert st_code == 200, resp


    def test_empty_cols(self, hge_ctx):

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


class TestEvtInsertOnly:

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        print ("In setup method")
        st_code, resp = hge_ctx.v1q_f('queries/insert_only/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f('queries/insert_only/teardown.yaml')
        assert st_code == 200, resp


    def test_insert_only(self, hge_ctx):

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

