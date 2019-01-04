#!/usr/bin/env python3

import pytest
import queue
import yaml
import time
from super_classes import DefaultTestQueries
from validate import check_query_f, check_query, check_event

def select_last_event_fromdb(hge_ctx):
    q = {
        "type": "select",
        "args": {
            "table": {"schema": "hdb_catalog", "name": "event_log"},
            "columns": ["*"],
            "order_by": ["-created_at"],
            "limit": 1
        }
    }
    st_code, resp = hge_ctx.v1q(q)
    return st_code, resp


def insert(hge_ctx, table, row, returning=[]):
    q = {
        "type": "insert",
        "args": {
            "table": table,
            "objects": [row],
            "returning": returning
        }
    }
    st_code, resp = hge_ctx.v1q(q)
    return st_code, resp


def update(hge_ctx, table, where_exp, set_exp):
    q = {
        "type": "update",
        "args": {
            "table": table,
            "where": where_exp,
            "$set": set_exp
        }
    }
    st_code, resp = hge_ctx.v1q(q)
    return st_code, resp


def delete(hge_ctx, table, where_exp):
    q = {
        "type": "delete",
        "args": {
            "table": table,
            "where": where_exp
        }
    }
    st_code, resp = hge_ctx.v1q(q)
    return st_code, resp

class TestCreateAndDelete(DefaultTestQueries):

    def test_create_delete(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/create_and_delete.yaml")

    def test_create_reset(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/create_and_reset.yaml")

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/create-delete'

class TestCreateEvtQuery(object):

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        print("In setup method")
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/basic/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/basic/teardown.yaml')
        assert st_code == 200, resp

    def test_basic(self, hge_ctx):
        table = {"schema": "hge_tests", "name": "test_t1"}

        init_row = {"c1": 1, "c2": "hello"}
        exp_ev_data = {
            "old": None,
            "new": init_row
        }
        headers = {}
        st_code, resp = insert(hge_ctx, table, init_row)
        assert st_code == 200, resp
        check_event(hge_ctx, "t1_all", table, "INSERT", exp_ev_data, headers, "/")

        where_exp = {"c1": 1}
        set_exp = {"c2": "world"}
        exp_ev_data = {
            "old": init_row,
            "new": {"c1": 1, "c2": "world"}
        }
        st_code, resp = update(hge_ctx, table, where_exp, set_exp)
        assert st_code == 200, resp
        check_event(hge_ctx, "t1_all", table, "UPDATE", exp_ev_data, headers, "/")

        exp_ev_data = {
            "old": {"c1": 1, "c2": "world"},
            "new": None
        }
        st_code, resp = delete(hge_ctx, table, where_exp)
        assert st_code == 200, resp
        check_event(hge_ctx, "t1_all", table, "DELETE", exp_ev_data, headers, "/")


class TestRetryConf(object):

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        print("In setup method")
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/retry_conf/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/retry_conf/teardown.yaml')
        assert st_code == 200, resp

    def test_basic(self, hge_ctx):
        table = {"schema": "hge_tests", "name": "test_t1"}

        init_row = {"c1": 1, "c2": "hello"}
        exp_ev_data = {
            "old": None,
            "new": init_row
        }
        headers = {}
        st_code, resp = insert(hge_ctx, table, init_row)
        assert st_code == 200, resp
        time.sleep(15)
        tries = hge_ctx.get_error_queue_size()
        assert tries == 5, tries


class TestEvtHeaders(object):

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        print("In setup method")
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/headers/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/headers/teardown.yaml')
        assert st_code == 200, resp

    def test_basic(self, hge_ctx):
        table = {"schema": "hge_tests", "name": "test_t1"}

        init_row = {"c1": 1, "c2": "hello"}
        exp_ev_data = {
            "old": None,
            "new": init_row
        }
        headers = {"X-Header-From-Value": "MyValue", "X-Header-From-Env": "MyEnvValue"}
        st_code, resp = insert(hge_ctx, table, init_row)
        assert st_code == 200, resp
        check_event(hge_ctx, "t1_all", table, "INSERT", exp_ev_data, headers, "/")


class TestUpdateEvtQuery(object):

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        print("In setup method")
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/update_query/create-setup.yaml')
        assert st_code == 200, resp
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/update_query/update-setup.yaml')
        assert st_code == 200, '{}'.format(resp)
        yield
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/update_query/teardown.yaml')
        assert st_code == 200, resp

    def test_update_basic(self, hge_ctx):
        table = {"schema": "hge_tests", "name": "test_t1"}

        init_row = {"c1": 1, "c2": "hello"}
        exp_ev_data = {
            "old": None,
            "new": {"c1": 1, "c2": "hello"}
        }
        headers = {}
        st_code, resp = insert(hge_ctx, table, init_row)
        assert st_code == 200, resp
        with pytest.raises(queue.Empty):
            check_event(hge_ctx, "t1_cols", table, "INSERT", exp_ev_data, headers, "/new")

        where_exp = {"c1": 1}
        set_exp = {"c2": "world"}
        # expected no event hence previous expected data
        st_code, resp = update(hge_ctx, table, where_exp, set_exp)
        assert st_code == 200, resp
        with pytest.raises(queue.Empty):
            check_event(hge_ctx, "t1_cols", table, "UPDATE", exp_ev_data, headers, "/new")

        where_exp = {"c1": 1}
        set_exp = {"c1": 2}
        exp_ev_data = {
            "old": {"c1": 1, "c2": "world"},
            "new": {"c1": 2, "c2": "world"}
        }
        st_code, resp = update(hge_ctx, table, where_exp, set_exp)
        assert st_code == 200, resp
        check_event(hge_ctx, "t1_cols", table, "UPDATE", exp_ev_data, headers, "/new")

        where_exp = {"c1": 2}
        exp_ev_data = {
            "old": {"c1": 2, "c2": "world"},
            "new": None
        }
        st_code, resp = delete(hge_ctx, table, where_exp)
        assert st_code == 200, resp
        check_event(hge_ctx, "t1_cols", table, "DELETE", exp_ev_data, headers, "/new")


class TestDeleteEvtQuery(object):

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        print("In setup method")
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/basic/setup.yaml')
        assert st_code == 200, resp
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/delete_query/setup.yaml')
        assert st_code == 200, '{}'.format(resp)
        yield
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/delete_query/teardown.yaml')
        assert st_code == 200, resp

    def test_delete_basic(self, hge_ctx):
        table = {"schema": "hge_tests", "name": "test_t1"}

        init_row = {"c1": 1, "c2": "hello"}
        exp_ev_data = {
            "old": None,
            "new": init_row
        }
        headers = {}
        st_code, resp = insert(hge_ctx, table, init_row)
        assert st_code == 200, resp
        with pytest.raises(queue.Empty):
            check_event(hge_ctx, "t1_all", table, "INSERT", exp_ev_data, headers, "/")

        where_exp = {"c1": 1}
        set_exp = {"c2": "world"}
        exp_ev_data = {
            "old": init_row,
            "new": {"c1": 1, "c2": "world"}
        }
        st_code, resp = update(hge_ctx, table, where_exp, set_exp)
        assert st_code == 200, resp
        with pytest.raises(queue.Empty):
            check_event(hge_ctx, "t1_all", table, "UPDATE", exp_ev_data, headers, "/")

        exp_ev_data = {
            "old": {"c1": 1, "c2": "world"},
            "new": None
        }
        st_code, resp = delete(hge_ctx, table, where_exp)
        assert st_code == 200, resp
        with pytest.raises(queue.Empty):
            check_event(hge_ctx, "t1_all", table, "DELETE", exp_ev_data, headers, "/")


class TestEvtSelCols:

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        print("In setup method")
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/selected_cols/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/selected_cols/teardown.yaml')
        assert st_code == 200, resp

    def test_selected_cols(self, hge_ctx):
        table = {"schema": "hge_tests", "name": "test_t1"}

        init_row = {"c1": 1, "c2": "hello"}
        exp_ev_data = {
            "old": None,
            "new": {"c1": 1, "c2": "hello"}
        }
        headers = {}
        st_code, resp = insert(hge_ctx, table, init_row)
        assert st_code == 200, resp
        check_event(hge_ctx, "t1_cols", table, "INSERT", exp_ev_data, headers, "/")

        where_exp = {"c1": 1}
        set_exp = {"c2": "world"}
        # expected no event hence previous expected data
        st_code, resp = update(hge_ctx, table, where_exp, set_exp)
        assert st_code == 200, resp
        with pytest.raises(queue.Empty):
            check_event(hge_ctx, "t1_cols", table, "UPDATE", exp_ev_data, headers, "/")

        where_exp = {"c1": 1}
        set_exp = {"c1": 2}
        exp_ev_data = {
            "old": {"c1": 1, "c2": "world"},
            "new": {"c1": 2, "c2": "world"}
        }
        st_code, resp = update(hge_ctx, table, where_exp, set_exp)
        assert st_code == 200, resp
        check_event(hge_ctx, "t1_cols", table, "UPDATE", exp_ev_data, headers, "/")

        where_exp = {"c1": 2}
        exp_ev_data = {
            "old": {"c1": 2, "c2": "world"},
            "new": None
        }
        st_code, resp = delete(hge_ctx, table, where_exp)
        assert st_code == 200, resp
        check_event(hge_ctx, "t1_cols", table, "DELETE", exp_ev_data, headers, "/")

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
        assert st_code == 200, resp


class TestEvtInsertOnly:

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        print("In setup method")
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/insert_only/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/insert_only/teardown.yaml')
        assert st_code == 200, resp

    def test_insert_only(self, hge_ctx):
        table = {"schema": "hge_tests", "name": "test_t1"}

        init_row = {"c1": 1, "c2": "hello"}
        exp_ev_data = {
            "old": None,
            "new": init_row
        }
        headers = {}
        st_code, resp = insert(hge_ctx, table, init_row)
        assert st_code == 200, resp
        check_event(hge_ctx, "t1_insert", table, "INSERT", exp_ev_data, headers, "/")

        where_exp = {"c1": 1}
        set_exp = {"c2": "world"}
        exp_ev_data = {
            "old": init_row,
            "new": {"c1": 1, "c2": "world"}
        }
        st_code, resp = update(hge_ctx, table, where_exp, set_exp)
        assert st_code == 200, resp
        with pytest.raises(queue.Empty):
            check_event(hge_ctx, "t1_insert", table, "UPDATE", exp_ev_data, headers, "/")

        exp_ev_data = {
            "old": {"c1": 1, "c2": "world"},
            "new": None
        }
        st_code, resp = delete(hge_ctx, table, where_exp)
        assert st_code == 200, resp
        with pytest.raises(queue.Empty):
            check_event(hge_ctx, "t1_insert", table, "DELETE", exp_ev_data, headers, "/")


class TestEvtSelPayload:

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        print("In setup method")
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/selected_payload/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/selected_payload/teardown.yaml')
        assert st_code == 200, resp

    def test_selected_payload(self, hge_ctx):
        table = {"schema": "hge_tests", "name": "test_t1"}

        init_row = {"c1": 1, "c2": "hello"}
        exp_ev_data = {
            "old": None,
            "new": {"c1": 1, "c2": "hello"}
        }
        headers = {}
        st_code, resp = insert(hge_ctx, table, init_row)
        assert st_code == 200, resp
        check_event(hge_ctx, "t1_payload", table, "INSERT", exp_ev_data, headers, "/")

        where_exp = {"c1": 1}
        set_exp = {"c2": "world"}
        exp_ev_data = {
            "old": {"c1": 1},
            "new": {"c1": 1}
        }
        st_code, resp = update(hge_ctx, table, where_exp, set_exp)
        assert st_code == 200, resp
        check_event(hge_ctx, "t1_payload", table, "UPDATE", exp_ev_data, headers, "/")

        where_exp = {"c1": 1}
        set_exp = {"c1": 2}
        exp_ev_data = {
            "old": {"c1": 1},
            "new": {"c1": 2}
        }
        st_code, resp = update(hge_ctx, table, where_exp, set_exp)
        assert st_code == 200, resp
        check_event(hge_ctx, "t1_payload", table, "UPDATE", exp_ev_data, headers, "/")

        where_exp = {"c1": 2}
        exp_ev_data = {
            "old": {"c2": "world"},
            "new": None
        }
        st_code, resp = delete(hge_ctx, table, where_exp)
        assert st_code == 200, resp
        check_event(hge_ctx, "t1_payload", table, "DELETE", exp_ev_data, headers, "/")

    def test_selected_payload_dep(self, hge_ctx):
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

class TestWebhookEnv(object):

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        print("In setup method")
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/webhook_env/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/webhook_env/teardown.yaml')
        assert st_code == 200, resp

    def test_basic(self, hge_ctx):
        table = {"schema": "hge_tests", "name": "test_t1"}

        init_row = {"c1": 1, "c2": "hello"}
        exp_ev_data = {
            "old": None,
            "new": init_row
        }
        headers = {}
        st_code, resp = insert(hge_ctx, table, init_row)
        assert st_code == 200, resp
        check_event(hge_ctx, "t1_all", table, "INSERT", exp_ev_data, headers, "/")

        where_exp = {"c1": 1}
        set_exp = {"c2": "world"}
        exp_ev_data = {
            "old": init_row,
            "new": {"c1": 1, "c2": "world"}
        }
        st_code, resp = update(hge_ctx, table, where_exp, set_exp)
        assert st_code == 200, resp
        check_event(hge_ctx, "t1_all", table, "UPDATE", exp_ev_data, headers, "/")

        exp_ev_data = {
            "old": {"c1": 1, "c2": "world"},
            "new": None
        }
        st_code, resp = delete(hge_ctx, table, where_exp)
        assert st_code == 200, resp
        check_event(hge_ctx, "t1_all", table, "DELETE", exp_ev_data, headers, "/")
