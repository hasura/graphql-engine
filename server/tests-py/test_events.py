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


def insert(hge_ctx, table, row, returning=[], headers = {}):
    q = {
        "type": "insert",
        "args": {
            "table": table,
            "objects": [row],
            "returning": returning
        }
    }
    st_code, resp = hge_ctx.v1q(q, headers = headers)
    return st_code, resp


def update(hge_ctx, table, where_exp, set_exp, headers = {}):
    q = {
        "type": "update",
        "args": {
            "table": table,
            "where": where_exp,
            "$set": set_exp
        }
    }
    st_code, resp = hge_ctx.v1q(q, headers = headers)
    return st_code, resp


def delete(hge_ctx, table, where_exp, headers = {}):
    q = {
        "type": "delete",
        "args": {
            "table": table,
            "where": where_exp
        }
    }
    st_code, resp = hge_ctx.v1q(q, headers = headers)
    return st_code, resp

@pytest.mark.usefixtures("evts_webhook")
class TestCreateAndDelete(DefaultTestQueries):

    def test_create_delete(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/create_and_delete.yaml")

    def test_create_reset(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/create_and_reset.yaml")

    def test_create_operation_spec_not_provider_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/create_trigger_operation_specs_not_provided_err.yaml")

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/create-delete'

class TestCreateEvtQuery(object):

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx, evts_webhook):
        print("In setup method")
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/basic/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/basic/teardown.yaml')
        assert st_code == 200, resp

    def test_basic(self, hge_ctx, evts_webhook):
        table = {"schema": "hge_tests", "name": "test_t1"}

        init_row = {"c1": 1, "c2": "hello"}
        exp_ev_data = {
            "old": None,
            "new": init_row
        }
        st_code, resp = insert(hge_ctx, table, init_row)
        assert st_code == 200, resp
        check_event(hge_ctx, evts_webhook, "t1_all", table, "INSERT", exp_ev_data)

        where_exp = {"c1": 1}
        set_exp = {"c2": "world"}
        exp_ev_data = {
            "old": init_row,
            "new": {"c1": 1, "c2": "world"}
        }
        st_code, resp = update(hge_ctx, table, where_exp, set_exp)
        assert st_code == 200, resp
        check_event(hge_ctx, evts_webhook, "t1_all", table, "UPDATE", exp_ev_data)

        exp_ev_data = {
            "old": {"c1": 1, "c2": "world"},
            "new": None
        }
        st_code, resp = delete(hge_ctx, table, where_exp)
        assert st_code == 200, resp
        check_event(hge_ctx, evts_webhook, "t1_all", table, "DELETE", exp_ev_data)


class TestRetryConf(object):

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx, evts_webhook):
        print("In setup method")
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/retry_conf/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/retry_conf/teardown.yaml')
        assert st_code == 200, resp

    def test_basic(self, hge_ctx, evts_webhook):
        table = {"schema": "hge_tests", "name": "test_t1"}

        init_row = {"c1": 1, "c2": "hello"}
        exp_ev_data = {
            "old": None,
            "new": init_row
        }
        st_code, resp = insert(hge_ctx, table, init_row)
        assert st_code == 200, resp
        time.sleep(15)
        tries = evts_webhook.get_error_queue_size()
        assert tries == 5, tries

    def test_timeout_short(self, hge_ctx, evts_webhook):
        table = {"schema": "hge_tests", "name": "test_t2"}

        init_row = {"c1": 1, "c2": "hello"}
        exp_ev_data = {
            "old": None,
            "new": init_row
        }
        st_code, resp = insert(hge_ctx, table, init_row)
        assert st_code == 200, resp
        time.sleep(20)
        tries = evts_webhook.get_error_queue_size()
        assert tries == 3, tries

    def test_timeout_long(self, hge_ctx, evts_webhook):
        table = {"schema": "hge_tests", "name": "test_t3"}

        init_row = {"c1": 1, "c2": "hello"}
        exp_ev_data = {
            "old": None,
            "new": init_row
        }
        st_code, resp = insert(hge_ctx, table, init_row)
        assert st_code == 200, resp
        time.sleep(15)
        check_event(hge_ctx, evts_webhook, "t3_timeout_long", table, "INSERT", exp_ev_data, webhook_path = "/timeout_long")

class TestEvtHeaders(object):

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx, evts_webhook):
        print("In setup method")
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/headers/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/headers/teardown.yaml')
        assert st_code == 200, resp

    def test_basic(self, hge_ctx, evts_webhook):
        table = {"schema": "hge_tests", "name": "test_t1"}

        init_row = {"c1": 1, "c2": "hello"}
        exp_ev_data = {
            "old": None,
            "new": init_row
        }
        headers = {"X-Header-From-Value": "MyValue", "X-Header-From-Env": "MyEnvValue"}
        st_code, resp = insert(hge_ctx, table, init_row)
        assert st_code == 200, resp
        check_event(hge_ctx, evts_webhook, "t1_all", table, "INSERT", exp_ev_data, headers = headers)


class TestUpdateEvtQuery(object):

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx, evts_webhook):
        print("In setup method")
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/update_query/create-setup.yaml')
        assert st_code == 200, resp
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/update_query/update-setup.yaml')
        assert st_code == 200, '{}'.format(resp)
        assert resp[1][0]["configuration"]["webhook"] == 'http://127.0.0.1:5592/new'
        yield
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/update_query/teardown.yaml')
        assert st_code == 200, resp

    def test_update_basic(self, hge_ctx, evts_webhook):
        table = {"schema": "hge_tests", "name": "test_t1"}

        init_row = {"c1": 1, "c2": "hello"}
        exp_ev_data = {
            "old": None,
            "new": {"c1": 1, "c2": "hello"}
        }
        st_code, resp = insert(hge_ctx, table, init_row)
        assert st_code == 200, resp
        with pytest.raises(queue.Empty):
            check_event(hge_ctx, evts_webhook, "t1_cols", table, "INSERT", exp_ev_data, webhook_path = "/new")

        where_exp = {"c1": 1}
        set_exp = {"c2": "world"}
        # expected no event hence previous expected data
        st_code, resp = update(hge_ctx, table, where_exp, set_exp)
        assert st_code == 200, resp
        with pytest.raises(queue.Empty):
            check_event(hge_ctx, evts_webhook, "t1_cols", table, "UPDATE", exp_ev_data, webhook_path = "/new")

        where_exp = {"c1": 1}
        set_exp = {"c1": 2}
        exp_ev_data = {
            "old": {"c1": 1, "c2": "world"},
            "new": {"c1": 2, "c2": "world"}
        }
        st_code, resp = update(hge_ctx, table, where_exp, set_exp)
        assert st_code == 200, resp
        check_event(hge_ctx, evts_webhook, "t1_cols", table, "UPDATE", exp_ev_data, webhook_path ="/new")

        where_exp = {"c1": 2}
        exp_ev_data = {
            "old": {"c1": 2, "c2": "world"},
            "new": None
        }
        st_code, resp = delete(hge_ctx, table, where_exp)
        assert st_code == 200, resp
        check_event(hge_ctx, evts_webhook, "t1_cols", table, "DELETE", exp_ev_data, webhook_path = "/new")


class TestDeleteEvtQuery(object):

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx, evts_webhook):
        print("In setup method")
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/basic/setup.yaml')
        assert st_code == 200, resp
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/delete_query/setup.yaml')
        assert st_code == 200, '{}'.format(resp)
        yield
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/delete_query/teardown.yaml')
        assert st_code == 200, resp

    def test_delete_basic(self, hge_ctx, evts_webhook):
        table = {"schema": "hge_tests", "name": "test_t1"}

        init_row = {"c1": 1, "c2": "hello"}
        exp_ev_data = {
            "old": None,
            "new": init_row
        }
        st_code, resp = insert(hge_ctx, table, init_row)
        assert st_code == 200, resp
        with pytest.raises(queue.Empty):
            check_event(hge_ctx, evts_webhook, "t1_all", table, "INSERT", exp_ev_data)

        where_exp = {"c1": 1}
        set_exp = {"c2": "world"}
        exp_ev_data = {
            "old": init_row,
            "new": {"c1": 1, "c2": "world"}
        }
        st_code, resp = update(hge_ctx, table, where_exp, set_exp)
        assert st_code == 200, resp
        with pytest.raises(queue.Empty):
            check_event(hge_ctx, evts_webhook, "t1_all", table, "UPDATE", exp_ev_data)

        exp_ev_data = {
            "old": {"c1": 1, "c2": "world"},
            "new": None
        }
        st_code, resp = delete(hge_ctx, table, where_exp)
        assert st_code == 200, resp
        with pytest.raises(queue.Empty):
            check_event(hge_ctx, evts_webhook, "t1_all", table, "DELETE", exp_ev_data)


class TestEvtSelCols:

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx, evts_webhook):
        print("In setup method")
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/selected_cols/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/selected_cols/teardown.yaml')
        assert st_code == 200, resp

    def test_selected_cols(self, hge_ctx, evts_webhook):
        table = {"schema": "hge_tests", "name": "test_t1"}

        init_row = {"c1": 1, "c2": "hello"}
        exp_ev_data = {
            "old": None,
            "new": {"c1": 1, "c2": "hello"}
        }
        st_code, resp = insert(hge_ctx, table, init_row)
        assert st_code == 200, resp
        check_event(hge_ctx, evts_webhook, "t1_cols", table, "INSERT", exp_ev_data)

        where_exp = {"c1": 1}
        set_exp = {"c2": "world"}
        # expected no event hence previous expected data
        st_code, resp = update(hge_ctx, table, where_exp, set_exp)
        assert st_code == 200, resp
        with pytest.raises(queue.Empty):
            check_event(hge_ctx, evts_webhook, "t1_cols", table, "UPDATE", exp_ev_data)

        where_exp = {"c1": 1}
        set_exp = {"c1": 2}
        exp_ev_data = {
            "old": {"c1": 1, "c2": "world"},
            "new": {"c1": 2, "c2": "world"}
        }
        st_code, resp = update(hge_ctx, table, where_exp, set_exp)
        assert st_code == 200, resp
        check_event(hge_ctx, evts_webhook, "t1_cols", table, "UPDATE", exp_ev_data)

        where_exp = {"c1": 2}
        exp_ev_data = {
            "old": {"c1": 2, "c2": "world"},
            "new": None
        }
        st_code, resp = delete(hge_ctx, table, where_exp)
        assert st_code == 200, resp
        check_event(hge_ctx, evts_webhook, "t1_cols", table, "DELETE", exp_ev_data)

    def test_selected_cols_dep(self, hge_ctx, evts_webhook):
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
    def transact(self, request, hge_ctx, evts_webhook):
        print("In setup method")
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/insert_only/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/insert_only/teardown.yaml')
        assert st_code == 200, resp

    def test_insert_only(self, hge_ctx, evts_webhook):
        table = {"schema": "hge_tests", "name": "test_t1"}

        init_row = {"c1": 1, "c2": "hello"}
        exp_ev_data = {
            "old": None,
            "new": init_row
        }
        st_code, resp = insert(hge_ctx, table, init_row)
        assert st_code == 200, resp
        check_event(hge_ctx, evts_webhook, "t1_insert", table, "INSERT", exp_ev_data)

        where_exp = {"c1": 1}
        set_exp = {"c2": "world"}
        exp_ev_data = {
            "old": init_row,
            "new": {"c1": 1, "c2": "world"}
        }
        st_code, resp = update(hge_ctx, table, where_exp, set_exp)
        assert st_code == 200, resp
        with pytest.raises(queue.Empty):
            check_event(hge_ctx, evts_webhook, "t1_insert", table, "UPDATE", exp_ev_data)

        exp_ev_data = {
            "old": {"c1": 1, "c2": "world"},
            "new": None
        }
        st_code, resp = delete(hge_ctx, table, where_exp)
        assert st_code == 200, resp
        with pytest.raises(queue.Empty):
            check_event(hge_ctx, evts_webhook, "t1_insert", table, "DELETE", exp_ev_data)


class TestEvtSelPayload:

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx, evts_webhook):
        print("In setup method")
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/selected_payload/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/selected_payload/teardown.yaml')
        assert st_code == 200, resp

    def test_selected_payload(self, hge_ctx, evts_webhook):
        table = {"schema": "hge_tests", "name": "test_t1"}

        init_row = {"c1": 1, "c2": "hello"}
        exp_ev_data = {
            "old": None,
            "new": {"c1": 1, "c2": "hello"}
        }
        st_code, resp = insert(hge_ctx, table, init_row)
        assert st_code == 200, resp
        check_event(hge_ctx, evts_webhook, "t1_payload", table, "INSERT", exp_ev_data)

        where_exp = {"c1": 1}
        set_exp = {"c2": "world"}
        exp_ev_data = {
            "old": {"c1": 1},
            "new": {"c1": 1}
        }
        st_code, resp = update(hge_ctx, table, where_exp, set_exp)
        assert st_code == 200, resp
        check_event(hge_ctx, evts_webhook, "t1_payload", table, "UPDATE", exp_ev_data)

        where_exp = {"c1": 1}
        set_exp = {"c1": 2}
        exp_ev_data = {
            "old": {"c1": 1},
            "new": {"c1": 2}
        }
        st_code, resp = update(hge_ctx, table, where_exp, set_exp)
        assert st_code == 200, resp
        check_event(hge_ctx, evts_webhook, "t1_payload", table, "UPDATE", exp_ev_data)

        where_exp = {"c1": 2}
        exp_ev_data = {
            "old": {"c2": "world"},
            "new": None
        }
        st_code, resp = delete(hge_ctx, table, where_exp)
        assert st_code == 200, resp
        check_event(hge_ctx, evts_webhook, "t1_payload", table, "DELETE", exp_ev_data)

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
    def transact(self, request, hge_ctx, evts_webhook):
        print("In setup method")
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/webhook_env/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/webhook_env/teardown.yaml')
        assert st_code == 200, resp

    def test_basic(self, hge_ctx, evts_webhook):
        table = {"schema": "hge_tests", "name": "test_t1"}

        init_row = {"c1": 1, "c2": "hello"}
        exp_ev_data = {
            "old": None,
            "new": init_row
        }
        st_code, resp = insert(hge_ctx, table, init_row)
        assert st_code == 200, resp
        check_event(hge_ctx, evts_webhook, "t1_all", table, "INSERT", exp_ev_data)

        where_exp = {"c1": 1}
        set_exp = {"c2": "world"}
        exp_ev_data = {
            "old": init_row,
            "new": {"c1": 1, "c2": "world"}
        }
        st_code, resp = update(hge_ctx, table, where_exp, set_exp)
        assert st_code == 200, resp
        check_event(hge_ctx, evts_webhook, "t1_all", table, "UPDATE", exp_ev_data)

        exp_ev_data = {
            "old": {"c1": 1, "c2": "world"},
            "new": None
        }
        st_code, resp = delete(hge_ctx, table, where_exp)
        assert st_code == 200, resp
        check_event(hge_ctx, evts_webhook, "t1_all", table, "DELETE", exp_ev_data)

class TestSessionVariables(object):

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx, evts_webhook):
        print("In setup method")
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/basic/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/basic/teardown.yaml')
        assert st_code == 200, resp

    def test_basic(self, hge_ctx, evts_webhook):
        table = {"schema": "hge_tests", "name": "test_t1"}

        init_row = {"c1": 1, "c2": "hello"}
        exp_ev_data = {
            "old": None,
            "new": init_row
        }
        session_variables = { 'x-hasura-role': 'admin', 'x-hasura-allowed-roles': "['admin','user']", 'x-hasura-user-id': '1'}
        st_code, resp = insert(hge_ctx, table, init_row, headers = session_variables)
        assert st_code == 200, resp
        check_event(hge_ctx, evts_webhook, "t1_all", table, "INSERT", exp_ev_data, session_variables = session_variables)

        where_exp = {"c1": 1}
        set_exp = {"c2": "world"}
        exp_ev_data = {
            "old": init_row,
            "new": {"c1": 1, "c2": "world"}
        }
        session_variables = { 'x-hasura-role': 'admin', 'x-hasura-random': 'some_random_info', 'X-Random-Header': 'not_session_variable'}
        st_code, resp = update(hge_ctx, table, where_exp, set_exp, headers = session_variables)
        assert st_code == 200, resp
        session_variables.pop('X-Random-Header')
        check_event(hge_ctx, evts_webhook, "t1_all", table, "UPDATE", exp_ev_data, session_variables = session_variables)

        exp_ev_data = {
            "old": {"c1": 1, "c2": "world"},
            "new": None
        }
        st_code, resp = delete(hge_ctx, table, where_exp)
        assert st_code == 200, resp
        check_event(hge_ctx, evts_webhook, "t1_all", table, "DELETE", exp_ev_data)


class TestManualEvents(object):

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx, evts_webhook):
        print("In setup method")
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/manual_events/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/manual_events/teardown.yaml')
        assert st_code == 200, resp

    def test_basic(self, hge_ctx, evts_webhook):
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/manual_events/enabled.yaml')
        assert st_code == 200, resp
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/manual_events/disabled.yaml')
        assert st_code == 400, resp
