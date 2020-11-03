#!/usr/bin/env python3

import pytest
import queue
import time
from validate import check_query_f, check_event

usefixtures = pytest.mark.usefixtures

# Every test in this class requires the events webhook to be running first
# We are also going to mark as server upgrade tests are allowed
# A few tests are going to be excluded with skip_server_upgrade_test mark
pytestmark = [usefixtures('evts_webhook'), pytest.mark.allow_server_upgrade_test]

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
    return insert_many(hge_ctx, table, [row], returning, headers)

def insert_many(hge_ctx, table, rows, returning=[], headers = {}):
    q = {
        "type": "insert",
        "args": {
            "table": table,
            "objects": rows,
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

@usefixtures("per_method_tests_db_state")
class TestCreateAndDelete:

    def test_create_delete(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/create_and_delete.yaml")

    def test_create_reset(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/create_and_reset.yaml")

    # Can't run server upgrade tests, as this test has a schema change
    @pytest.mark.skip_server_upgrade_test
    def test_create_operation_spec_not_provider_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/create_trigger_operation_specs_not_provided_err.yaml")

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/create-delete'

# Smoke test for handling a backlog of events
@usefixtures("per_method_tests_db_state")
class TestEventFlood(object):

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/basic'

    def test_flood(self, hge_ctx, evts_webhook):
        table = {"schema": "hge_tests", "name": "test_t1"}

        payload = range(1,1001)
        rows = list(map(lambda x: {"c1": x, "c2": "hello"}, payload))
        st_code, resp = insert_many(hge_ctx, table, rows)
        assert st_code == 200, resp

        def get_evt():
            # TODO ThreadedHTTPServer helps locally (I only need a timeout of
            # 10 here), but we still need a bit of a long timeout here for CI
            # it seems, since webhook can't keep up there:
            ev_full = evts_webhook.get_event(600)
            return ev_full['body']['event']['data']['new']['c1']
        # Make sure we got all payloads (probably out of order):
        ns = list(map(lambda _: get_evt(), payload))
        ns.sort()
        assert ns == list(payload)


@usefixtures("per_method_tests_db_state")
class TestCreateEvtQuery(object):

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/basic'

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

@usefixtures('per_method_tests_db_state')
class TestRetryConf(object):

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/retry_conf'

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

@usefixtures('per_method_tests_db_state')
class TestEvtHeaders(object):

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/headers'

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

        init_row = {"c1": 1, "c2": "hello", "c3": {"name": "clarke"}}
        exp_ev_data = {
            "old": None,
            "new": {"c1": 1, "c2": "hello", "c3": {"name": "clarke"}}
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
        set_exp = {"c3": {"name": "bellamy"}}
        exp_ev_data = {
            "old": {"c1": 1, "c2": "world", "c3": {"name": "clarke"}},
            "new": {"c1": 1, "c2": "world", "c3": {"name": "bellamy"}}
        }
        st_code, resp = update(hge_ctx, table, where_exp, set_exp)
        assert st_code == 200, resp
        check_event(hge_ctx, evts_webhook, "t1_cols", table, "UPDATE", exp_ev_data, webhook_path ="/new")

        where_exp = {"c1": 1}
        set_exp = {"c1": 2}
        exp_ev_data = {
            "old": {"c1": 1, "c2": "world", "c3": {"name": "bellamy"}},
            "new": {"c1": 2, "c2": "world", "c3": {"name": "bellamy"}}
        }
        st_code, resp = update(hge_ctx, table, where_exp, set_exp)
        assert st_code == 200, resp
        check_event(hge_ctx, evts_webhook, "t1_cols", table, "UPDATE", exp_ev_data, webhook_path ="/new")

        where_exp = {"c1": 2}
        exp_ev_data = {
            "old": {"c1": 2, "c2": "world", "c3": {"name": "bellamy"}},
            "new": None
        }
        st_code, resp = delete(hge_ctx, table, where_exp)
        assert st_code == 200, resp
        check_event(hge_ctx, evts_webhook, "t1_cols", table, "DELETE", exp_ev_data, webhook_path = "/new")

@usefixtures('per_method_tests_db_state')
class TestDeleteEvtQuery(object):

    directory = 'queries/event_triggers'

    setup_files = [
        directory + '/basic/setup.yaml',
        directory + '/delete_query/setup.yaml'
    ]

    teardown_files = [ directory + '/delete_query/teardown.yaml']

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

@usefixtures('per_class_tests_db_state')
class TestEvtSelCols:

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/selected_cols'

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

    @pytest.mark.skip_server_upgrade_test
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

@usefixtures('per_method_tests_db_state')
class TestEvtInsertOnly:

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/insert_only'

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


@usefixtures('per_class_tests_db_state')
class TestEvtSelPayload:

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/selected_payload'

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

@usefixtures('per_method_tests_db_state')
class TestWebhookEnv(object):

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/webhook_env'

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

@usefixtures('per_method_tests_db_state')
class TestSessionVariables(object):

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/basic'

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


@usefixtures('per_method_tests_db_state')
class TestManualEvents(object):

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/manual_events'

    def test_basic(self, hge_ctx, evts_webhook):
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/manual_events/enabled.yaml')
        assert st_code == 200, resp
        st_code, resp = hge_ctx.v1q_f('queries/event_triggers/manual_events/disabled.yaml')
        assert st_code == 400, resp

@usefixtures('per_method_tests_db_state')
class TestEventsAsynchronousExecution(object):

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/async_execution'

    def test_async_execution(self,hge_ctx,evts_webhook):
        """
        A test to check if the events generated by the graphql-engine are
        processed asynchronously. This test measures the time taken to process
        all the events and that time should definitely be lesser than the time
        taken if the events were to be executed sequentially.

        This test inserts 5 rows and the webhook(/timeout_long) takes
        ~5 seconds to process one request. So, if the graphql-engine
        were to process the events sequentially it will take 5 * 5 = 25 seconds.
        Theorotically, all the events should have been processed in ~5 seconds,
        adding a 5 seconds buffer to the comparision, so that this test
        doesn't flake in the CI.
        """
        table = {"schema": "hge_tests", "name": "test_t1"}

        payload = range(1,6)
        rows = list(map(lambda x: {"c1": x, "c2": "hello"}, payload))
        st_code, resp = insert_many(hge_ctx, table, rows)
        start_time = time.perf_counter()
        assert st_code == 200, resp
        for i in range(1,6):
            _ = evts_webhook.get_event(7) # webhook takes 5 seconds to process a request
        end_time = time.perf_counter()
        time_elapsed = end_time - start_time
        assert time_elapsed < 10
