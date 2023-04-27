import pytest
import queue
import time

import fixtures.postgres
from context import EvtsWebhookServer, HGECtx
from utils import *
from validate import check_query_f, check_event, check_event_transformed, check_events

usefixtures = pytest.mark.usefixtures

# Every test in this class requires the events webhook to be running first
pytestmark = usefixtures('evts_webhook')

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
    return hge_ctx.v1q(q)

def insert_mutation(hge_ctx, table, row, headers = {}):
    return insert_many_mutation(hge_ctx, table, [row], headers)

def insert_many_mutation(hge_ctx, table, rows, headers = {}):
    insert_mutation_field = ""
    mutation_name = "insert" + "_" + table["name"]
    if (table["schema"]):
        insert_value_type = table["schema"] +"_" + table["name"] + "_" + "insert" + "_" + "input"
        insert_mutation_field = "insert" + "_" + table["schema"] +"_" + table["name"]
    else:
        insert_value_type = table["name"] + "_" + "insert" + "_" + "input"
        insert_mutation_field = "insert" + "_" + table["name"]

    insert_mutation_query = """
    mutation {mutation_name}($values: [{insert_value_type}!]!) {{
        {insert_mutation_field}(objects: $values) {{
            affected_rows
        }}
    }}
    """.format(mutation_name = mutation_name, insert_value_type = insert_value_type, insert_mutation_field = insert_mutation_field )

    variables = {'values': rows}
    graphql_query = {'query': insert_mutation_query, 'variables': variables}

    hge_ctx.v1graphqlq(graphql_query, headers = headers)

def update_mutation(hge_ctx, table, where_exp, set_exp, headers = {}):
    update_mutation_field = ""
    mutation_name = "update" + "_" + table["name"]

    if (table["schema"]):
        update_mutation_field = "update" + "_" + table["schema"] +"_" + table["name"]
    else:
        update_mutation_field = "update" + "_" + table["name"]

    update_mutation_query = """
    mutation {mutation_name} {{
        {update_mutation_field}(where: {where_exp}, _set: {set_exp}) {{
            affected_rows
        }}
    }}
    """.format(mutation_name = mutation_name,
               update_mutation_field = update_mutation_field,
               where_exp = where_exp,
               set_exp = set_exp)

    print("--- UPDATE MUTATION QUERY ---- \n", update_mutation_query)

    graphql_query = {'query': update_mutation_query}
    resp = hge_ctx.v1graphqlq(graphql_query, headers = headers)

    #print(" ---- UPDATE MUTATION RESP ----", resp)
    return resp

def delete_mutation(hge_ctx, table, where_exp, headers = {}):
    delete_mutation_field = ""
    mutation_name = "delete" + "_" + table["name"]

    if (table["schema"]):
        delete_mutation_field = "delete" + "_" + table["schema"] +"_" + table["name"]
    else:
        delete_mutation_field = "delete" + "_" + table["name"]

    delete_mutation_query = """
    mutation {mutation_name} {{
        {delete_mutation_field}(where: {where_exp}) {{
            affected_rows
        }}
    }}
    """.format(mutation_name = mutation_name,
               delete_mutation_field = delete_mutation_field,
               where_exp = where_exp)

    print("--- DELETE MUTATION QUERY ---- \n", delete_mutation_query)

    graphql_query = {'query': delete_mutation_query}
    resp = hge_ctx.v1graphqlq(graphql_query, headers = headers)

    print(" ---- DELETE MUTATION RESP ----", resp)
    return resp

@usefixtures("per_method_tests_db_state")
class TestEventCreateAndDelete:

    def test_create_delete(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/create_and_delete.yaml")

    def test_create_reset(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/create_and_reset.yaml")

    def test_create_operation_spec_not_provider_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/create_trigger_operation_specs_not_provided_err.yaml")

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/create-delete'

@usefixtures("per_method_tests_db_state")
class TestEventCreateAndResetNonDefaultSource:

    @pytest.fixture(scope='class', autouse=True)
    def another_source(self, owner_engine, add_source):
        backend: fixtures.postgres.Backend = add_source('postgres')
        backend_database = backend.engine.url.database
        assert backend_database is not None

        with fixtures.postgres.switch_schema(owner_engine, backend_database).connect() as connection:
            connection.execute('DROP SCHEMA IF EXISTS hge_tests CASCADE')
        with backend.engine.connect() as connection:
            connection.execute('CREATE SCHEMA hge_tests')

        yield backend

        # TODO: remove once parallelization work is completed
        #       cleanup will no longer be required
        with backend.engine.connect() as connection:
            connection.execute('DROP SCHEMA IF EXISTS hge_tests CASCADE')

    def test_create_reset_non_default_source(self, hge_ctx, another_source):
        check_query_f(hge_ctx, self.dir() + "/create_and_reset_non_default_source.yaml")

        with another_source.engine.connect() as connection:
            # Check that the event log table exists.
            # This must be run against the source database.
            result = connection.execute("SELECT EXISTS (SELECT * FROM information_schema.tables WHERE table_schema = 'hdb_catalog' and table_name = 'event_log')")
            row = result.first()
            assert row == (True,), f'Result: {row!r}'

            # We plan on clearing the metadata in code in the future, so this is not run as YAML input.
            hge_ctx.v1metadataq({
                "type": "clear_metadata",
                "args": {}
            })

            # Check that the event log table has been dropped.
            # This must be run against the source database.
            result = connection.execute("SELECT EXISTS (SELECT * FROM information_schema.tables WHERE table_schema = 'hdb_catalog' and table_name = 'event_log')")
            row = result.first()
            assert row == (False,), f'Result: {row!r}'

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/create_and_reset_non_default'

@pytest.mark.backend('mssql')
@usefixtures("per_method_tests_db_state")
class TestEventCreateAndDeleteMSSQL:

    def test_create_delete(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/create_and_delete_mssql.yaml")

    def test_create_reset(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/create_and_reset_mssql.yaml")

        table = {"schema": "hge_tests", "name": "test_t1"}
        init_row = {"c1": 1, "c2": "world"}
        insert_mutation(hge_ctx, table, init_row)

        check_query_f(hge_ctx, self.dir() + "/create_and_reset_mssql_2.yaml")

    def test_create_operation_spec_not_provider_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/create_trigger_operation_specs_not_provided_err_mssql.yaml")

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/create-delete'


# Generates a backlog of events, then:
# - checks that we're processing with the concurrency and backpressure
#   characteristics we expect
# - ensures all events are successfully processed
@pytest.mark.backend('mssql', 'postgres')
# Set a known batch size for assertions.
@pytest.mark.hge_env('HASURA_GRAPHQL_EVENTS_FETCH_BATCH_SIZE', str(100))
# Set the HTTP pool size to trigger backpressure upon flooding.
@pytest.mark.hge_env('HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE', str(8))
@usefixtures("per_method_tests_db_state")
class TestEventFloodPostgresMSSQL(object):

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/flood'

    def test_flood(self, hge_ctx: HGECtx, evts_webhook: EvtsWebhookServer):
        table = {"schema": "hge_tests", "name": "test_flood"}

        # Trigger a bunch of events; hasura will begin processing but block on /block
        payload = range(1,1001)
        rows = list(map(lambda x: {"c1": x, "c2": "hello"}, payload))

        if (hge_ctx.backend == "postgres"):
            insert_many(hge_ctx, table, rows)
        elif (hge_ctx.backend == "mssql"):
            insert_many_mutation(hge_ctx, table, rows)

        def check_backpressure():
            # Expect that HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE webhooks are pending:
            assert evts_webhook.blocked_count == 8
            # ...Great, so presumably:
            # - event handlers are run concurrently
            # - with concurrency limited by HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE

            if (hge_ctx.backend == "postgres"):
                locked_counts = {
                    "type":"run_sql",
                    "args":{
                        "sql":'''
                        select
                        (select count(*) from hdb_catalog.event_log where locked IS NOT NULL) as num_locked,
                        count(*) as total
                        from hdb_catalog.event_log
                        where table_name = 'test_flood'
                        '''
                    }
                }
                resp = hge_ctx.v1q(locked_counts)

            elif (hge_ctx.backend == "mssql"):
                locked_counts = {
                    "type":"mssql_run_sql",
                    "args":{
                        "source": "mssql",
                        "sql":'''
                        select
                        (select count(*) from hdb_catalog.event_log where locked IS NOT NULL) as num_locked,
                        count(*) as total
                        from hdb_catalog.event_log
                        where table_name = 'test_flood'
                        '''
                    }
                }
                resp = hge_ctx.v2q(locked_counts)
            else:
                raise NotImplementedError('Unknown backend.')

            # Make sure we have 2*HASURA_GRAPHQL_EVENTS_FETCH_BATCH_SIZE events checked out:
            #  - 100 prefetched
            #  - 100 being processed right now (but blocked on HTTP_POOL capacity)
            # TODO it seems like we have some shared state in CI causing this to fail when we check 1000 below
            # assert resp['result'][1] == ['200', '1000']
            if (hge_ctx.backend == "postgres"):
                assert resp['result'][1][0] == '200'
            elif (hge_ctx.backend == "mssql"):
                assert resp['result'][1][0] == 200

        # Rather than sleep arbitrarily, loop until assertions pass:
        until_asserts_pass(30, check_backpressure)
        # ...then make sure we're truly stable:
        time.sleep(3)
        check_backpressure()

        # unblock open and future requests to /block; check all events processed
        evts_webhook.unblock()

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

@usefixtures('postgis', 'per_class_tests_db_state')
class TestEventDataFormat(object):

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/data_format'

    def test_bigint(self, hge_ctx, evts_webhook):
      table = {"schema": "hge_tests", "name": "test_bigint"}

      init_row = {"id": 50755254975729665, "name": "hello"}
      exp_ev_data = {
          "old": None,
          "new": {"id": "50755254975729665", "name": "hello"}
      }

      insert(hge_ctx, table, init_row)
      check_event(hge_ctx, evts_webhook, "bigint_all", table, "INSERT", exp_ev_data)

    def test_geojson(self, hge_ctx, evts_webhook):
      table = {"schema": "hge_tests", "name": "test_geojson"}

      exp_ev_data = {
          "old": {  "id" : 1,
                    "location":{
                        "coordinates":[
                          -43.77,
                          45.64
                        ],
                        "crs":{
                          "type":"name",
                          "properties":{
                              "name":"urn:ogc:def:crs:EPSG::4326"
                          }
                        },
                        "type":"Point"
                    }
                  },
          "new": {  "id": 2,
                    "location":{
                        "coordinates":[
                          -43.77,
                          45.64
                        ],
                        "crs":{
                          "type":"name",
                          "properties":{
                              "name":"urn:ogc:def:crs:EPSG::4326"
                          }
                        },
                        "type":"Point"
                    }
                  }
      }


      where_exp = {"id" : 1}
      set_exp = {"id": 2}
      update(hge_ctx, table, where_exp, set_exp)
      check_event(hge_ctx, evts_webhook, "geojson_all", table, "UPDATE", exp_ev_data)

@pytest.mark.backend('mssql')
@usefixtures("per_class_tests_db_state")
class TestEventDataFormatBigIntMSSQL(object):

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/data_format/mssql/bigint'

    def test_bigint(self, hge_ctx, evts_webhook):
      table = {"schema": "hge_tests", "name": "test_bigint"}

      #init_row = {"id": 50755254975729665, "name": "hello"}
      exp_ev_data = {
          "old": None,
          "new": {"id": 50755254975729665, "name": "hello"}
      }

      # TODO: Naveen: Insert mutation on big int values in MSSQL source
      # does not work as of now, hence using 'run_sql' to directly insert rows
      # and trigger the event trigger. When they are supported in future, we
      # might wanna use the insert_mutation here for consistency.
      #
      # resp = insert_mutation(hge_ctx, table, init_row)
      insert_bigint_sql = {
       "type":"mssql_run_sql",
        "args":{
            "source": "mssql",
            "sql":'''
            INSERT INTO hge_tests.test_bigint ([id], [name]) VALUES (50755254975729665, 'hello')
            '''
        }
      }
      resp = hge_ctx.v2q(insert_bigint_sql)
      print("----------- resp ----------\n", resp)
      check_event(hge_ctx, evts_webhook, "bigint_all", table, "INSERT", exp_ev_data)

@pytest.mark.backend('mssql')
@usefixtures("per_class_tests_db_state")
class TestEventDataFormatGeoJSONMSSQL(object):

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/data_format/mssql/geojson'

    def test_geojson(self, hge_ctx, evts_webhook):
      check_query_f(hge_ctx, self.dir() + '/create_geojson_event_trigger.yaml')

@pytest.mark.backend('mssql','postgres')
@usefixtures("per_class_tests_db_state")
class TestCreateEventQueryPostgresMSSQL(object):

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/basic'

    def test_basic(self, hge_ctx, evts_webhook):
        table = {"schema": "hge_tests", "name": "test_t1"}

        # Check Insert Event Trigger Payload
        init_row = {"c1": 1, "c2": "hello"}
        exp_ev_data = {
            "old": None,
            "new": init_row
        }

        if (hge_ctx.backend == "postgres"):
            insert(hge_ctx, table, init_row)
        elif (hge_ctx.backend == "mssql"):
            insert_mutation(hge_ctx, table, init_row)

        check_event(hge_ctx, evts_webhook, "t1_all", table, "INSERT", exp_ev_data)

        # Check Update Event Trigger Payload
        if (hge_ctx.backend == "postgres"):
            where_exp = {"c1": 1}
            set_exp = {"c2": "world"}
            update(hge_ctx, table, where_exp, set_exp)
        elif (hge_ctx.backend == "mssql"):
            where_exp = '{c1: {_eq: 1}}'
            set_exp = '{c2: "world"}'
            update_mutation(hge_ctx, table, where_exp, set_exp)

        exp_ev_data = {
                "old": init_row,
                "new": {"c1": 1, "c2": "world"}
            }
        check_event(hge_ctx, evts_webhook, "t1_all", table, "UPDATE", exp_ev_data)

        # Check Delete Event Trigger Payload
        if (hge_ctx.backend == "postgres"):
            where_exp = {"c1": 1}
            delete(hge_ctx, table, where_exp)
        elif (hge_ctx.backend == "mssql"):
            where_exp = '{c1: {_eq: 1}}'
            delete_mutation(hge_ctx, table, where_exp)

        exp_ev_data = {
            "old": {"c1": 1, "c2": "world"},
            "new": None
        }

        check_event(hge_ctx, evts_webhook, "t1_all", table, "DELETE", exp_ev_data)

@pytest.mark.backend('postgres')
@usefixtures("per_class_tests_db_state")
class TestCreateEventQueryPostgres(object):

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/basic'

    def test_partitioned_table_basic_insert(self, hge_ctx, evts_webhook):
        hge_ctx.v1q_f(self.dir() + '/partition_table_setup.yaml')
        table = { "schema":"hge_tests", "name": "measurement"}

        init_row = { "city_id": 1, "logdate": "2006-02-02", "peaktemp": 1, "unitsales": 1}

        exp_ev_data = {
            "old": None,
            "new": init_row
        }
        insert(hge_ctx, table, init_row)
        check_event(hge_ctx, evts_webhook, "measurement_all", table, "INSERT", exp_ev_data)
        hge_ctx.v1q_f(self.dir() + '/partition_table_teardown.yaml')

@pytest.mark.backend('mssql','postgres')
@usefixtures('per_method_tests_db_state')
class TestEventRetryConfPostgresMSSQL(object):

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/retry_conf'

    # webhook: http://127.0.0.1:5592/fail
    # retry_conf:
    #   num_retries: 4
    #   interval_sec: 1
    def test_basic(self, hge_ctx, evts_webhook):
        table = {"schema": "hge_tests", "name": "test_t1"}

        init_row = {"c1": 1, "c2": "hello"}
        exp_ev_data = {
            "old": None,
            "new": init_row
        }
        if (hge_ctx.backend == "postgres"):
            insert(hge_ctx, table, init_row)
        elif (hge_ctx.backend == "mssql"):
            insert_mutation(hge_ctx, table, init_row)

        check_event(hge_ctx, evts_webhook, "t1_retry", table, "INSERT", exp_ev_data, webhook_path = "/fail", retry = 0)
        check_event(hge_ctx, evts_webhook, "t1_retry", table, "INSERT", exp_ev_data, webhook_path = "/fail", retry = 1)
        check_event(hge_ctx, evts_webhook, "t1_retry", table, "INSERT", exp_ev_data, webhook_path = "/fail", retry = 2)
        check_event(hge_ctx, evts_webhook, "t1_retry", table, "INSERT", exp_ev_data, webhook_path = "/fail", retry = 3)
        check_event(hge_ctx, evts_webhook, "t1_retry", table, "INSERT", exp_ev_data, webhook_path = "/fail", retry = 4)

    # webhook: http://127.0.0.1:5592/sleep_2s
    # retry_conf:
    #   num_retries: 2
    #   interval_sec: 1
    #   timeout_sec: 1
    def test_timeout_short(self, hge_ctx, evts_webhook):
        table = {"schema": "hge_tests", "name": "test_t2"}

        init_row = {"c1": 1, "c2": "hello"}
        exp_ev_data = {
            "old": None,
            "new": init_row
        }
        if (hge_ctx.backend == "postgres"):
            insert(hge_ctx, table, init_row)
        elif (hge_ctx.backend == "mssql"):
            insert_mutation(hge_ctx, table, init_row)
        check_event(hge_ctx, evts_webhook, "t2_timeout_short", table, "INSERT", exp_ev_data, webhook_path = "/sleep_2s", retry = 0, get_timeout = 5)
        check_event(hge_ctx, evts_webhook, "t2_timeout_short", table, "INSERT", exp_ev_data, webhook_path = "/sleep_2s", retry = 1, get_timeout = 5)
        check_event(hge_ctx, evts_webhook, "t2_timeout_short", table, "INSERT", exp_ev_data, webhook_path = "/sleep_2s", retry = 2, get_timeout = 5)

    # webhook: http://127.0.0.1:5592/sleep_2s
    # retry_conf:
    #   num_retries: 0
    #   interval_sec: 2
    #   timeout_sec: 10
    def test_timeout_long(self, hge_ctx, evts_webhook):
        table = {"schema": "hge_tests", "name": "test_t3"}

        init_row = {"c1": 1, "c2": "hello"}
        exp_ev_data = {
            "old": None,
            "new": init_row
        }
        if (hge_ctx.backend == "postgres"):
            insert(hge_ctx, table, init_row)
        elif (hge_ctx.backend == "mssql"):
            insert_mutation(hge_ctx, table, init_row)
        time.sleep(2)
        check_event(hge_ctx, evts_webhook, "t3_timeout_long", table, "INSERT", exp_ev_data, webhook_path = "/sleep_2s")

    # Keep this one last
    def test_queue_empty(self, hge_ctx, evts_webhook):
        try:
            evts_webhook.get_event(3)
            assert False, "expected queue to be empty"
        except queue.Empty:
            pass

@pytest.mark.backend('mssql', 'postgres')
@pytest.mark.hge_env('EVENT_WEBHOOK_HEADER', 'MyEnvValue')
@usefixtures('per_method_tests_db_state')
class TestEventHeadersPostgresMSSQL(object):

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
        if (hge_ctx.backend == "postgres"):
            insert(hge_ctx, table, init_row)
        elif (hge_ctx.backend == "mssql"):
            insert_mutation(hge_ctx, table, init_row)
        check_event(hge_ctx, evts_webhook, "t1_all", table, "INSERT", exp_ev_data, headers = headers)

class TestUpdateEventQuery(object):

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/update_query'

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx, evts_webhook):
        print("In setup method")
        # Adds trigger on 'test_t1' with...
        #   insert:
        #     columns: '*'
        #   update:
        #     columns: [c2, c3]
        resp = hge_ctx.v1q_f(self.dir() + '/create-setup.yaml')

        # overwrites trigger added above, with...
        #   delete:
        #     columns: "*"
        #   update:
        #     columns: ["c1", "c3"]
        resp = hge_ctx.v1q_f(self.dir() + '/update-setup.yaml')
        assert resp[1]["sources"][0]["tables"][0]["event_triggers"][0]["webhook"] == '{{EVENT_WEBHOOK_HANDLER}}/new'
        yield
        resp = hge_ctx.v1q_f(self.dir() + '/teardown.yaml')

    def test_update_basic(self, hge_ctx, evts_webhook):
        table = {"schema": "hge_tests", "name": "test_t1"}

        # Expect that inserting a row (which would have triggered in original
        # create_event_trigger) does not trigger
        init_row = {"c1": 1, "c2": "hello", "c3": {"name": "clarke"}}
        insert(hge_ctx, table, init_row)
        with pytest.raises(queue.Empty):
            check_event(hge_ctx, evts_webhook, "t1_cols", table, "INSERT", {}, webhook_path = "/new", get_timeout = 0)

        # Likewise for an update on c2:
        where_exp = {"c1": 1}
        set_exp = {"c2": "world"}
        update(hge_ctx, table, where_exp, set_exp)
        with pytest.raises(queue.Empty):
            check_event(hge_ctx, evts_webhook, "t1_cols", table, "UPDATE", {}, webhook_path = "/new", get_timeout = 0)

        where_exp = {"c1": 1}
        set_exp = {"c3": {"name": "bellamy"}}
        exp_ev_data = {
            "old": {"c1": 1, "c2": "world", "c3": {"name": "clarke"}},
            "new": {"c1": 1, "c2": "world", "c3": {"name": "bellamy"}}
        }
        update(hge_ctx, table, where_exp, set_exp)
        check_event(hge_ctx, evts_webhook, "t1_cols", table, "UPDATE", exp_ev_data, webhook_path ="/new")

        where_exp = {"c1": 1}
        set_exp = {"c1": 2}
        exp_ev_data = {
            "old": {"c1": 1, "c2": "world", "c3": {"name": "bellamy"}},
            "new": {"c1": 2, "c2": "world", "c3": {"name": "bellamy"}}
        }
        update(hge_ctx, table, where_exp, set_exp)

        check_event(hge_ctx, evts_webhook, "t1_cols", table, "UPDATE", exp_ev_data, webhook_path ="/new")

        where_exp = {"c1": 2}
        exp_ev_data = {
            "old": {"c1": 2, "c2": "world", "c3": {"name": "bellamy"}},
            "new": None
        }
        delete(hge_ctx, table, where_exp)

        check_event(hge_ctx, evts_webhook, "t1_cols", table, "DELETE", exp_ev_data, webhook_path = "/new")

@pytest.mark.backend('mssql')
class TestUpdateEventQueryMSSQL(object):

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/update_query'

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx, evts_webhook):
        print("In setup method")
        # Adds trigger on 'test_t1' with...
        #   insert:
        #     columns: '*'
        #   update:
        #     columns: ["c3", "c4"]
        hge_ctx.v2q_f(self.dir() + '/schema-setup-mssql.yaml')
        hge_ctx.v1metadataq_f(self.dir() + '/create-setup-mssql.yaml')

        # overwrites trigger added above, with...
        #   delete:
        #     columns: "*"
        #   update:
        #     columns: ["c1", "c2", "c4"]

        resp = hge_ctx.v1metadataq_f(self.dir() + '/update-setup-mssql.yaml')
        sources = resp[1]["sources"]
        for source in sources:
            if source["name"] == "mssql":
                assert source["tables"][0]["event_triggers"][0]["webhook"] == '{{EVENT_WEBHOOK_HANDLER}}/new'

        yield
        print("--- TEARDOWN STARTED -----")
        resp = hge_ctx.v2q_f(self.dir() + '/teardown-mssql.yaml')

    def test_update_basic(self, hge_ctx, evts_webhook):
        table = {"schema": "hge_tests", "name": "test_t1"}

        # Expect that inserting a row (which would have triggered in original
        # create_event_trigger) does not trigger
        init_row = {"c1": 1, "c2": 100, "c3": "hello", "c4": "{'name': 'clarke'}"}
        resp = insert_mutation(hge_ctx, table, init_row)
        with pytest.raises(queue.Empty):
            check_event(hge_ctx, evts_webhook, "t1_cols", table, "INSERT", {}, webhook_path = "/new", get_timeout = 0)

        # Likewise for an update on c3:
        where_exp = '{c1: {_eq: 1}}'
        set_exp = '{c3: "world"}'
        resp = update_mutation(hge_ctx, table, where_exp, set_exp)
        print("--- RESP 1 ---", resp)
        with pytest.raises(queue.Empty):
            check_event(hge_ctx, evts_webhook, "t1_cols", table, "UPDATE", {}, webhook_path = "/new", get_timeout = 0)

        # Update on row c4 should initiate the event trigger
        where_exp = '{c1: {_eq: 1}}'
        set_exp = '{c4: "{\'name\': \'bellamy\'}"}'
        exp_ev_data = {
            "old": {"c1": 1, "c2":100, "c3": "world", "c4": "{'name': 'clarke'}"},
            "new": {"c1": 1, "c2":100, "c3": "world", "c4": "{'name': 'bellamy'}"}
        }
        resp = update_mutation(hge_ctx, table, where_exp, set_exp)
        print("----- RESP 2 ----", resp)

        check_event(hge_ctx, evts_webhook, "t1_cols", table, "UPDATE", exp_ev_data, webhook_path ="/new")

        # Update on row c2 should initiate the event trigger
        where_exp = '{c1: {_eq: 1}}'
        set_exp = '{c2: 101}'
        exp_ev_data = {
            "old": {"c1": 1, "c2":100, "c3": "world", "c4": "{'name': 'bellamy'}"},
            "new": {"c1": 1, "c2":101, "c3": "world", "c4": "{'name': 'bellamy'}"}
        }
        resp = update_mutation(hge_ctx, table, where_exp, set_exp)
        print("----- RESP 3 ----", resp)
        check_event(hge_ctx, evts_webhook, "t1_cols", table, "UPDATE", exp_ev_data, webhook_path ="/new")

        # Test Delete Event Trigger
        where_exp = '{c1: {_eq: 1}}'
        exp_ev_data = {
            "old": {"c1": 1, "c2":101, "c3": "world", "c4": "{'name': 'bellamy'}"},
            "new": None
        }
        resp = delete_mutation(hge_ctx, table, where_exp)
        print("----- RESP 4 ----", resp)
        check_event(hge_ctx, evts_webhook, "t1_cols", table, "DELETE", exp_ev_data, webhook_path = "/new")

@usefixtures('per_method_tests_db_state')
class TestDeleteEventQuery(object):

    directory = 'queries/event_triggers'

    setup_files = [
        directory + '/basic/setup.yaml',
        directory + '/delete_query/setup.yaml'
    ]

    teardown_files = [ directory + '/delete_query/teardown.yaml']

    # Ensure deleting an event trigger works
    def test_delete_basic(self, hge_ctx, evts_webhook):
        table = {"schema": "hge_tests", "name": "test_t1"}

        init_row = {"c1": 1, "c2": "hello"}
        exp_ev_data = {
            "old": None,
            "new": init_row
        }
        insert(hge_ctx, table, init_row)
        with pytest.raises(queue.Empty):
            check_event(hge_ctx, evts_webhook, "t1_all", table, "INSERT", exp_ev_data, get_timeout=0)

        where_exp = {"c1": 1}
        set_exp = {"c2": "world"}
        exp_ev_data = {
            "old": init_row,
            "new": {"c1": 1, "c2": "world"}
        }
        update(hge_ctx, table, where_exp, set_exp)
        with pytest.raises(queue.Empty):
            check_event(hge_ctx, evts_webhook, "t1_all", table, "UPDATE", exp_ev_data, get_timeout=0)

        exp_ev_data = {
            "old": {"c1": 1, "c2": "world"},
            "new": None
        }
        delete(hge_ctx, table, where_exp)
        with pytest.raises(queue.Empty):
            # NOTE: use a bit of a delay here, to catch any stray events generated above
            check_event(hge_ctx, evts_webhook, "t1_all", table, "DELETE", exp_ev_data, get_timeout=2)

@pytest.mark.backend('mssql')
@usefixtures('per_method_tests_db_state')
class TestDeleteEventQueryMSSQL(object):

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/delete_query'

    # Ensure deleting an event trigger works
    def test_delete_basic(self, hge_ctx, evts_webhook):
        table = {"schema": "hge_tests", "name": "test_t1"}

        init_row = {"c1": 1, "c2": "hello"}
        exp_ev_data = {
            "old": None,
            "new": init_row
        }
        resp = insert_mutation(hge_ctx, table, init_row)
        print("----- RESP 1 -----", resp)
        with pytest.raises(queue.Empty):
            check_event(hge_ctx, evts_webhook, "t1_all", table, "INSERT", exp_ev_data, get_timeout=0)

        where_exp = '{c1: {_eq: 1}}'
        set_exp = '{c2: "world"}'
        exp_ev_data = {
            "old": init_row,
            "new": {"c1": 1, "c2": "world"}
        }
        resp = update_mutation(hge_ctx, table, where_exp, set_exp)
        print("----- RESP 2 -----", resp)
        with pytest.raises(queue.Empty):
            check_event(hge_ctx, evts_webhook, "t1_all", table, "UPDATE", exp_ev_data, get_timeout=0)

        exp_ev_data = {
            "old": {"c1": 1, "c2": "world"},
            "new": None
        }
        resp = delete_mutation(hge_ctx, table, where_exp)
        print("----- RESP 3 -----", resp)
        with pytest.raises(queue.Empty):
            # NOTE: use a bit of a delay here, to catch any stray events generated above
            check_event(hge_ctx, evts_webhook, "t1_all", table, "DELETE", exp_ev_data, get_timeout=2)


@usefixtures('per_class_tests_db_state')
class TestEventSelCols:

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
        insert(hge_ctx, table, init_row)
        check_event(hge_ctx, evts_webhook, "t1_cols", table, "INSERT", exp_ev_data)

        where_exp = {"c1": 1}
        set_exp = {"c2": "world"}
        # expected no event hence previous expected data
        update(hge_ctx, table, where_exp, set_exp)
        with pytest.raises(queue.Empty):
            check_event(hge_ctx, evts_webhook, "t1_cols", table, "UPDATE", exp_ev_data, get_timeout=0)

        where_exp = {"c1": 1}
        set_exp = {"c1": 2}
        exp_ev_data = {
            "old": {"c1": 1, "c2": "world"},
            "new": {"c1": 2, "c2": "world"}
        }
        update(hge_ctx, table, where_exp, set_exp)
        check_event(hge_ctx, evts_webhook, "t1_cols", table, "UPDATE", exp_ev_data)

        where_exp = {"c1": 2}
        exp_ev_data = {
            "old": {"c1": 2, "c2": "world"},
            "new": None
        }
        delete(hge_ctx, table, where_exp)
        check_event(hge_ctx, evts_webhook, "t1_cols", table, "DELETE", exp_ev_data)

    def test_selected_cols_dep(self, hge_ctx, evts_webhook):
        resp = hge_ctx.v1q({
            "type": "run_sql",
            "args": {
                "sql": "alter table hge_tests.test_t1 drop column c1"
            }
        }, expected_status_code = 400)
        assert resp['code'] == "dependency-error", resp

        resp = hge_ctx.v1q({
            "type": "run_sql",
            "args": {
                "sql": "alter table hge_tests.test_t1 drop column c2"
            }
        })

@pytest.mark.backend('mssql')
@usefixtures('per_class_tests_db_state')
class TestEventSelColsMSSQL:

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/selected_cols'

    def test_selected_cols(self, hge_ctx, evts_webhook):
        table = {"schema": "hge_tests", "name": "test_t1"}

        init_row = {"c1": 1, "c2": "hello", "c3": "bellamy"}
        exp_ev_data = {
            "old": None,
            "new": {"c1": 1, "c2": "hello", "c3": "bellamy"}
        }
        resp = insert_mutation(hge_ctx, table, init_row)
        print("----- RESP 1 -----", resp)
        check_event(hge_ctx, evts_webhook, "t1_cols", table, "INSERT", exp_ev_data)

        where_exp = '{c1: {_eq: 1}}'
        set_exp = '{c1: 2}'

        # expected no event hence previous expected data
        resp = update_mutation(hge_ctx, table, where_exp, set_exp)
        print("----- RESP 2 -----", resp)
        with pytest.raises(queue.Empty):
            check_event(hge_ctx, evts_webhook, "t1_cols", table, "UPDATE", exp_ev_data, get_timeout=0)

        where_exp = '{c1: {_eq: 2}}'
        set_exp = '{c2: "world"}'
        exp_ev_data = {
            "old": {"c1": 2, "c2": "hello", "c3": "bellamy"},
            "new": {"c1": 2, "c2": "world", "c3": "bellamy"}
        }

        resp = update_mutation(hge_ctx, table, where_exp, set_exp)
        print("----- RESP 3 -----", resp)
        check_event(hge_ctx, evts_webhook, "t1_cols", table, "UPDATE", exp_ev_data)

        where_exp = '{c1: {_eq: 2}}'
        exp_ev_data = {
            "old": {"c1": 2, "c2": "world", "c3": "bellamy"},
            "new": None
        }
        resp = delete_mutation(hge_ctx, table, where_exp)
        print("----- RESP 4 -----", resp)
        check_event(hge_ctx, evts_webhook, "t1_cols", table, "DELETE", exp_ev_data)

    def test_selected_cols_dep(self, hge_ctx, evts_webhook):
        # Dropping Primary Key is not allowed
        resp = hge_ctx.v2q({
            "type": "mssql_run_sql",
            "args": {
                "source": "mssql",
                "sql": "alter table hge_tests.test_t1 drop column c1"
            }
        }, expected_status_code = 400)
        assert resp['code'] == "bad-request", resp

        # 'C2' cannot be dropped because event trigger is created on that column
        resp = hge_ctx.v2q({
            "type": "mssql_run_sql",
            "args": {
                "source": "mssql",
                "sql": "alter table hge_tests.test_t1 drop column c2"
            }
        }, expected_status_code = 400)
        print("----- RESP 5 -----", resp)
        assert resp['code'] == "dependency-error", resp

        resp = hge_ctx.v2q({
            "type": "mssql_run_sql",
            "args": {
                "source": "mssql",
                "sql": "alter table hge_tests.test_t1 drop column c3"
            }
        })
        print("----- RESP 6 -----", resp)

@usefixtures('per_method_tests_db_state')
class TestEventInsertOnly:

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
        insert(hge_ctx, table, init_row)
        check_event(hge_ctx, evts_webhook, "t1_insert", table, "INSERT", exp_ev_data)

        where_exp = {"c1": 1}
        set_exp = {"c2": "world"}
        exp_ev_data = {
            "old": init_row,
            "new": {"c1": 1, "c2": "world"}
        }
        update(hge_ctx, table, where_exp, set_exp)
        with pytest.raises(queue.Empty):
            check_event(hge_ctx, evts_webhook, "t1_insert", table, "UPDATE", exp_ev_data, get_timeout=0)

        exp_ev_data = {
            "old": {"c1": 1, "c2": "world"},
            "new": None
        }
        delete(hge_ctx, table, where_exp)
        with pytest.raises(queue.Empty):
            # NOTE: use a bit of a delay here, to catch any stray events generated above
            check_event(hge_ctx, evts_webhook, "t1_insert", table, "DELETE", exp_ev_data, get_timeout=2)

@pytest.mark.backend('mssql')
@usefixtures('per_method_tests_db_state')
class TestEventInsertOnlyMSSQL:

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
        resp = insert_mutation(hge_ctx, table, init_row)
        print("----- RESP 1 -----", resp)
        check_event(hge_ctx, evts_webhook, "t1_insert", table, "INSERT", exp_ev_data)

        where_exp = '{c1: {_eq: 1}}'
        set_exp = '{c2: "world"}'
        exp_ev_data = {
            "old": init_row,
            "new": {"c1": 1, "c2": "world"}
        }
        resp = update_mutation(hge_ctx, table, where_exp, set_exp)
        print("----- RESP 2 -----", resp)
        with pytest.raises(queue.Empty):
            check_event(hge_ctx, evts_webhook, "t1_insert", table, "UPDATE", exp_ev_data, get_timeout=0)

        exp_ev_data = {
            "old": {"c1": 1, "c2": "world"},
            "new": None
        }
        resp = delete_mutation(hge_ctx, table, where_exp)
        print("----- RESP 3 -----", resp)
        with pytest.raises(queue.Empty):
            # NOTE: use a bit of a delay here, to catch any stray events generated above
            check_event(hge_ctx, evts_webhook, "t1_insert", table, "DELETE", exp_ev_data, get_timeout=2)

@pytest.mark.backend('mssql')
@usefixtures('per_method_tests_db_state')
class TestEventUpdateOnlyMSSQL:

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/update_only'

    def test_update_only(self, hge_ctx, evts_webhook):
        table = {"schema": "hge_tests", "name": "test_t1"}

        init_row = {"c1": 1, "c2": "hello"}
        exp_ev_data = {
            "old": None,
            "new": init_row
        }
        resp = insert_mutation(hge_ctx, table, init_row)
        print("----- RESP 1 -----", resp)
        # INSERT operations will not fire event triggers
        with pytest.raises(queue.Empty):
            check_event(hge_ctx, evts_webhook, "t1_update", table, "INSERT", exp_ev_data, get_timeout=0)

        # CASE 1: Primary key is not changed, and some updates happen
        where_exp = '{c1: {_eq: 1}}'
        set_exp = '{c2: "world"}'
        exp_ev_data = {
            "old": {"c1": 1, "c2": "hello"},
            "new": {"c1": 1, "c2": "world"}
        }
        resp = update_mutation(hge_ctx, table, where_exp, set_exp)
        print("----- RESP 2 -----", resp)
        check_event(hge_ctx, evts_webhook, "t1_update", table, "UPDATE", exp_ev_data)

        # CASE 2: Primary key has changed.
        # When PK of MSSQL changes, then old data will be NULL
        where_exp = '{c1: {_eq: 1}}'
        set_exp = '{c1: 2}'
        exp_ev_data = {
            "old": None,
            "new": {"c1": 2, "c2": "world"}
        }
        resp = update_mutation(hge_ctx, table, where_exp, set_exp)
        print("----- RESP 3 -----", resp)
        check_event(hge_ctx, evts_webhook, "t1_update", table, "UPDATE", exp_ev_data)

        # DELETE operations will not fire event triggers
        where_exp = '{c1: {_eq: 2}}'
        exp_ev_data = {
            "old": {"c1": 2, "c2": "world"},
            "new": None
        }
        resp = delete_mutation(hge_ctx, table, where_exp)
        print("----- RESP 4 -----", resp)
        with pytest.raises(queue.Empty):
            # NOTE: use a bit of a delay here, to catch any stray events generated above
            check_event(hge_ctx, evts_webhook, "t1_update", table, "DELETE", exp_ev_data, get_timeout=2)

    # CASE 3: An Update transaction, which can give rise to both CASE 1 and CASE 2
    # described above.
    # i.e for a single update transaction which changes the primary key of a row
    # and a non primary key of another row, 2 event triggers should be fired.
    def test_update_both_cases(self, hge_ctx, evts_webhook):
      table = {"schema": "hge_tests", "name": "test_t1"}
      exp_insert_ev_data = {}

      # Set up the table values to check the update transaction
      insert_values_sql = {
        "type":"mssql_run_sql",
        "args":{
            "source": "mssql",
            "sql":'''
            INSERT INTO hge_tests.test_t1 ([c1], [c2]) VALUES (1, 'hello'), (2, 'world')
            '''
        }
      }
      resp = hge_ctx.v2q(insert_values_sql)
      print("----------- resp ----------\n", resp)

      # INSERT operations will not fire event triggers
      with pytest.raises(queue.Empty):
          check_event(hge_ctx, evts_webhook, "t1_update", table, "INSERT", exp_insert_ev_data, get_timeout=0)

      # An UPDATE SQL which will create two events, one for each case
      # The following update transaction does the following changes
      # We have the following values in table [(1, 'hello'), (2, 'world')]
      # UPDATE transaction changes that to    [(2, 'hello'), (3, 'clarke')]
      update_values_sql = {
        "type":"mssql_run_sql",
        "args":{
            "source": "mssql",
            "sql":'''
            UPDATE hge_tests.test_t1
            SET c1 = (CASE WHEN c1 = 1 THEN 2
                           WHEN c1 = 2 THEN 3
                           ELSE c1 END),
                c2 = (CASE WHEN c1 = 2 THEN N'clarke' ELSE c2 END)
            '''
        }
      }

      # CASE 2.1 : Primary key ('id') is updated, but the updated primary key value
      # is already present in the table, then both data.old and data.new will be
      # constructed
      exp_ev_data_case_1 = {
        "old": {"c1": 2, "c2": "world"},
        "new": {"c1": 2, "c2": "hello"}
      }

      # CASE 2.2: Primary key ('id') is updated to a value which did not exists before
      # in the table, so "data.old" will be NULL
      exp_ev_data_case_2 = {
        "old": None,
        "new": {"c1": 3, "c2": "clarke"}
      }

      resp = hge_ctx.v2q(update_values_sql)
      print("----------- resp ----------\n", resp)

      exp_ev_datas = [exp_ev_data_case_1, exp_ev_data_case_2]

      # The UPDATE SQL above will trigger exactly two triggers, one for each case
      check_events(hge_ctx, evts_webhook, "t1_update", table, "UPDATE", 2, exp_ev_datas)

@usefixtures('per_class_tests_db_state')
class TestEventSelPayload:

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
        insert(hge_ctx, table, init_row)
        check_event(hge_ctx, evts_webhook, "t1_payload", table, "INSERT", exp_ev_data)

        where_exp = {"c1": 1}
        set_exp = {"c2": "world"}
        exp_ev_data = {
            "old": {"c1": 1},
            "new": {"c1": 1}
        }
        update(hge_ctx, table, where_exp, set_exp)
        check_event(hge_ctx, evts_webhook, "t1_payload", table, "UPDATE", exp_ev_data)

        where_exp = {"c1": 1}
        set_exp = {"c1": 2}
        exp_ev_data = {
            "old": {"c1": 1},
            "new": {"c1": 2}
        }
        update(hge_ctx, table, where_exp, set_exp)
        check_event(hge_ctx, evts_webhook, "t1_payload", table, "UPDATE", exp_ev_data)

        where_exp = {"c1": 2}
        exp_ev_data = {
            "old": {"c2": "world"},
            "new": None
        }
        delete(hge_ctx, table, where_exp)
        check_event(hge_ctx, evts_webhook, "t1_payload", table, "DELETE", exp_ev_data)

    def test_selected_payload_dep(self, hge_ctx):
        resp = hge_ctx.v1q({
            "type": "run_sql",
            "args": {
                "sql": "alter table hge_tests.test_t1 drop column c1"
            }
        }, expected_status_code = 400)
        assert resp['code'] == "dependency-error", resp

        resp = hge_ctx.v1q({
            "type": "run_sql",
            "args": {
                "sql": "alter table hge_tests.test_t1 drop column c2"
            }
        }, expected_status_code = 400)
        assert resp['code'] == "dependency-error", resp

@pytest.mark.backend('mssql')
@usefixtures('per_class_tests_db_state')
class TestEventSelPayloadMSSQL:

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/selected_payload'

    def test_selected_payload(self, hge_ctx, evts_webhook):
        table = {"schema": "hge_tests", "name": "test_t1"}

        init_row = {"c1": 1, "c2": "hello", "c3": "bellamy"}
        exp_ev_data = {
            "old": None,
            "new": {"c1": 1, "c2": "hello", "c3": "bellamy"}
        }
        resp = insert_mutation(hge_ctx, table, init_row)
        print("----- RESP 1 -----", resp)
        check_event(hge_ctx, evts_webhook, "t1_payload", table, "INSERT", exp_ev_data)

        where_exp = '{c1: {_eq: 1}}'
        set_exp = '{c2: "world"}'
        exp_ev_data = {
            "old": {"c2": "hello"},
            "new": {"c2": "world"}
        }
        resp = update_mutation(hge_ctx, table, where_exp, set_exp)
        print("----- RESP 2 -----", resp)
        check_event(hge_ctx, evts_webhook, "t1_payload", table, "UPDATE", exp_ev_data)

        where_exp = '{c1: {_eq: 1}}'
        set_exp = '{c3: "harry"}'
        exp_ev_data = {
            "old": {"c2": "world"},
            "new": {"c2": "world"}
        }
        resp = update_mutation(hge_ctx, table, where_exp, set_exp)
        print("----- RESP 3 -----", resp)
        check_event(hge_ctx, evts_webhook, "t1_payload", table, "UPDATE", exp_ev_data)

        where_exp = '{c1: {_eq: 1}}'
        exp_ev_data = {
            "old": {"c3": "harry"},
            "new": None
        }
        resp = delete_mutation(hge_ctx, table, where_exp)
        print("----- RESP 4 -----", resp)
        check_event(hge_ctx, evts_webhook, "t1_payload", table, "DELETE", exp_ev_data)

    def test_selected_payload_dep(self, hge_ctx):
        # Dropping Primary Key is not allowed
        resp = hge_ctx.v2q({
            "type": "mssql_run_sql",
            "args": {
                "source": "mssql",
                "sql": "alter table hge_tests.test_t1 drop column c1"
            }
        }, expected_status_code = 400)
        print("----- RESP 5 -----", resp)
        assert resp['code'] == "bad-request", resp

        # 'C2' cannot be dropped because event trigger is created on that column
        resp = hge_ctx.v2q({
            "type": "mssql_run_sql",
            "args": {
                "source": "mssql",
                "sql": "alter table hge_tests.test_t1 drop column c2"
            }
        }, expected_status_code = 400)
        print("----- RESP 6 -----", resp)
        assert resp['code'] == "dependency-error", resp

        # 'C3' cannot be dropped because event trigger is created on that column
        resp = hge_ctx.v2q({
            "type": "mssql_run_sql",
            "args": {
                "source": "mssql",
                "sql": "alter table hge_tests.test_t1 drop column c3"
            }
        }, expected_status_code = 400)
        print("----- RESP 7 -----", resp)
        assert resp['code'] == "dependency-error", resp

@usefixtures('per_method_tests_db_state')
class TestWebhookEvent(object):

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
        insert(hge_ctx, table, init_row)
        check_event(hge_ctx, evts_webhook, "t1_all", table, "INSERT", exp_ev_data)

        where_exp = {"c1": 1}
        set_exp = {"c2": "world"}
        exp_ev_data = {
            "old": init_row,
            "new": {"c1": 1, "c2": "world"}
        }
        update(hge_ctx, table, where_exp, set_exp)
        check_event(hge_ctx, evts_webhook, "t1_all", table, "UPDATE", exp_ev_data)

        exp_ev_data = {
            "old": {"c1": 1, "c2": "world"},
            "new": None
        }
        delete(hge_ctx, table, where_exp)
        check_event(hge_ctx, evts_webhook, "t1_all", table, "DELETE", exp_ev_data)

@pytest.mark.backend('mssql')
@usefixtures('per_method_tests_db_state')
class TestWebhookEventMSSQL(object):

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
        resp = insert_mutation(hge_ctx, table, init_row)
        print("----- RESP 1 -----", resp)
        check_event(hge_ctx, evts_webhook, "t1_all", table, "INSERT", exp_ev_data)

        where_exp = '{c1: {_eq: 1}}'
        set_exp = '{c2: "world"}'
        exp_ev_data = {
            "old": init_row,
            "new": {"c1": 1, "c2": "world"}
        }
        resp = update_mutation(hge_ctx, table, where_exp, set_exp)
        print("----- RESP 2 -----", resp)
        check_event(hge_ctx, evts_webhook, "t1_all", table, "UPDATE", exp_ev_data)

        exp_ev_data = {
            "old": {"c1": 1, "c2": "world"},
            "new": None
        }
        resp = delete_mutation(hge_ctx, table, where_exp)
        print("----- RESP 3 -----", resp)
        check_event(hge_ctx, evts_webhook, "t1_all", table, "DELETE", exp_ev_data)

@usefixtures('per_method_tests_db_state')
class TestEventWebhookTemplateURL(object):

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/webhook_template_url'

    def test_basic(self, hge_ctx, evts_webhook):
        table = {"schema": "hge_tests", "name": "test_t1"}

        init_row = {"c1": 1, "c2": "hello"}
        exp_ev_data = {
            "old": None,
            "new": init_row
        }
        insert(hge_ctx, table, init_row)
        check_event(hge_ctx, evts_webhook, "t1_all", table, "INSERT", exp_ev_data, webhook_path = '/trigger')

        where_exp = {"c1": 1}
        set_exp = {"c2": "world"}
        exp_ev_data = {
            "old": init_row,
            "new": {"c1": 1, "c2": "world"}
        }
        update(hge_ctx, table, where_exp, set_exp)
        check_event(hge_ctx, evts_webhook, "t1_all", table, "UPDATE", exp_ev_data, webhook_path = '/trigger')

        exp_ev_data = {
            "old": {"c1": 1, "c2": "world"},
            "new": None
        }
        delete(hge_ctx, table, where_exp)
        check_event(hge_ctx, evts_webhook, "t1_all", table, "DELETE", exp_ev_data, webhook_path = '/trigger')

@pytest.mark.backend('mssql')
@usefixtures('per_method_tests_db_state')
class TestEventWebhookTemplateURLMSSQL(object):

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/webhook_template_url'

    def test_basic(self, hge_ctx, evts_webhook):
        table = {"schema": "hge_tests", "name": "test_t1"}

        init_row = {"c1": 1, "c2": "hello"}
        exp_ev_data = {
            "old": None,
            "new": init_row
        }
        resp = insert_mutation(hge_ctx, table, init_row)
        print("----- RESP 1 -----", resp)
        check_event(hge_ctx, evts_webhook, "t1_all", table, "INSERT", exp_ev_data, webhook_path = '/trigger')

        where_exp = '{c1: {_eq: 1}}'
        set_exp = '{c2: "world"}'
        exp_ev_data = {
            "old": init_row,
            "new": {"c1": 1, "c2": "world"}
        }
        resp = update_mutation(hge_ctx, table, where_exp, set_exp)
        print("----- RESP 2 -----", resp)
        check_event(hge_ctx, evts_webhook, "t1_all", table, "UPDATE", exp_ev_data, webhook_path = '/trigger')

        exp_ev_data = {
            "old": {"c1": 1, "c2": "world"},
            "new": None
        }
        resp = delete_mutation(hge_ctx, table, where_exp)
        print("----- RESP 3 -----", resp)
        check_event(hge_ctx, evts_webhook, "t1_all", table, "DELETE", exp_ev_data, webhook_path = '/trigger')

@usefixtures('per_method_tests_db_state')
class TestEventSessionVariables(object):

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
        insert(hge_ctx, table, init_row, headers = session_variables)
        check_event(hge_ctx, evts_webhook, "t1_all", table, "INSERT", exp_ev_data, session_variables = session_variables)

        where_exp = {"c1": 1}
        set_exp = {"c2": "world"}
        exp_ev_data = {
            "old": init_row,
            "new": {"c1": 1, "c2": "world"}
        }
        session_variables = { 'x-hasura-role': 'admin', 'x-hasura-random': 'some_random_info', 'X-Random-Header': 'not_session_variable'}
        update(hge_ctx, table, where_exp, set_exp, headers = session_variables)
        session_variables.pop('X-Random-Header')
        check_event(hge_ctx, evts_webhook, "t1_all", table, "UPDATE", exp_ev_data, session_variables = session_variables)

        exp_ev_data = {
            "old": {"c1": 1, "c2": "world"},
            "new": None
        }
        delete(hge_ctx, table, where_exp)
        check_event(hge_ctx, evts_webhook, "t1_all", table, "DELETE", exp_ev_data)

@pytest.mark.backend('mssql')
@usefixtures('per_method_tests_db_state')
class TestEventSessionVariablesMSSQL(object):

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
        resp = insert_mutation(hge_ctx, table, init_row, headers = session_variables)
        print("----- RESP 1 -----", resp)
        check_event(hge_ctx, evts_webhook, "t1_all", table, "INSERT", exp_ev_data, session_variables = session_variables)

        where_exp = '{c1: {_eq: 1}}'
        set_exp = '{c2: "world"}'
        exp_ev_data = {
            "old": init_row,
            "new": {"c1": 1, "c2": "world"}
        }
        session_variables = { 'x-hasura-role': 'admin', 'x-hasura-random': 'some_random_info', 'X-Random-Header': 'not_session_variable'}
        resp = update_mutation(hge_ctx, table, where_exp, set_exp, headers = session_variables)
        print("----- RESP 2 -----", resp)
        session_variables.pop('X-Random-Header')
        check_event(hge_ctx, evts_webhook, "t1_all", table, "UPDATE", exp_ev_data, session_variables = session_variables)

        exp_ev_data = {
            "old": {"c1": 1, "c2": "world"},
            "new": None
        }
        resp = delete_mutation(hge_ctx, table, where_exp)
        print("----- RESP 3 -----", resp)
        check_event(hge_ctx, evts_webhook, "t1_all", table, "DELETE", exp_ev_data)

@usefixtures('per_method_tests_db_state')
class TestManualEvents(object):

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/manual_events'

    def test_basic(self, hge_ctx, evts_webhook):
        resp = hge_ctx.v1metadataq_f(
            'queries/event_triggers/manual_events/enabled.yaml')
        print("----- RESP 1 -----", resp)
        resp = hge_ctx.v1metadataq_f(
            'queries/event_triggers/manual_events/disabled.yaml',
            expected_status_code = 400)
        print("----- RESP 2 -----", resp)

    # This test is being added to ensure that the manual events
    # are not failing after any reload_metadata operation, this
    # has been an issue of concern in some of the recent releases(v2.0.1 onwards)
    def test_basic_with_reload_metadata(self, hge_ctx, evts_webhook):
        reload_metadata_q = {
            "type": "reload_metadata",
            "args": {
                "source": "mssql",
                "reload_sources": True
            }
        }

        for _ in range(5):
            self.test_basic(hge_ctx, evts_webhook)

            resp = hge_ctx.v1metadataq(reload_metadata_q)
            print("----- RESP 3 -----", resp)

            self.test_basic(hge_ctx, evts_webhook)

@pytest.mark.backend('mssql')
@usefixtures('per_method_tests_db_state')
class TestManualEventsMSSQL(object):

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/manual_events'

    def test_basic(self, hge_ctx, evts_webhook):
        hge_ctx.v1metadataq_f(
            'queries/event_triggers/manual_events/enabled-mssql.yaml')
        hge_ctx.v1metadataq_f(
            'queries/event_triggers/manual_events/disabled-mssql.yaml',
            expected_status_code = 400)

    # This test is being added to ensure that the manual events
    # are not failing after any reload_metadata operation, this
    # has been an issue of concern in some of the recent releases(v2.0.1 onwards)
    def test_basic_with_reload_metadata(self, hge_ctx, evts_webhook):
        reload_metadata_q = {
            "type": "reload_metadata",
            "args": {
                "reload_sources": True
            }
        }

        for _ in range(5):
            self.test_basic(hge_ctx, evts_webhook)

            hge_ctx.v1metadataq(reload_metadata_q)

            self.test_basic(hge_ctx, evts_webhook)

@pytest.mark.backend('mssql','postgres')
@usefixtures('per_method_tests_db_state')
class TestEventsAsynchronousExecutionPostgresMSSQL(object):

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/async_execution'

    def test_async_execution(self,hge_ctx,evts_webhook):
        """
        A test to check if the events generated by the graphql-engine are
        processed asynchronously. This test measures the time taken to process
        all the events and that time should definitely be lesser than the time
        taken if the events were to be executed sequentially.

        This test inserts 5 rows and the webhook(/sleep_2s) takes
        ~2 seconds to process one request. So, if the graphql-engine
        were to process the events sequentially it will take 5 * 2 = 10 seconds.
        Theorotically, all the events should have been processed in ~2 seconds,
        adding a 5 seconds buffer to the comparision, so that this test
        doesn't flake in the CI.
        """
        table = {"schema": "hge_tests", "name": "test_t1"}

        payload = range(1,6)
        rows = list(map(lambda x: {"c1": x, "c2": "hello"}, payload))
        if (hge_ctx.backend == "postgres"):
            resp = insert_many(hge_ctx, table, rows)
        elif (hge_ctx.backend == "mssql"):
            resp = insert_many_mutation(hge_ctx, table, rows)
        else:
            raise NotImplementedError("Unknown backend.")
        print("----- RESP 1 -----", resp)
        start_time = time.perf_counter()
        for _ in range(1,6):
            evts_webhook.get_event(5) # webhook takes 2 seconds to process a request (+ buffer)
        end_time = time.perf_counter()
        time_elapsed = end_time - start_time
        assert time_elapsed < 10

@usefixtures("per_class_tests_db_state")
class TestEventTransform(object):

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/transform'

    def test_basic(self, hge_ctx, evts_webhook):
        # GIVEN
        check_query_f(hge_ctx, self.dir() + '/basic_transform.yaml')

        # WHEN
        table = {"schema": "hge_tests", "name": "test_t1"}
        insert_row = {"id": 0, "first_name": "Simon", "last_name": "Marlow"}
        insert(hge_ctx, table, insert_row)

        # THEN
        expectedPath = "/?foo=bar"
        expectedBody = insert_row

        check_event_transformed(hge_ctx,
                                evts_webhook,
                                expectedBody,
                                headers={"foo": "bar"},
                                removedHeaders=["user-agent"],
                                webhook_path=expectedPath)

@pytest.mark.backend('mssql')
@usefixtures("per_method_tests_db_state")
class TestEventTransformMSSQL(object):

    @classmethod
    def dir(cls):
        return 'queries/event_triggers/transform'

    def test_basic(self, hge_ctx, evts_webhook):
        # GIVEN
        check_query_f(hge_ctx, self.dir() + '/basic_transform_mssql.yaml')

        # WHEN
        table = {"schema": "hge_tests", "name": "test_t1"}
        insert_row = {"id": 0, "first_name": "Simon", "last_name": "Marlow"}
        resp = insert_mutation(hge_ctx, table, insert_row)
        print("----- RESP 1 -----", resp)

        # THEN
        expectedPath = "/?foo=bar"
        expectedBody = insert_row

        check_event_transformed(hge_ctx,
                                evts_webhook,
                                expectedBody,
                                headers={"foo": "bar"},
                                removedHeaders=["user-agent"],
                                webhook_path=expectedPath)
