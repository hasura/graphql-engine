from croniter import croniter
from datetime import datetime, timedelta
import itertools
import json
import sqlalchemy

from validate import validate_event_headers, validate_event_webhook
from utils import until_asserts_pass

# The create and delete tests should ideally go in setup and teardown YAML files,
# We can't use that here because, the payload is dynamic i.e. in case of one-off scheduled events
# the value is the current timestamp and in case of cron  Triggers, the cron schedule is
# derived based on the current timestamp

def stringify_datetime(dt):
    return dt.strftime("%Y-%m-%dT%H:%M:%S.%fZ")
class TestScheduledEvent(object):

    @classmethod
    def dir(cls):
        return 'queries/scheduled_triggers'

    webhook_payload = {"foo": "baz"}

    header_conf = [
        {
            "name": "header-key",
            "value": "header-value"
        }
    ]

    def test_scheduled_events(self, hge_ctx, scheduled_triggers_evts_webhook, metadata_schema_url):
        metadata_engine = sqlalchemy.engine.create_engine(metadata_schema_url)

        query = {
            "type": "bulk",
            "args": [
                # Succeeds
                {
                    "type": "create_scheduled_event",
                    "args": {
                        "webhook": f'{scheduled_triggers_evts_webhook.url}/test',
                        "schedule_at": stringify_datetime(datetime.utcnow()),
                        "payload": self.webhook_payload,
                        "headers": self.header_conf,
                        "comment": "test scheduled event",
                    },
                },
                # Fails immediately, with 'dead'
                {
                    "type": "create_scheduled_event",
                    "args": {
                        "webhook": f'{scheduled_triggers_evts_webhook.url}/',
                        "schedule_at": "2020-01-01T00:00:00Z",
                        "payload": self.webhook_payload,
                        "headers": self.header_conf,
                    },
                },
                # Fails on request, trying twice:
                {
                    "type": "create_scheduled_event",
                    "args": {
                        "webhook": f'{scheduled_triggers_evts_webhook.url}/fail',
                        "schedule_at": stringify_datetime(datetime.utcnow()),
                        "payload": self.webhook_payload,
                        "headers": self.header_conf,
                        "retry_conf": {
                            "num_retries": 1,
                            "retry_interval_seconds": 1,
                            "timeout_seconds": 1,
                            "tolerance_seconds": 21600,
                        },
                    },
                },
            ],
        }
        resp = hge_ctx.v1q(query)
        assert len(resp) == 3, resp
        # ensuring that valid event_id is returned for all requests
        assert all(['event_id' in r for r in resp]), resp

        # Here we check the three requests received by the webhook.
        # Collect the three generated events (they may arrive out of order):
        e1 = scheduled_triggers_evts_webhook.get_event(12) # at least 10 sec, see processScheduledTriggers.sleep
        e2 = scheduled_triggers_evts_webhook.get_event(12)
        e3 = scheduled_triggers_evts_webhook.get_event(12)
        [event_fail1, event_fail2, event_success] = sorted([e1,e2,e3], key=lambda e: e['path'])
        # Check the two failures:
        validate_event_webhook(event_fail1['path'],'/fail')
        validate_event_webhook(event_fail2['path'],'/fail')

        # Check the one successful webhook call:
        with metadata_engine.connect() as connection:
            query = '''
                select to_json(timezone('utc', created_at)) as created_at
                from hdb_catalog.hdb_scheduled_events
                where comment = 'test scheduled event'
            '''
            result = connection.execute(query).fetchone()
            assert result is not None
            db_created_at = result['created_at']

        validate_event_webhook(event_success['path'], '/test')
        validate_event_headers(event_success['headers'], {"header-key": "header-value"})
        assert event_success['body']['payload'] == self.webhook_payload
        assert event_success['body']['created_at'] == db_created_at.replace(" ","T") + "Z"
        payload_keys = dict.keys(event_success['body'])
        for k in ["scheduled_time","created_at","id"]: # additional keys
            assert k in payload_keys
        assert scheduled_triggers_evts_webhook.is_queue_empty()

        def try_check_events_statuses():
            with metadata_engine.connect() as connection:
                scheduled_event_statuses = list(
                    connection.execute(
                        "select status, tries from hdb_catalog.hdb_scheduled_events order by status desc"
                    ).fetchall()
                )
            # 3 scheduled events have been created
            # one should be dead because the timestamp was past the tolerance limit
            # one should be delivered because all the parameters were reasonable
            # one should be error because the webhook returns an error state
            assert scheduled_event_statuses == [
                # status       tries
                ( 'error',         2), # num_retries + 1
                ( 'delivered',     1),
                ( 'dead',          0),
            ]

        until_asserts_pass(100, try_check_events_statuses)

# WARNING: The tests in this class are not independent; they depend on the side effects of previous tests.
class TestCronTrigger(object):

    cron_trigger_name = "cron_trigger"
    # setting the test to be after 30 mins, to make sure that
    # any of the events are not delivered.
    min_after_30_mins = (datetime.utcnow() + timedelta(minutes=30)).minute
    cron_schedule = "{} * * * *".format(min_after_30_mins)
    init_time = datetime.utcnow()

    def test_create_cron_schedule_triggers(self, hge_ctx, scheduled_triggers_evts_webhook):
        cron_st_api_query = {
            "type": "create_cron_trigger",
            "args": {
                "name": self.cron_trigger_name,
                "webhook": f"{scheduled_triggers_evts_webhook.url}/foo",
                "schedule": self.cron_schedule,
                "headers": [
                    {
                        "name": "foo",
                        "value": "baz",
                    },
                ],
                "payload": {"foo": "baz"},
                "include_in_metadata": True,
            },
        }
        resp = hge_ctx.v1q(cron_st_api_query)
        # the cron events will be generated based on the current time, they
        # will not be exactly the same though(the server now and now here)
        assert resp['message'] == 'success'

    def test_check_generated_cron_scheduled_events(self, metadata_schema_url):
        metadata_engine = sqlalchemy.engine.create_engine(metadata_schema_url)

        schedule = croniter(self.cron_schedule, self.init_time)
        expected_scheduled_timestamps = list(itertools.islice(schedule.all_next(datetime), 100))
        self.verify_timestamps(metadata_engine, expected_scheduled_timestamps)

    def test_update_existing_cron_trigger(self ,hge_ctx, metadata_schema_url, scheduled_triggers_evts_webhook):
        metadata_engine = sqlalchemy.engine.create_engine(metadata_schema_url)

        expected_scheduled_timestamps = []
        iter = croniter(self.cron_schedule,datetime.utcnow())
        for _ in range(100):
            expected_scheduled_timestamps.append(iter.next(datetime))
        q = {
            "type": "create_cron_trigger",
            "args": {
                "name": self.cron_trigger_name,
                "webhook": f"{scheduled_triggers_evts_webhook.url}/foo",
                "schedule": self.cron_schedule,
                "headers": [
                    {
                        "name": "header-name",
                        "value": "header-value",
                    },
                ],
                "payload": {"foo": "baz"},
                "include_in_metadata": True,
                "replace": True,
            },
        }
        hge_ctx.v1q(q)

        resp = hge_ctx.v1q({'type': 'export_metadata', 'args': {}})

        all_cron_triggers = resp['cron_triggers']
        for cron_trigger in all_cron_triggers:
            if cron_trigger['name'] == self.cron_trigger_name:
                assert cron_trigger['headers'] == [{
                    "name": "header-name",
                    "value": "header-value",
                }]

        # After updating the cron trigger, the future events should have been created
        self.verify_timestamps(metadata_engine, expected_scheduled_timestamps)

    def test_check_fired_webhook_event(self, hge_ctx, scheduled_triggers_evts_webhook):
        q = {
            "type": "create_cron_trigger",
            "args": {
                "name": "test_cron_trigger",
                "webhook": f"{scheduled_triggers_evts_webhook.url}/test",
                "schedule": "* * * * *",
                "headers": [
                    {
                        "name": "header-key",
                        "value": "header-value",
                    },
                ],
                "payload": {"foo": "baz"},
                "include_in_metadata": False,
            },
        }
        hge_ctx.v1q(q)
        # The maximum timeout is set to 75s because, the cron timestamps
        # that are generated will start from the next minute, suppose
        # the cron schedule is "* * * * *" and the time the cron trigger
        # is created is 10:00:00, then the next event will be scheduled
        # at 10:01:00, but the events processor will not process it
        # exactly at the zeroeth second of 10:01. The only guarantee
        # is that, the event processor will start to process the event before
        # 10:01:10 (seel sleep in processScheduledTriggers). So, in the worst
        # case, it will take 70 seconds to process the first scheduled event.
        event = scheduled_triggers_evts_webhook.get_event(75)
        validate_event_webhook(event['path'], '/test')
        validate_event_headers(event['headers'], {"header-key":"header-value"})
        assert event['body']['payload'] == {"foo": "baz"}
        assert event['body']['name'] == 'test_cron_trigger'

    def test_get_cron_triggers(self, hge_ctx, scheduled_triggers_evts_webhook):
        q = {
            "type": "get_cron_triggers",
            "args": {}
        }
        resp = hge_ctx.v1metadataq(q)
        respDict = json.loads(json.dumps(resp))
        assert respDict['cron_triggers'] == [
            {
                "headers": [
                    {
                        "name": "header-name",
                        "value": "header-value",
                    }
                ],
                "include_in_metadata": True,
                "name": self.cron_trigger_name,
                "payload": {
                    "foo": "baz",
                },
                "retry_conf": {
                    "num_retries": 0,
                    "retry_interval_seconds": 10,
                    "timeout_seconds": 60,
                    "tolerance_seconds": 21600,
                },
                "schedule": self.cron_schedule,
                "webhook": f"{scheduled_triggers_evts_webhook.url}/foo",
            },
            {
                "headers": [
                    {
                        "name": "header-key",
                        "value": "header-value",
                    },
                ],
                "include_in_metadata": False,
                "name": "test_cron_trigger",
                "payload": {
                    "foo": "baz",
                },
                "retry_conf": {
                    "num_retries": 0,
                    "retry_interval_seconds": 10,
                    "timeout_seconds": 60,
                    "tolerance_seconds": 21600,
                },
                "schedule": "* * * * *",
                "webhook": f"{scheduled_triggers_evts_webhook.url}/test",
            },
        ]


    def test_export_and_import_cron_triggers(self, hge_ctx, metadata_schema_url, scheduled_triggers_evts_webhook):
        metadata_engine = sqlalchemy.engine.create_engine(metadata_schema_url)

        q = {
            "type": "export_metadata",
            "args": {}
        }
        resp = hge_ctx.v1q(q)
        respDict = json.loads(json.dumps(resp))
        # Only the cron triggers with `include_in_metadata` set to `True`
        # should be exported
        assert respDict['cron_triggers'] == [
            {
                "headers": [
                    {
                        "name": "header-name",
                        "value": "header-value",
                    }
                ],
                "include_in_metadata": True,
                "name": self.cron_trigger_name,
                "payload": {
                    "foo": "baz"
                },
                "schedule": self.cron_schedule,
                "webhook": f"{scheduled_triggers_evts_webhook.url}/foo",
            },
        ]
        q = {
            "type": "replace_metadata",
            "args": {
                "metadata": resp,
            },
        }
        resp = hge_ctx.v1q(q)

        with metadata_engine.connect() as connection:
            sql = '''
                select count(1) as count
                from hdb_catalog.hdb_cron_events
                where trigger_name = %s
            '''
            result = connection.execute(sql, (self.cron_trigger_name,)).fetchone()
            assert result is not None
            count = result['count']
        # Check if the future cron events are created for
        # for a cron trigger while imported from the metadata
        assert int(count) == 100

    def test_attempt_to_create_duplicate_cron_trigger_fail(self, hge_ctx, scheduled_triggers_evts_webhook):
        q = {
            "type": "create_cron_trigger",
            "args": {
                "name": "test_cron_trigger",
                "webhook": f"{scheduled_triggers_evts_webhook.url}/test",
                "schedule": "* * * * *",
                "headers": [
                    {
                        "name": "header-key",
                        "value": "header-value",
                    },
                ],
                "payload": {"foo": "baz"},
                "include_in_metadata": False,
            },
        }
        resp = hge_ctx.v1q(q, expected_status_code = 400)
        assert dict(resp) == {
            "code": "already-exists",
            "error": 'cron trigger with name: test_cron_trigger already exists',
            "path": "$.args"
        }

    def test_delete_cron_scheduled_trigger(self,hge_ctx):
        q = {
            "type": "bulk",
            "args": [
                {
                    "type": "delete_cron_trigger",
                    "args": {
                        "name": self.cron_trigger_name,
                    },
                },
                {
                    "type": "delete_cron_trigger",
                    "args": {
                        "name": "test_cron_trigger",
                    },
                },
            ],
        }
        hge_ctx.v1q(q)

    def verify_timestamps(self, metadata_engine, expected_scheduled_timestamps):
        # Get timestamps in UTC from the db to compare them with the croniter-generated timestamps
        with metadata_engine.connect() as connection:
            sql = '''
                select timezone('utc', scheduled_time) as scheduled_time
                from hdb_catalog.hdb_cron_events
                where trigger_name = %s
                order by scheduled_time asc
            '''
            actual_scheduled_timestamps = list(scheduled_time for (scheduled_time,) in connection.execute(sql, (self.cron_trigger_name,)).fetchall())

        assert actual_scheduled_timestamps == expected_scheduled_timestamps
