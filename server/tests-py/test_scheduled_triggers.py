#!/usr/bin/env python3

import pytest
from datetime import datetime,timedelta
from croniter import croniter
from validate import validate_event_webhook,validate_event_headers
from queue import Empty
import json
import time

# The create and delete tests should ideally go in setup and teardown YAML files,
# We can't use that here because, the payload is dynamic i.e. in case of one-off scheduled events
# the value is the current timestamp and in case of cron  Triggers, the cron schedule is
# derived based on the current timestamp

def stringify_datetime(dt):
    return dt.strftime("%Y-%m-%dT%H:%M:%S.%fZ")

class TestScheduledEvent(object):

    webhook_payload = {"foo":"baz"}

    header_conf = [
        {
            "name":"header-key",
            "value":"header-value"
        }
    ]

    url = "/v1/query"

    webhook_domain = "http://127.0.0.1:5594"


    # Succeeds
    def test_create_scheduled_event(self,hge_ctx):
        query = {
            "type":"create_scheduled_event",
            "args":{
                "webhook":'{{SCHEDULED_TRIGGERS_WEBHOOK_DOMAIN}}/test',
                "schedule_at":stringify_datetime(datetime.utcnow()),
                "payload":self.webhook_payload,
                "headers":self.header_conf,
                "comment":"test scheduled event"
            }
        }
        st, resp = hge_ctx.v1q(query)
        assert st == 200,resp

    # Fails immediately, with 'dead'
    def test_create_scheduled_event_with_very_old_scheduled_time(self,hge_ctx):
        query = {
            "type":"create_scheduled_event",
            "args":{
                "webhook":"{{SCHEDULED_TRIGGERS_WEBHOOK_DOMAIN}}/",
                "schedule_at": "2020-01-01T00:00:00Z",
                "payload":self.webhook_payload,
                "headers":self.header_conf
            }
        }
        st, resp = hge_ctx.v1q(query)
        assert st == 200,resp

    # Fails on request, trying twice:
    def test_create_trigger_with_error_returning_webhook(self,hge_ctx):
        query = {
            "type":"create_scheduled_event",
            "args":{
                "webhook":self.webhook_domain + '/fail',
                "schedule_at": stringify_datetime(datetime.utcnow()),
                "payload":self.webhook_payload,
                "headers":self.header_conf,
                "retry_conf":{
                    "num_retries":1,
                    "retry_interval_seconds":1,
                    "timeout_seconds":1,
                    "tolerance_seconds": 21600
                }
            }
        }
        st, resp = hge_ctx.v1q(query)
        assert st == 200, resp

    # Here we check the three requests received by the webhook, from above.
    def test_check_fired_webhook_events(self,hge_ctx,scheduled_triggers_evts_webhook):
        # Collect the three generated events (they may arrive out of order):
        e1 = scheduled_triggers_evts_webhook.get_event(12) # at least 10 sec, see processScheduledTriggers.sleep
        e2 = scheduled_triggers_evts_webhook.get_event(12)
        e3 = scheduled_triggers_evts_webhook.get_event(12)
        [event_fail1, event_fail2, event_success] = sorted([e1,e2,e3], key=lambda e: e['path'])
        # Check the two failures:
        validate_event_webhook(event_fail1['path'],'/fail')
        validate_event_webhook(event_fail2['path'],'/fail')
        # Check the one successful webhook call:
        query = {
            "type":"run_sql",
            "args":{
                "sql":'''
                select timezone('utc',created_at) as created_at
                from hdb_catalog.hdb_scheduled_events
                where comment = 'test scheduled event';
                '''
            }
        }
        st, resp = hge_ctx.v1q(query)
        assert st == 200, resp
        db_created_at = resp['result'][1][0]
        validate_event_webhook(event_success['path'],'/test')
        validate_event_headers(event_success['headers'],{"header-key":"header-value"})
        assert event_success['body']['payload'] == self.webhook_payload
        assert event_success['body']['created_at'] == db_created_at.replace(" ","T") + "Z"
        payload_keys = dict.keys(event_success['body'])
        for k in ["scheduled_time","created_at","id"]: # additional keys
            assert k in payload_keys
        assert scheduled_triggers_evts_webhook.is_queue_empty()

    def test_check_events_statuses(self,hge_ctx):
        query = {
            "type":"run_sql",
            "args":{
                "sql":"select status,tries from hdb_catalog.hdb_scheduled_events"
            }
        }
        st, resp = hge_ctx.v1q(query)
        assert st == 200, resp
        scheduled_event_statuses = dict(resp['result'])
        # 3 scheduled events have been created
        # one should be dead because the timestamp was past the tolerance limit
        # one should be delivered because all the parameters were reasonable
        # one should be error because the webhook returns an error state
        assert scheduled_event_statuses == {
                 'status':    'tries',
                 'dead':      '0',
                 'delivered': '1',
                 'error':     '2' # num_retries + 1
                }

    def test_teardown_scheduled_events(self,hge_ctx):
        query = {
            "type":"run_sql",
            "args": {
                "sql":"delete from hdb_catalog.hdb_scheduled_events"
            }
        }
        st, resp = hge_ctx.v1q(query)
        assert st == 200,resp

class TestCronTrigger(object):

    cron_trigger_name = "cron_trigger"

    def test_create_cron_schedule_triggers(self,hge_ctx):
        # setting the test to be after 30 mins, to make sure that
        # any of the events are not delivered.
        min_after_30_mins = (datetime.utcnow() + timedelta(minutes=30)).minute
        TestCronTrigger.cron_schedule = "{} * * * *".format(min_after_30_mins)

        cron_st_api_query = {
            "type":"create_cron_trigger",
            "args":{
                "name":self.cron_trigger_name,
                "webhook":"{{SCHEDULED_TRIGGERS_WEBHOOK_DOMAIN}}" + "/foo",
                "schedule":self.cron_schedule,
                "headers":[
                    {
                        "name":"foo",
                        "value":"baz"
                    }
                ],
                "payload":{"foo":"baz"},
                "include_in_metadata":True
            }
        }
        cron_st_code,cron_st_resp = hge_ctx.v1q(cron_st_api_query)
        TestCronTrigger.init_time = datetime.utcnow()
        # the cron events will be generated based on the current time, they
        # will not be exactly the same though(the server now and now here)
        assert cron_st_code == 200,cron_st_resp
        assert cron_st_resp['message'] == 'success'

    def test_check_generated_cron_scheduled_events(self,hge_ctx):
        expected_schedule_timestamps = []
        iter = croniter(self.cron_schedule,self.init_time)
        for i in range(100):
            expected_schedule_timestamps.append(iter.next(datetime))
        # Get timestamps in UTC from the db to compare it with
        # the croniter generated timestamps
        sql = '''
        select timezone('utc',scheduled_time) as scheduled_time
        from hdb_catalog.hdb_cron_events where
        trigger_name = '{}' order by scheduled_time asc;'''
        q = {
            "type":"run_sql",
            "args":{
                "sql":sql.format(self.cron_trigger_name)
            }
        }
        st,resp = hge_ctx.v1q(q)
        assert st == 200,resp
        ts_resp = resp['result'][1:]
        assert len(ts_resp) == 100
        # 100 scheduled events are generated in a single batch when the
        # scheduled events need hydration
        actual_schedule_timestamps = []
        for ts in ts_resp:
            datetime_ts = datetime.strptime(ts[0],"%Y-%m-%d %H:%M:%S")
            actual_schedule_timestamps.append(datetime_ts)
        assert actual_schedule_timestamps == expected_schedule_timestamps

    def test_update_existing_cron_trigger(self,hge_ctx):
        expected_schedule_timestamps = []
        iter = croniter(self.cron_schedule,datetime.utcnow())
        for i in range(100):
            expected_schedule_timestamps.append(iter.next(datetime))
        q = {
            "type":"create_cron_trigger",
            "args":{
                "name":self.cron_trigger_name,
                "webhook":"{{SCHEDULED_TRIGGERS_WEBHOOK_DOMAIN}}" + "/foo",
                "schedule":self.cron_schedule,
                "headers":[
                    {
                        "name":"header-name",
                        "value":"header-value"
                    }
                ],
                "payload":{"foo":"baz"},
                "include_in_metadata":True,
                "replace":True
            }
        }
        st,resp = hge_ctx.v1q(q)
        assert st == 200, resp

        st, resp = hge_ctx.v1q({'type': 'export_metadata', 'args': {}})
        assert st == 200,resp

        all_cron_triggers = resp['cron_triggers']
        for cron_trigger in all_cron_triggers:
            if cron_trigger['name'] == self.cron_trigger_name:
                assert cron_trigger['headers'] == [{
                    "name":"header-name",
                    "value":"header-value"
                }]

        # Get timestamps in UTC from the db to compare it with
        # the croniter generated timestamps
        # After updating the cron trigger, the future events should
        # have been created
        sql = '''
        select timezone('utc',scheduled_time) as scheduled_time
        from hdb_catalog.hdb_cron_events where
        trigger_name = '{}' order by scheduled_time asc;'''
        q = {
            "type":"run_sql",
            "args":{
                "sql":sql.format(self.cron_trigger_name)
            }
        }
        st,resp = hge_ctx.v1q(q)
        assert st == 200,resp
        ts_resp = resp['result'][1:]
        assert len(ts_resp) == 100
        actual_schedule_timestamps = []
        for ts in ts_resp:
            datetime_ts = datetime.strptime(ts[0],"%Y-%m-%d %H:%M:%S")
            actual_schedule_timestamps.append(datetime_ts)
        assert actual_schedule_timestamps == expected_schedule_timestamps

    def test_check_fired_webhook_event(self, hge_ctx, scheduled_triggers_evts_webhook):
        q = {
            "type":"create_cron_trigger",
            "args":{
                "name":"test_cron_trigger",
                "webhook":"{{SCHEDULED_TRIGGERS_WEBHOOK_DOMAIN}}" + "/test",
                "schedule":"* * * * *",
                "headers":[
                    {
                        "name":"header-key",
                        "value":"header-value"
                    }
                ],
                "payload":{"foo":"baz"},
                "include_in_metadata":False
            }
        }
        st,resp = hge_ctx.v1q(q)
        assert st == 200, resp
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
        validate_event_webhook(event['path'],'/test')
        validate_event_headers(event['headers'],{"header-key":"header-value"})
        assert event['body']['payload'] == {"foo":"baz"}
        assert event['body']['name'] == 'test_cron_trigger'

    def test_export_and_import_cron_triggers(self, hge_ctx):
        q = {
            "type": "export_metadata",
            "args": {}
        }
        st, resp = hge_ctx.v1q(q)
        assert st == 200, resp
        respDict = json.loads(json.dumps(resp))
        # Only the cron triggers with `include_in_metadata` set to `True`
        # should be exported
        assert respDict['cron_triggers'] == [
            {
                "headers": [
                    {
                        "name": "header-name",
                        "value": "header-value"
                    }
                ],
                "include_in_metadata": True,
                "name": self.cron_trigger_name,
                "payload": {
                    "foo": "baz"
                },
                "schedule": self.cron_schedule,
                "webhook": "{{SCHEDULED_TRIGGERS_WEBHOOK_DOMAIN}}/foo"
            }
        ]
        q = {
            "type": "replace_metadata",
            "args": {
                "metadata": resp
            }
        }
        st, resp = hge_ctx.v1q(q)
        sql = '''
        select count(1) as count
        from hdb_catalog.hdb_cron_events
        where trigger_name = '{}'
        '''
        run_sql_query = {
            "type": "run_sql",
            "args": {
                "sql": sql.format(self.cron_trigger_name)
            }
        }
        st, resp = hge_ctx.v1q(run_sql_query)
        assert st == 200, resp
        count_resp = resp['result'][1][0]
        # Check if the future cron events are created for
        # for a cron trigger while imported from the metadata
        assert int(count_resp) == 100

    def test_attempt_to_create_duplicate_cron_trigger_fail(self, hge_ctx):
        q = {
            "type":"create_cron_trigger",
            "args":{
                "name":"test_cron_trigger",
                "webhook":"{{SCHEDULED_TRIGGERS_WEBHOOK_DOMAIN}}" + "/test",
                "schedule":"* * * * *",
                "headers":[
                    {
                        "name":"header-key",
                        "value":"header-value"
                    }
                ],
                "payload":{"foo":"baz"},
                "include_in_metadata":False
            }
        }
        st, resp = hge_ctx.v1q(q)
        assert st == 400, dict(resp)
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
                    "type":"delete_cron_trigger",
                    "args":{
                        "name":self.cron_trigger_name
                    }
                },
                {
                    "type":"delete_cron_trigger",
                    "args":{
                        "name":"test_cron_trigger"
                    }
                }
            ]
        }
        st,resp = hge_ctx.v1q(q)
        assert st == 200,resp
