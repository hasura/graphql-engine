#!/usr/bin/env python3

import pytest
from datetime import datetime,timedelta
from croniter import croniter
from validate import validate_event_webhook,validate_event_headers
from queue import Empty
import time

# The create and delete tests should ideally go in setup and teardown YAML files,
# We can't use that here because, the payload is dynamic i.e. in case of adhoc Scheduled Triggers
# the value is the current timestamp and in case of cron Scheduled Triggers, the cron schedule is
# derived based on the current timestamp

def stringify_datetime(dt):
    return dt.strftime("%Y-%m-%dT%H:%M:%S.%fZ")

class TestScheduledTriggerAdhoc(object):

    adhoc_trigger_name = "adhoc_scheduled_trigger"
    webhook_path = "/hello"
    webhook_payload = {"foo":"baz"}
    url = "/v1/query"

    def get_events_count_of_trigger(self,hge_ctx,trigger_name):
        events_count_sql = '''
        select count(*) from hdb_catalog.hdb_scheduled_events where name = '{}'
        '''.format(trigger_name)
        q = {
            "type":"run_sql",
            "args":{
                "sql":events_count_sql
            }
        }
        return hge_ctx.v1q(q)

    def test_create_adhoc_scheduled_trigger(self,hge_ctx,scheduled_triggers_evts_webhook):
        current_time = datetime.utcnow()
        current_time_str =  stringify_datetime(current_time)
        adhoc_st_api_query = {
            "type":"create_scheduled_trigger",
            "args":{
                "name":self.adhoc_trigger_name,
                "webhook":"http://127.0.0.1:5594" + self.webhook_path,
                "schedule":{
                    "type":"adhoc",
                    "value":current_time_str
                },
                "payload":self.webhook_payload,
                "headers":[
                    {
                        "name":"header-1",
                        "value":"header-1-value"
                    }
                ]
            }
        }
        adhoc_st_code,adhoc_st_resp = hge_ctx.v1q(adhoc_st_api_query)
        assert adhoc_st_resp['message'] == 'success'
        assert adhoc_st_code == 200

    def test_check_adhoc_generated_events_count(self,hge_ctx,scheduled_triggers_evts_webhook):
        adhoc_event_st,adhoc_event_resp = self.get_events_count_of_trigger(hge_ctx,self.adhoc_trigger_name)
        assert int(adhoc_event_resp['result'][1][0]) == 1 # An adhoc Scheduled Trigger should create exactly one schedule event

    def test_create_scheduled_events_with_valid_timestamps(self,hge_ctx,scheduled_triggers_evts_webhook):
        event_timestamps = ['2020-01-01 15:44 Z',
                            '2020-01-01T15:45 Z',
                            '2020-01-01T15:46Z',
                            '2020-01-01T15:47:00+0000',
                            '2020-01-01T15:48:00-05:30']
        scheduled_event_query = {
            "type":"create_scheduled_event",
            "args": {
                "name": self.adhoc_trigger_name
            }
        }
        for timestamp in event_timestamps:
            scheduled_event_query['args']['timestamp'] = timestamp
            st,resp = hge_ctx.v1q(scheduled_event_query)
            assert st == 200,resp
            assert resp['message'] == 'success'

    def test_check_adhoc_webhook_event(self,hge_ctx,scheduled_triggers_evts_webhook):
        ev_full = scheduled_triggers_evts_webhook.get_event(60)
        validate_event_webhook(ev_full['path'],'/hello')
        validate_event_headers(ev_full['headers'],{"header-1":"header-1-value"})
        assert ev_full['body'] == self.webhook_payload
        assert scheduled_triggers_evts_webhook.is_queue_empty()

    def test_delete_adhoc_scheduled_trigger(self,hge_ctx,scheduled_triggers_evts_webhook):
        q = {
            "type":"delete_scheduled_trigger",
            "args":{
                "name":self.adhoc_trigger_name
            }
        }
        st,resp = hge_ctx.v1q(q)
        assert st == 200,resp

class TestScheduledTriggerCron(object):

    cron_trigger_name = "cron_scheduled_trigger"

    def test_create_cron_schedule_triggers(self,hge_ctx):
        # setting the test to be after 30 mins, to make sure that
        # any of the events are not delivered.
        min_after_30_mins = (datetime.utcnow() + timedelta(minutes=30)).minute
        TestScheduledTriggerCron.cron_schedule = "{} * * * *".format(min_after_30_mins)

        cron_st_api_query = {
            "type":"create_scheduled_trigger",
            "args":{
                "name":self.cron_trigger_name,
                "webhook":"http://127.0.0.1:5594" + "/foo",
                "schedule":{
                    "type":"cron",
                    "value":self.cron_schedule
                },
                "headers":[
                    {
                        "name":"foo",
                        "value":"baz"
                    }
                ],
                "payload":{"foo":"baz"}
            }
        }
        cron_st_code,cron_st_resp = hge_ctx.v1q(cron_st_api_query)
        TestScheduledTriggerCron.init_time = datetime.utcnow()
        # the cron events will be generated based on the current time, they
        # will not be exactly the same though(the server now and now here)
        assert cron_st_code == 200
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
        from hdb_catalog.hdb_scheduled_events where
        name = '{}' order by scheduled_time asc;'''
        q = {
            "type":"run_sql",
            "args":{
                "sql":sql.format(self.cron_trigger_name)
            }
        }
        st,resp = hge_ctx.v1q(q)
        assert st == 200
        ts_resp = resp['result'][1:]
        assert len(ts_resp) == 100
        # 100 scheduled events are generated in a single batch when the
        # scheduled events need hydration
        actual_schedule_timestamps = []
        for ts in ts_resp:
            datetime_ts = datetime.strptime(ts[0],"%Y-%m-%d %H:%M:%S")
            actual_schedule_timestamps.append(datetime_ts)
        assert actual_schedule_timestamps == expected_schedule_timestamps

    def test_delete_cron_scheduled_trigger(self,hge_ctx):
        q = {
            "type":"delete_scheduled_trigger",
            "args":{
                "name":self.cron_trigger_name
            }
        }
        st,resp = hge_ctx.v1q(q)
        assert st == 200,resp
