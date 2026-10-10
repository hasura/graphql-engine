import { ActionRequestTransform, WebhookURL } from './actions';
import { HeaderConfig } from './header';
import { HeaderFromValue } from './metadata/v2';

export type CronTriggerName = string;

type CronTriggerRequestTransform = ActionRequestTransform;

export interface CronTrigger {
  /**	Name of the cron trigger */
  name: CronTriggerName;
  /**	URL of the webhook */
  webhook: WebhookURL;
  /**	Cron expression at which the trigger should be invoked. */
  schedule: string;
  /** Any JSON payload which will be sent when the webhook is invoked. */
  payload?: Record<string, any>;
  /** List of headers to be sent with the webhook */
  headers: HeaderConfig[];
  /**	Retry configuration if scheduled invocation delivery fails */
  retry_conf?: RetryConfST;
  /**	Flag to indicate whether a trigger should be included in the metadata. When a cron trigger is included in the metadata, the user will be able to export it when the metadata of the graphql-engine is exported. */
  include_in_metadata: boolean;
  /**	Custom comment. */
  comment?: string;
  /** Rest connectors. */
  request_transform?: CronTriggerRequestTransform;
}

/**
 * Args for the one-off `create_scheduled_event` metadata API. It shares every
 * field with a cron trigger except the schedule: instead of a recurring cron
 * expression it takes `schedule_at`, an ISO timestamp for a single delivery.
 */
export type ScheduledEventCreateArgs = Omit<CronTrigger, 'schedule'> & {
  /** ISO timestamp at which the one-off scheduled event should run. */
  schedule_at: string;
};

export interface RetryConfST {
  /**
   * Number of times to retry delivery.
   * Default: 0
   * @TJS-type integer
   */
  num_retries?: number;
  /**
   * Number of seconds to wait between each retry.
   * Default: 10
   * @TJS-type integer
   */
  retry_interval_seconds?: number;
  /**
   * Number of seconds to wait for response before timing out.
   * Default: 60
   * @TJS-type integer
   */
  timeout_seconds?: number;
  /**
   * Number of seconds between scheduled time and actual delivery time that is acceptable. If the time difference is more than this, then the event is dropped.
   * Default: 21600 (6 hours)
   * @TJS-type integer
   */
  tolerance_seconds?: number;
}

export type CronTriggerType = 'one_off' | 'cron';
export type CronTriggerStatus = 'scheduled' | 'delivered' | 'dead' | 'error';

export type ScheduledEvent = {
  created_at: string;
  id: string;
  next_retry_at: string | null;
  scheduled_time: string;
  status: CronTriggerStatus;
  tries: number;
  trigger_name: string;
};

export type ScheduledEventInvocation<P = unknown> = {
  created_at: string;
  event_id: string;
  id: string;
  request: {
    headers: HeaderFromValue[];
    payload: {
      comment: string;
      id: string;
      name: string;
      payload: P;
      scheduled_time: string;
    };
    version: '1' | '2';
  };
  response: {
    data: {
      message: string;
    };
    type: 'webhook_response' | 'client_error';
    version: '1' | '2';
  };
  status: number;
};
