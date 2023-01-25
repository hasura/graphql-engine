import { ActionRequestTransform, WebhookURL } from '../actions';
import { ServerHeader } from '../remoteSchemas/remoteSchemas';

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
  headers: ServerHeader[];
  /**	Retry configuration if scheduled invocation delivery fails */
  retry_conf?: RetryConfST;
  /**	Flag to indicate whether a trigger should be included in the metadata. When a cron trigger is included in the metadata, the user will be able to export it when the metadata of the graphql-engine is exported. */
  include_in_metadata: boolean;
  /**	Custom comment. */
  comment?: string;
  /** Rest connectors. */
  request_transform?: CronTriggerRequestTransform;
}

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
