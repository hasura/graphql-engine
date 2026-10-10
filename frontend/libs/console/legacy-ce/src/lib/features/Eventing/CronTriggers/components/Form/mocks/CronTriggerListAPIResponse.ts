import { ListCronTriggerAPIResponse } from './types';

export const listCronTriggerAPIResponse: ListCronTriggerAPIResponse = {
  cron_triggers: [
    {
      comment: 'this trigger is incuded in metadata',
      headers: [
        {
          name: 'x-hasura-id',
          value: '12',
        },
        {
          name: 'x-hasura-env',
          value_from_env: 'MY_WEBHOOK',
        },
      ],
      include_in_metadata: true,
      name: 'cron_1',
      payload: {
        username: 'sampleuser',
      },
      retry_conf: {
        num_retries: 50,
        retry_interval_seconds: 100,
        timeout_seconds: 600,
        tolerance_seconds: 21600,
      },
      schedule: '* * * * *',
      webhook: 'http://httpbin.org/post',
    },
    {
      comment: 'this trigger is not included in metadata',
      headers: [
        {
          name: 'x-hasura-user',
          value: 'username',
        },
        {
          name: 'x-hasura-value-env',
          value: 'MY_WEBHOOK',
        },
      ],
      include_in_metadata: false,
      name: 'cron_new',
      payload: {
        userinfo: 'sampleuser',
      },
      retry_conf: {
        num_retries: 15,
        retry_interval_seconds: 15,
        timeout_seconds: 70,
        tolerance_seconds: 21600,
      },
      schedule: '*/10 * * * *',
      webhook: 'http://test.com',
      request_transform: {
        version: 2,
        template_engine: 'Kriti',
        method: 'GET',
        url: '/users/get',
        query_params: {
          userId: '12',
          userName: 'abcd',
        },
      },
    },
  ],
};
