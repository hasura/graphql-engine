import { http, HttpResponse } from 'msw';
import { listCronTriggerAPIResponse } from './CronTriggerListAPIResponse';
import { CronRequestBody, CronResponseBody } from './types';
import endpoints from '../../../../../../Endpoints';

export const handlers = () => [
  http.post(endpoints.metadata, async ({ request }) => {
    const body = (await request.json()) as CronRequestBody;

    // TODO: here we could do checks to verify validity of requests in future as our api's mature,
    // currently, in most cases server accepts anything, and it could be tech dept for us to maintain such checks
    if (
      body.type === 'bulk' ||
      body.type === 'concurrent_bulk' ||
      body.type === 'create_cron_trigger' ||
      body.type === 'delete_cron_trigger' ||
      body.type === 'test_webhook_transform'
    ) {
      return HttpResponse.json({ message: 'success' } as CronResponseBody);
    }

    if (body.type === 'get_cron_triggers') {
      return HttpResponse.json(listCronTriggerAPIResponse as CronResponseBody);
    }

    return HttpResponse.json(
      {
        code: 'parse-failed',
        error: `unknown metadata command ${body.type}`,
        path: '$',
      } as CronResponseBody,
      { status: 400 },
    );
  }),
];
