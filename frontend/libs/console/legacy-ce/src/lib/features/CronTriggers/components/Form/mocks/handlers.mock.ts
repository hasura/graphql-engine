import { rest } from 'msw';
import Endpoints from '../../../../../Endpoints';
import { listCronTriggerAPIResponse } from './CronTriggerListAPIResponse';
import { CronRequestBody, CronResponseBody } from './types';

export const handlers = () => [
  rest.post<CronRequestBody, CronResponseBody>(
    Endpoints.metadata,
    (req, res, ctx) => {
      const body = req.body;

      // TODO: here we could do checks to verify validity of requests in future as our api's mature,
      // currently, in most cases server accepts anything, and it could be tech dept for us to maintain such checks
      if (
        body.type === 'bulk' ||
        body.type === 'concurrent_bulk' ||
        body.type === 'create_cron_trigger' ||
        body.type === 'delete_cron_trigger' ||
        body.type === 'test_webhook_transform'
      ) {
        return res(ctx.json({ message: 'success' } as CronResponseBody));
      }

      if (body.type === 'get_cron_triggers') {
        return res(ctx.json(listCronTriggerAPIResponse as CronResponseBody));
      }

      return res(
        ctx.status(400),
        ctx.json({
          code: 'parse-failed',
          error: `unknown metadata command ${body.type}`,
          path: '$',
        } as CronResponseBody)
      );
    }
  ),
];
