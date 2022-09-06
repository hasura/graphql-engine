import { rest } from 'msw';
import { TMigration } from '../../../../MetadataAPI/hooks/useMetadataMigration';

const baseUrl = 'http://localhost:8080';

export const handlers = (delay = 0, url = baseUrl) => [
  // todo export metadata mock based on the input

  rest.post(`${url}/v1/metadata`, (req, res, ctx) => {
    const body = req.body as TMigration['query'];

    if (
      body.type !== 'drop_collection_from_allowlist' ||
      body.args.collection !== 'allowed_queries'
    ) {
      return res(
        ctx.delay(delay),
        ctx.status(500),
        ctx.json({
          message: 'error',
        })
      );
    }

    return res(
      ctx.delay(delay),
      ctx.json({
        message: 'success',
      })
    );
  }),
];
