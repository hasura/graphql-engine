import { rest } from 'msw';
import { TMigration } from '../../../../MetadataAPI/hooks/useMetadataMigration';

const baseUrl = 'http://localhost:8080';

export const handlers = (delay = 0, url = baseUrl) => [
  // todo export metadata mock based on the input

  rest.post(`${url}/v1/metadata`, (req, res, ctx) => {
    const body = req.body as TMigration['query'];

    if (body?.args && body?.args?.[0]?.type === 'add_query_to_collection') {
      if (
        body?.args?.[0]?.query_name === 'MyQuery33' &&
        body?.args?.[0]?.collection_name === 'testCollection'
      )
        return res(
          ctx.delay(delay),
          ctx.json({
            message: 'success',
          })
        );
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
