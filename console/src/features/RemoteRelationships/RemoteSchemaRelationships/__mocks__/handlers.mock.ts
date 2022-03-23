import { rest } from 'msw';

import { schema } from './schema';
import { countries } from './countries_schema';
import { metadata } from './metadata';

const baseUrl = 'http://localhost:8080';

export const handlers = (url = baseUrl) => [
  rest.post(`${url}/v1/metadata`, (req, res, ctx) => {
    const body = req.body as Record<string, any>;

    if (
      body.type === 'introspect_remote_schema' &&
      body?.args?.name === 'remoteSchema1'
    ) {
      return res(ctx.json(schema));
    }

    if (
      body.type === 'introspect_remote_schema' &&
      body?.args?.name === 'remoteSchema2'
    ) {
      return res(ctx.json(countries));
    }

    if (body.type === 'export_metadata') {
      return res(ctx.json(metadata));
    }

    return res(ctx.json([{ message: 'success' }]));
  }),
];
