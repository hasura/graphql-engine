import { rest } from 'msw';
import { metadata, schemaList, query } from './dataStubs';

const url = 'http://localhost:8080';

export const handlers = [
  rest.post(`${url}/v2/query`, (req, res, ctx) => {
    const body = req.body as Record<string, any>;

    const isUseSchemaList = body?.args?.sql?.includes('SELECT schema_name');

    if (isUseSchemaList) {
      return res(ctx.json(schemaList));
    }

    return res(ctx.json(query));
  }),

  rest.post(`${url}/v1/metadata`, (req, res, ctx) => {
    const body = req.body as Record<string, any>;
    if (body.type === 'export_metadata') {
      return res(ctx.json(metadata));
    }

    return res(ctx.json([{ message: 'success' }]));
  }),
];
