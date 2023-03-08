import { rest } from 'msw';
import { introspection, mockMetadata, tableInfo } from './data';

export const handlers = () => [
  rest.post(`http://localhost:8080/v1/metadata`, async (req, res, ctx) => {
    const body = (await req.json()) as Record<string, any>;

    switch (body.type) {
      case 'export_metadata':
        return res(ctx.json(mockMetadata));
      case 'get_table_info':
        return res(ctx.json(tableInfo));
      default:
        return res(ctx.json({}));
    }
  }),
  rest.post(`http://localhost:8080/v1/graphql`, async (req, res, ctx) => {
    return res(ctx.json(introspection));
  }),
];
