import { rest } from 'msw';
import {
  mockInconsistentMetadata,
  mockListSourceKindsResponse,
  mockMetadata,
} from './mockData';

const baseUrl = 'http://localhost:8080';

export const handlers = (url = baseUrl) => [
  rest.post(`${url}/v1/metadata`, async (_req, res, ctx) => {
    const reqBody = (await _req.json()) as Record<string, any>;

    if (reqBody.type === 'list_source_kinds')
      return res(ctx.json(mockListSourceKindsResponse));
    if (reqBody.type === 'export_metadata') return res(ctx.json(mockMetadata));
    if (reqBody.type === 'get_inconsistent_metadata')
      return res(ctx.json(mockInconsistentMetadata));
  }),
];
