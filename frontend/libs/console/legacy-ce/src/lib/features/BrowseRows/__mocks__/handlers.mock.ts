import { rest } from 'msw';
import {
  graphqlRequestResponseMap,
  metadataRequestResponseMap,
} from './networkRequests.mock';

const baseUrl = 'http://localhost:8080';

export const handlers = (url = baseUrl) => [
  rest.post(`${url}/v1/metadata`, async (_req, res, ctx) => {
    const reqBody = (await _req.json()) as Record<string, any>;

    return res(ctx.json(metadataRequestResponseMap[JSON.stringify(reqBody)]));
  }),
  rest.post(`${url}/v1/graphql`, async (_req, res, ctx) => {
    const reqBody = (await _req.json()) as Record<string, any>;

    if (graphqlRequestResponseMap[JSON.stringify(reqBody)])
      return res(ctx.json(graphqlRequestResponseMap[JSON.stringify(reqBody)]));

    return res(ctx.json({}));
  }),
];
