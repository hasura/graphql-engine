import { rest } from 'msw';
import { results } from './data';

const baseUrl = 'http://localhost:8080';

export const handlers = (url = baseUrl) => [
  rest.post(`${url}/v1/graphql`, (req, res, ctx) => {
    return res(ctx.json(results));
  }),
];
