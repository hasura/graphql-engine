import { rest } from 'msw';
import { metadata } from './metadata';

const baseUrl = 'http://localhost:8080';

export const handlers = (url = baseUrl) => [
  rest.post(`${url}/v1/metadata`, (req, res, ctx) => {
    return res(ctx.json(metadata));
  }),
];
