import { rest } from 'msw';
import { Metadata } from '../../../hasura-metadata-types';

const baseUrl = 'http://localhost:8080';

export const metadata: Metadata = {
  resource_version: 1,
  metadata: {
    version: 3,
    sources: [
      {
        name: 'sqlite_test',
        kind: 'sqlite_test',
        tables: [
          {
            table: ['Album'],
          },
          {
            table: ['Artist'],
          },
        ],
        configuration: {
          some_value: true,
        },
      },
    ],
  },
};

export const handlers = (url = baseUrl) => [
  rest.post(`${url}/v2/query`, (req, res, ctx) => {
    return res(ctx.json({}));
  }),

  rest.post(`${url}/v1/metadata`, (req, res, ctx) => {
    return res(ctx.json(metadata));
  }),
];
