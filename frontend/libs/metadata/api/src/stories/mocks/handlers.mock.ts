import { http, HttpResponse } from 'msw';
import { Metadata, MetadataTable } from '@hasura/shared/types';

const baseUrl = 'http://localhost:8080';

export const metadata: Metadata = {
  resource_version: 1,
  metadata: {
    version: 3,
    sources: [
      {
        name: 'sqlite_test',
        kind: 'sqlite',
        tables: [
          {
            table: ['Album'],
          },
          {
            table: ['Artist'],
          },
        ] as MetadataTable[],
        configuration: {
          some_value: true,
        },
      },
    ],
  },
};

export const handlers = (url = baseUrl) => [
  http.post(`${url}/v2/query`, () => {
    return HttpResponse.json({});
  }),

  http.post(`${url}/v1/metadata`, () => {
    return HttpResponse.json(metadata);
  }),
];
