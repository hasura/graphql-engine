import { http, HttpResponse } from 'msw';
import {
  graphqlRequestResponseMap,
  metadataRequestResponseMap,
} from './networkRequests.mock';

const baseUrl = 'http://localhost:8080';

export const handlers = (url = baseUrl) => [
  http.post(`${url}/v1/metadata`, async ({ request }) => {
    const reqBody = (await request.json()) as Record<string, any>;

    return HttpResponse.json(
      metadataRequestResponseMap[JSON.stringify(reqBody)],
    );
  }),
  http.post(`${url}/v1/graphql`, async ({ request }) => {
    const reqBody = (await request.json()) as Record<string, any>;

    if (graphqlRequestResponseMap[JSON.stringify(reqBody)])
      return HttpResponse.json(
        graphqlRequestResponseMap[JSON.stringify(reqBody)],
      );

    return HttpResponse.json({});
  }),
];
