import { http, HttpResponse } from 'msw';
import { mockMetadata } from './mockData';

const baseUrl = 'http://localhost:8080';

export const handlers = (url = baseUrl) => [
  http.post(`${url}/v1/metadata`, async ({ request }) => {
    const reqBody = (await request.json()) as Record<string, any>;

    if (reqBody.type === 'export_metadata')
      return HttpResponse.json(mockMetadata);

    console.log(reqBody.type);

    return HttpResponse.json({});
  }),
];
