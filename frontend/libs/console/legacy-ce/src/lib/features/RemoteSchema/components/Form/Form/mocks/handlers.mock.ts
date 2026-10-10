import { http, HttpResponse } from 'msw';

export const handlers = () => [
  http.post('http://localhost:8080/v1/metadata', () => {
    return HttpResponse.json({ message: 'success' });
  }),
];
