import { http, HttpResponse } from 'msw';
import export_metadata from './export_metadata';
import { introspection } from './introspection';
import { queries } from './query';

export function handlers() {
  return [
    http.post('http://localhost:8080/v1/graphql', async () => {
      return HttpResponse.json(introspection);
    }),
    http.post('http://localhost:8080/v2/query', async ({ request }) => {
      const body = await request.json();
      // If body.type matches a payload in the queries array, return that payload
      if (body.type === 'run_sql') {
        const query = queries.find(
          (q) =>
            q.payload.type === body.type &&
            // Trim whitespace and compare sql
            q.payload.args.sql.trim() === body.args.sql.trim(),
        );
        if (query) {
          return HttpResponse.json(query.response);
        }
      }
      return HttpResponse.json({});
    }),
    http.post('http://localhost:8080/v1/metadata', async ({ request }) => {
      const body = await request.json();
      if (body.type === 'export_metadata') {
        return HttpResponse.json(export_metadata);
      }
      return HttpResponse.json({});
    }),
  ];
}
