import { rest } from 'msw';
import export_metadata from './export_metadata';
import { introspection } from './introspection';
import { queries } from './query';

export function handlers() {
  return [
    rest.post('http://localhost:8080/v1/graphql', async (req, res, ctx) => {
      return res(ctx.json(introspection));
    }),
    rest.post('http://localhost:8080/v2/query', async (req, res, ctx) => {
      const body = await req.json();
      // If body.type matches a payload in the queries array, return that payload
      if (body.type === 'run_sql') {
        const query = queries.find(
          q =>
            q.payload.type === body.type &&
            // Trim whitespace and compare sql
            q.payload.args.sql.trim() === body.args.sql.trim()
        );
        if (query) {
          return res(ctx.json(query.response));
        }
      }
      return res(ctx.json({}));
    }),
    rest.post('http://localhost:8080/v1/metadata', async (req, res, ctx) => {
      const body = await req.json();
      if (body.type === 'export_metadata') {
        return res(ctx.json(export_metadata));
      }
      return res(ctx.json({}));
    }),
  ];
}
