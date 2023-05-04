import { rest } from 'msw';
import { metadata } from './metadata';

export const handlers = {
  200: [
    rest.post('http://localhost:8080/v1/metadata', async (req, res, ctx) => {
      const body = await req.json();
      if (body.type.endsWith('_track_logical_model')) {
        return res(
          ctx.json({
            message: 'success',
          })
        );
      }
      if (body.type === 'export_metadata') {
        return res(ctx.json(metadata));
      }
    }),
  ],
  400: [
    rest.post('http://localhost:8080/v1/metadata', async (req, res, ctx) => {
      const body = await req.json();
      if (body.type.endsWith('_track_logical_model')) {
        return res(
          ctx.status(400),
          ctx.json({
            code: 'already-tracked',
            error: `Logical model '${body.args.name}' is already tracked.`,
            path: '$.args',
          })
        );
      }
      if (body.type === 'export_metadata') {
        return res(ctx.json(metadata));
      }
    }),
  ],
  500: [
    rest.post('http://localhost:8080/v1/metadata', async (req, res, ctx) => {
      const body = await req.json();
      if (body.type.endsWith('_track_logical_model')) {
        return res(
          ctx.status(500),
          ctx.json({
            code: 'unexpected',
            error: 'LogicalModels is disabled!',
            path: '$.args',
          })
        );
      }
      if (body.type === 'export_metadata') {
        return res(ctx.json(metadata));
      }
    }),
  ],
};
