import { metadata } from './metadata';
import { mssqlStoredProceduresMockResponse } from './query';
import { extractTypeAndArgs } from '../../AddNativeQuery/mocks/native-query-handlers';
import { rest } from 'msw';

export const handlers = {
  200: [
    rest.post('http://localhost:8080/v1/metadata', async (req, res, ctx) => {
      const { type, isBulkAtomic } = await extractTypeAndArgs(req);
      if (
        (isBulkAtomic && type.endsWith('_track_logical_model')) ||
        type.endsWith('_track_stored_procedure')
      ) {
        return res(
          ctx.json({
            message: 'success',
          })
        );
      }
      if (type === 'export_metadata') {
        return res(ctx.json(metadata));
      }
    }),
    rest.post('http://localhost:8080/v2/query', async (req, res, ctx) => {
      const body = await req.json();
      if (body.type.endsWith('mssql_run_sql')) {
        return res(ctx.json(mssqlStoredProceduresMockResponse));
      }
    }),
  ],
  400: [
    rest.post('http://localhost:8080/v1/metadata', async (req, res, ctx) => {
      const { type, isBulkAtomic, args } = await extractTypeAndArgs<{
        name: string;
      }>(req);
      if (
        isBulkAtomic &&
        (type.endsWith('_track_logical_model') ||
          type.endsWith('_track_stored_procedure'))
      ) {
        return res(
          ctx.status(400),
          ctx.json({
            code: 'already-tracked',
            error: `Logical model '${args.name}' is already tracked.`,
            path: '$.args',
          })
        );
      }
      if (type === 'export_metadata') {
        return res(ctx.json(metadata));
      }
    }),
    rest.post('http://localhost:8080/v2/query', async (req, res, ctx) => {
      const body = await req.json();
      if (body.type.endsWith('mssql_run_sql')) {
        return res(ctx.json(mssqlStoredProceduresMockResponse));
      }
    }),
  ],
  500: [
    rest.post('http://localhost:8080/v1/metadata', async (req, res, ctx) => {
      const { type, isBulkAtomic } = await extractTypeAndArgs(req);

      if (isBulkAtomic && type.endsWith('_track_logical_model')) {
        return res(
          ctx.status(500),
          ctx.json({
            code: 'unexpected',
            error: 'LogicalModels is disabled!',
            path: '$.args',
          })
        );
      }
      if (type === 'export_metadata') {
        return res(ctx.json(metadata));
      }
    }),
    rest.post('http://localhost:8080/v2/query', async (req, res, ctx) => {
      const body = await req.json();
      if (body.type.endsWith('mssql_run_sql')) {
        return res(
          ctx.status(500),
          ctx.json({
            code: 'unexpected',
            error: 'SQL SERVER ERROR!',
            path: '$.args',
          })
        );
      }
    }),
  ],
};
