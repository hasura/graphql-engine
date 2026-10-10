import { metadata } from './metadata';
import { mssqlStoredProceduresMockResponse } from './query';
import { extractTypeAndArgs } from '../../AddNativeQuery/mocks/native-query-handlers';
import { http, HttpResponse } from 'msw';

export const handlers = {
  200: [
    http.post('http://localhost:8080/v1/metadata', async ({ request }) => {
      const { type, isBulkAtomic } = await extractTypeAndArgs(request);
      if (
        (isBulkAtomic && type.endsWith('_track_logical_model')) ||
        type.endsWith('_track_stored_procedure')
      ) {
        return HttpResponse.json({
          message: 'success',
        });
      }
      if (type === 'export_metadata') {
        return HttpResponse.json(metadata);
      }
    }),
    http.post('http://localhost:8080/v2/query', async ({ request }) => {
      const body = await request.json();
      if (body.type.endsWith('mssql_run_sql')) {
        return HttpResponse.json(mssqlStoredProceduresMockResponse);
      }
    }),
  ],
  400: [
    http.post('http://localhost:8080/v1/metadata', async ({ request }) => {
      const { type, isBulkAtomic, args } = await extractTypeAndArgs<{
        name: string;
      }>(request);
      if (
        isBulkAtomic &&
        (type.endsWith('_track_logical_model') ||
          type.endsWith('_track_stored_procedure'))
      ) {
        return HttpResponse.json(
          {
            code: 'already-tracked',
            error: `Logical model '${args.name}' is already tracked.`,
            path: '$.args',
          },
          { status: 400 },
        );
      }
      if (type === 'export_metadata') {
        return HttpResponse.json(metadata);
      }
    }),
    http.post('http://localhost:8080/v2/query', async ({ request }) => {
      const body = await request.json();
      if (body.type.endsWith('mssql_run_sql')) {
        return HttpResponse.json(mssqlStoredProceduresMockResponse);
      }
    }),
  ],
  500: [
    http.post('http://localhost:8080/v1/metadata', async ({ request }) => {
      const { type, isBulkAtomic } = await extractTypeAndArgs(request);

      if (isBulkAtomic && type.endsWith('_track_logical_model')) {
        return HttpResponse.json(
          {
            code: 'unexpected',
            error: 'LogicalModels is disabled!',
            path: '$.args',
          },
          { status: 500 },
        );
      }
      if (type === 'export_metadata') {
        return HttpResponse.json(metadata);
      }
    }),
    http.post('http://localhost:8080/v2/query', async ({ request }) => {
      const body = await request.json();
      if (body.type.endsWith('mssql_run_sql')) {
        return HttpResponse.json(
          {
            code: 'unexpected',
            error: 'SQL SERVER ERROR!',
            path: '$.args',
          },
          { status: 500 },
        );
      }
    }),
  ],
};
