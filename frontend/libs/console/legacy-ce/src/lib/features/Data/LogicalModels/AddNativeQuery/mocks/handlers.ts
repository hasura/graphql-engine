import { rest } from 'msw';
import { MockMetadataOptions, metadata } from '../../mocks/metadata';

type TrackNativeQueryResult =
  | 'success'
  | 'already_exists'
  | 'native_queries_disabled'
  | 'validation_failed';

export const handlers = (
  metadataOptions: MockMetadataOptions,
  trackNativeQueryResult: TrackNativeQueryResult = 'success'
) => [
  rest.post('http://localhost:8080/v1/metadata', async (req, res, ctx) => {
    const requestBody = await req.json<{
      type: string;
      args: { root_field_name: string };
    }>();

    const response = (
      json: Record<string, any>,
      status: 200 | 400 | 500 = 200
    ) => res(ctx.status(status), ctx.delay(), ctx.json(json));

    if (requestBody.type === 'export_metadata') {
      return response(metadata(metadataOptions));
    }

    if (requestBody.type.endsWith('_track_native_query')) {
      switch (trackNativeQueryResult) {
        case 'success':
          return response({ message: 'success' });
        case 'already_exists':
          return response(
            {
              code: 'already-tracked',
              error: `Native query '${requestBody.args.root_field_name}' is already tracked.`,
              path: '$.args',
            },
            400
          );
        case 'validation_failed':
          return response(
            {
              code: 'validation-failed',
              error: 'Failed to validate query',
              internal: {
                arguments: [],
                error: {
                  description: null,
                  exec_status: 'FatalError',
                  hint: null,
                  message: 'relation "foo" does not exist',
                  status_code: '42P01',
                },
                prepared: false,
                statement:
                  'PREPARE _logimo_vali_ AS WITH _cte_logimo_vali_ AS (\nselect * from foo\n)\nSELECT one, two\nFROM _cte_logimo_vali_',
              },
              path: '$.args',
            },
            400
          );

        case 'native_queries_disabled':
          return response(
            {
              code: 'unexpected',
              error: 'NativeQueries is disabled!',
              path: '$.args',
            },
            500
          );
      }
    }
  }),
];
