import { rest } from 'msw';
import { MockMetadataOptions, buildMetadata } from '../../mocks/metadata';

type ResultBase = 'success' | 'native_queries_disabled';

type TrackNativeQueryResult =
  | ResultBase
  | 'already_exists'
  | 'validation_failed';

type UntrackNativeQueryResult = ResultBase | 'not_found';

type TrackLogicalModelResult = ResultBase | 'already_tracked';
type UnTrackLogicalModelResult = ResultBase | 'not_found';

type HandlersOptions = {
  metadataOptions: MockMetadataOptions;
  trackNativeQueryResult?: TrackNativeQueryResult;
  untrackNativeQueryResult?: UntrackNativeQueryResult;
  trackLogicalModelResult?: TrackLogicalModelResult;
  untrackLogicalModelResult?: UnTrackLogicalModelResult;
};

export const nativeQueryHandlers = ({
  metadataOptions,
  trackNativeQueryResult = 'success',
  untrackNativeQueryResult = 'success',
  trackLogicalModelResult = 'success',
  untrackLogicalModelResult = 'success',
}: HandlersOptions) => [
  rest.post('http://localhost:8080/v1/metadata', async (req, res, ctx) => {
    const reqBody = await req.json<{
      type: string;
      args: { root_field_name?: string; name?: string; source?: string };
    }>();

    const response = (
      json: Record<string, any>,
      status: 200 | 400 | 500 = 200
    ) => res(ctx.status(status), ctx.delay(), ctx.json(json));

    if (reqBody.type === 'export_metadata') {
      return response(buildMetadata(metadataOptions));
    }

    if (reqBody.type.endsWith('_track_native_query')) {
      switch (trackNativeQueryResult) {
        case 'success':
          return response({ message: 'success' });
        case 'already_exists':
          return response(
            {
              code: 'already-tracked',
              error: `Native query '${reqBody.args.root_field_name}' is already tracked.`,
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
    if (reqBody.type.endsWith('_untrack_native_query')) {
      switch (untrackNativeQueryResult) {
        case 'success':
          return response({ message: 'success' });
        case 'not_found':
          return response(
            {
              code: 'not-found',
              error: `Native query "${reqBody.args.root_field_name}" not found in source "${reqBody.args.source}".`,
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

    if (reqBody.type.endsWith('_track_logical_model')) {
      switch (trackLogicalModelResult) {
        case 'success':
          return response({ message: 'success' });
        case 'already_tracked':
          return response(
            {
              code: 'already-tracked',
              error: `Logical model '${reqBody.args.name}' is already tracked.`,
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
    if (reqBody.type.endsWith('_untrack_logical_model')) {
      switch (untrackLogicalModelResult) {
        case 'success':
          return response({ message: 'success' });
        case 'not_found':
          return response(
            {
              code: 'not-found',
              error: `Logical model "${reqBody.args.name}" not found in source "${reqBody.args.source}".`,
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
