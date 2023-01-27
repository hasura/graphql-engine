import { z } from 'zod';
import type { OpenTelemetry } from './types';

export type OpenTelemetryQueries = SetOpenTelemetryQuery['type'];

// --------------------------------------------------
// SET OPENTELEMETRY CONFIG
// --------------------------------------------------

/**
 * Allow to set/update the OpenTelemetry configuration.
 */
export type SetOpenTelemetryQuery = {
  type: 'set_opentelemetry_config';

  args: OpenTelemetry;

  successResponse: { message: 'success' };

  errorResponse: SetOpenTelemetryQueryErrors;
};

// ERRORS
// Please note that this is a non-comprehensive list of errors. It does not include
// - all the server-prevented errors also prevented by the Console: ex. 'Invalid URL' and
// 'max_export_batch_size must be a positive integer'
// - passing a wrong OpenTelemetry config, prevented by the Console
// that would result in something like {"code":"unexpected","error":"cannot continue due to new inconsistent metadata","internal":[{"definition":{"headers":[],"protocol":"http/protobuf","resource_attributes":[]},"name":"open_telemetry exporter_otlp","reason":"Inconsistent object: Missing traces endpoint","type":"open_telemetry"}],"path":"$.args"}%
// - all the internal server errors
export type SetOpenTelemetryQueryErrors = NotPreventedByTheConsoleErrors;

type NotPreventedByTheConsoleErrors = {
  httpStatus: 400;
  body:
    | z.infer<typeof hasuraEnvVarsNotAllowedSchema>
    | z.infer<typeof unexistingEnvVarSchema>
    | UnknownError;
};

// Even in case of multiple HASURA_GRAPHQL_ env vars used, the server always report just one of them
// in the error.
export const hasuraEnvVarsNotAllowedSchema = z.object({
  // ATTENTION: the real server error contains more data, but it's useless to type it because it only
  // makes this schema more fragile. At the time of writing, the errors are not set in stone and can
  // change in the future.

  error:
    // the use of z.custom is not more needed when this issue will be fixed: https://github.com/colinhacks/zod/issues/419
    z.custom<`env variables starting with "HASURA_GRAPHQL_" are not allowed in value_from_env: HASURA_GRAPHQL_${EnvVarName}`>(
      val => {
        if (typeof val !== 'string') return false;

        // see: https://regex101.com/r/afZdol/1
        return /^env variables starting with \"HASURA_GRAPHQL_\" are not allowed in value_from_env: HASURA_GRAPHQL_(?<envVarName>.[A-Z_]+)$/.test(
          val
        );
      }
    ),

  // It would be nice to highlight the problematic env var in the Console's form but, at the time of
  // writing, the  RequestHeadersSelector component is not connected to the form and cannot show
  // the input field errors. Hence the `path` returned by the server is currently useless.
  // path: `$.args.exporter_otlp.headers[${Base0Index}]`, // Base0Index = number
});

// Even in case of multiple unexisting env vars, the server always report just one of them in the
// reason.
export const unexistingEnvVarSchema = z.object({
  // ATTENTION: the real server error contains more data, but it's useless to type it because it only
  // makes this schema more fragile. At the time of writing, the errors are not set in stone and can
  // change in the future.

  internal: z.array(
    z.object({
      reason:
        z.custom<`Inconsistent object: environment variable '${EnvVarName}' not set`>(
          val => {
            if (typeof val !== 'string') return false;

            // see: https://regex101.com/r/CBCkEd/1
            return /Inconsistent object: environment variable '(?<envVarName>.*?)' not set/.test(
              val
            );
          }
        ),
    })
  ),

  // It would be nice to highlight the problematic env var in the Console's form but, at the time of
  // writing, the  RequestHeadersSelector component is not connected to the form and cannot show
  // the input field errors. More, the server does not get the name of the problematic var so only
  // parsing the error message is possible (but it's better off to avoid it).
});

type UnknownError = unknown;

type EnvVarName = string;

/^env variables starting with \\"HASURA_GRAPHQL_\\" are not allowed in value_from_env: HASURA_GRAPHQL_(?<envVarName>.[A-Z_]+)$/.test(
  'env variables starting with "HASURA_GRAPHQL_" are not allowed in value_from_env: HASURA_GRAPHQL_ENABLED_APIS'
);
