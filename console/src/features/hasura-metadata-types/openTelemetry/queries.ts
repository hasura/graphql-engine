import type { OpenTelemetry } from './types';

export type OpenTelemetryQueries = SetOpenTelemetryConfigQuery['type'];

// --------------------------------------------------
// SET OPENTELEMETRY CONFIG
// --------------------------------------------------

/**
 * Allow to set/update the OpenTelemetry configuration.
 */
export type SetOpenTelemetryConfigQuery = {
  type: 'set_opentelemetry_config';

  args: OpenTelemetry;

  successResponse: { message: 'success' };

  errorResponse: SetOpenTelemetryConfigQueryErrors;
};

// ERRORS
// Please note that this is a non-comprehensive list of errors. It does not include
// - all the server-prevented errors also prevented by the Console: ex. 'Invalid URL' and
// 'max_export_batch_size must be a positive integer'
// - passing a wrong OpenTelemetry config, prevented by the Console
// that would result in something like {"code":"unexpected","error":"cannot continue due to new inconsistent metadata","internal":[{"definition":{"headers":[],"protocol":"http/protobuf","resource_attributes":[]},"name":"open_telemetry exporter_otlp","reason":"Inconsistent object: Missing traces endpoint","type":"open_telemetry"}],"path":"$.args"}%
// - all the internal server errors
// ATTENTION: the `unknown` type is useful to force the consumer to manage every kind of possible
// error object. Unfortunately, we do not have good visibility over the server errors yet, and
// treating them as a black box (apart from some particular cases) is the only thing we can do.
type SetOpenTelemetryConfigQueryErrors = ErrorsNotManagedByTheConsole | unknown;

type ErrorsNotManagedByTheConsole = {
  httpStatus: 400;
} & {
  code: 'parse-failed';
  error: `Environment variable not found: "${string}"`;
  path: '$.args';
};
