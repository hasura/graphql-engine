import {
  openTelemetrySchema,
  unexistingEnvVarSchema,
  hasuraEnvVarsNotAllowedSchema,
} from '.';

/**
 * Allow to parse the data the server returns and early catch possible console/server misalignments.
 *
 * Please note that this is not a strict check (https://github.com/colinhacks/zod#strict) since
 * strict on discriminated unions is not supported by Zod. Hence if the server adds more properties
 * to OpenTelemetry and the Console is not updated, chances are the Console strips out the extra
 * properties.
 *
 * More: this function does not throw in case of incompatible types. It's up to the consumer to
 * decide what to do in case of mismatch.
 */
export function parseOpenTelemetry(object: unknown) {
  return openTelemetrySchema.safeParse(object);
}

export function parseUnexistingEnvVarSchemaError(errorBody: unknown) {
  return unexistingEnvVarSchema.safeParse(errorBody);
}
export function parseHasuraEnvVarsNotAllowedError(errorBody: unknown) {
  return hasuraEnvVarsNotAllowedSchema.safeParse(errorBody);
}
