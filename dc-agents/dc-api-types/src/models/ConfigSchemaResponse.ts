/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { OpenApiSchema } from './OpenApiSchema';

export type ConfigSchemaResponse = {
  configSchema: OpenApiSchema;
  otherSchemas: Record<string, OpenApiSchema>;
};

