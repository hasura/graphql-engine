/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { OpenApiSchema } from './OpenApiSchema';

export type ConfigSchemaResponse = {
  config_schema: OpenApiSchema;
  other_schemas: Record<string, OpenApiSchema>;
};

