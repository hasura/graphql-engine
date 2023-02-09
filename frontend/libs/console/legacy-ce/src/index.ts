export const add = (a: number, b: number): number => a + b;
export { App } from './lib/client';

export type {
  Metadata,
  SetOpenTelemetryQuery,
  unexistingEnvVarSchema,
  hasuraEnvVarsNotAllowedSchema,
} from '@/features/hasura-metadata-types';
export type { ServerConfig } from './lib/hooks';
export type { MetadataResponse, SchemaResponse } from '@/features/MetadataAPI';
