import { Capabilities } from '@hasura/dc-api-types';

export const supportsSchemaLessTables = (
  capabilities: Capabilities | undefined,
) => {
  return capabilities?.data_schema?.supports_schemaless_tables ?? false;
};
