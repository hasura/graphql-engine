import { Capabilities } from '@hasura/dc-api-types';
import { Feature } from '../../../DataSource';

export const supportsSchemaLessTables = (
  capabilities: Capabilities | Feature | undefined
) => {
  if (capabilities === Feature.NotImplemented) {
    return false;
  }
  return capabilities?.data_schema?.supports_schemaless_tables ?? false;
};
