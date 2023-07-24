import { Capabilities } from '@hasura/dc-api-types';
import { Feature } from '../../../DataSource';

export const supportsSchemaLessTables = (
  capabilities: Capabilities | Feature | undefined
) => {
  if (capabilities === Feature.NotImplemented) {
    return false;
  }
  return (
    // @ts-expect-error Remove once dc-api-types is updated
    capabilities?.data_schema?.supports_schemaless_tables ?? false
  );
};
