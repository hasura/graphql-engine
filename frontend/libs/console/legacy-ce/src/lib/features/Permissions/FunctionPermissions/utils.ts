import { Metadata } from '../../hasura-metadata-types';
import { MetadataSelectors } from '../../hasura-metadata-api';

export const getMetadataDataSource =
  (dataSourceName: string) => (m: Metadata) => {
    return MetadataSelectors.findSource(dataSourceName)(m);
  };
