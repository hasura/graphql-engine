import { DataTarget } from '@/features/Datasources';
import { MetadataSelector } from './metadataSelectors';
import { useMetadata } from './useMetadata';

export const useArrayRelationships = (target: DataTarget) => {
  const { data, ...rest } = useMetadata(
    MetadataSelector.getLocalDBArrayRelationships(target.database, {
      name: target.table,
      schema: 'schema' in target ? target.schema : '',
    })
  );
  return { data, ...rest };
};
