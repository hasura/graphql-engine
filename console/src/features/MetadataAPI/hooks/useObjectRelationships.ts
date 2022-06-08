import { DataTarget } from '@/features/Datasources';
import { MetadataSelector } from './metadataSelectors';
import { useMetadata } from './useMetadata';

export const useObjectRelationships = (target: DataTarget) => {
  const { data, ...rest } = useMetadata(
    MetadataSelector.getLocalDBObjectRelationships(target.database, {
      name: target.table,
      schema: 'schema' in target ? target.schema : '',
    })
  );
  return { data, ...rest };
};
