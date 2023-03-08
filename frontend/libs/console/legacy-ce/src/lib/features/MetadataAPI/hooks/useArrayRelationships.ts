import { DataTarget } from '../../Datasources';
import { MetadataSelector } from './metadataSelectors';
import { useMetadata } from './useMetadata';

export const useArrayRelationships = (target: DataTarget) => {
  const { data, ...rest } = useMetadata(
    MetadataSelector.getLocalDBArrayRelationships(target.database, {
      name: target.table,
      schema: 'schema' in target ? target.schema : '',
      ...('dataset' in target && { dataset: target.dataset }),
    })
  );
  return { data, ...rest };
};
