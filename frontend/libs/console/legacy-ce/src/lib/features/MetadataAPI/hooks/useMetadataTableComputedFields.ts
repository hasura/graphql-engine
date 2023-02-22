import { QualifiedTable } from '../../../metadata/types';
import { MetadataSelector } from './metadataSelectors';
import { useMetadata } from './useMetadata';

export const useMetadataTableComputedFields = (
  table: QualifiedTable,
  dataSource: string
) => {
  return useMetadata(
    MetadataSelector.getTableComputedFields(dataSource, table)
  );
};
