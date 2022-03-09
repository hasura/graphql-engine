import { QualifiedTable } from '@/metadata/types';
import { MetadataSelector } from './metadataSelectors';
import { useMetadata } from './useMetadata';

export const useMetadataTablePermissions = (
  table: QualifiedTable,
  dataSource: string
) => {
  return useMetadata(MetadataSelector.getTablePermissions(dataSource, table));
};
