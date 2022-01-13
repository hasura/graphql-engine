import { QualifiedTable } from '@/metadata/types';
import { useAppSelector } from '@/store';
import { MetadataSelector } from './metadataSelectors';
import { useMetadata } from './useMetadata';

export const useMetadataTablePermissions = (table: QualifiedTable) => {
  const source: string = useAppSelector(
    state => state.tables.currentDataSource
  );
  return useMetadata(MetadataSelector.getTablePermissions(source, table));
};
