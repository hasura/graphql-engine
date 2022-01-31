import { QualifiedTable } from '@/metadata/types';
import { useAppSelector } from '@/store';
import { MetadataSelector } from './metadataSelectors';
import { useMetadata } from './useMetadata';

export const useMetadataTables = () => {
  const source: string = useAppSelector(
    state => state.tables.currentDataSource
  );
  return useMetadata(MetadataSelector.getTables(source));
};

export const useRemoteDatabaseRelationships = (
  database: string,
  table: QualifiedTable
) => {
  return useMetadata(
    MetadataSelector.getRemoteDatabaseRelationships(database, table)
  );
};

export const useRemoteSchemaRelationships = (
  database: string,
  table: QualifiedTable
) => {
  return useMetadata(
    MetadataSelector.getRemoteSchemaRelationships(database, table)
  );
};
