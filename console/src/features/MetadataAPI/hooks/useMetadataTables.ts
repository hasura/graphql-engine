import { QualifiedTable } from '@/metadata/types';
import { MetadataSelector } from './metadataSelectors';
import { useMetadata } from './useMetadata';

export const useMetadataTables = (dataSource: string) => {
  return useMetadata(MetadataSelector.getTables(dataSource));
};

export const useTables = (database: string) => {
  return useMetadata(MetadataSelector.getTables(database));
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
