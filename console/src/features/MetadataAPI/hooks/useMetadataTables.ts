import { DataTarget } from '@/features/Datasources';

import type { QualifiedTable } from '@/metadata/types';
import { MetadataSelector } from './metadataSelectors';

import { useMetadata } from './useMetadata';

export const useMetadataTables = (dataSource: string) => {
  return useMetadata(MetadataSelector.getTables(dataSource));
};

export const useTables = (database: string) => {
  return useMetadata(MetadataSelector.getTables(database));
};

export const useRemoteDatabaseRelationships = (target: DataTarget) => {
  return useMetadata(
    MetadataSelector.getRemoteDatabaseRelationships({ target })
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
