import { useTableRelationships, DataTarget } from '@/features/Datasources';

import { QualifiedTable } from '@/metadata/types';
import { MetadataSelector } from './metadataSelectors';
import { MetadataTransformer } from './metadataTransformers';
import { useMetadata } from './useMetadata';

export const useMetadataTables = (dataSource: string) => {
  return useMetadata(MetadataSelector.getTables(dataSource));
};

export const useTables = (database: string) => {
  return useMetadata(MetadataSelector.getTables(database));
};

export const useExistingRelationships = (
  database: string,
  table: QualifiedTable
) => {
  // metadata doesn't contain sufficient info
  // therefore have to get full table relationship info
  const {
    data: tableRelationships,
    isLoading: loadingTableRelationships,
    error: tableRelationshipsError,
  } = useTableRelationships({
    target: { database, schema: table.schema, table: table.name },
  });

  const { error, isLoading: loadingMetadata, ...rest } = useMetadata(
    MetadataSelector.createGetLocalDBRelationships(database, table),
    relationships =>
      MetadataTransformer.transformTableRelationships({
        target: {
          database,
          table: table.name,
          schema: table.schema,
        },
        relationships,
        tableRelationships,
      }),
    {
      enabled: !!tableRelationships?.length,
    }
  );

  const isLoading = loadingMetadata || loadingTableRelationships;

  return { ...rest, isLoading, error: error || tableRelationshipsError };
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
