import { DataSource } from '../../DataSource';
import { DataTarget } from '../../Datasources';
import { Table } from '../../hasura-metadata-types';
import { useHttpClient } from '../../Network';

import type { QualifiedTable } from '../../../metadata/types';
import { useQuery } from 'react-query';
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

export const useSupportedQueryTypes = ({
  dataSourceName,
  table,
}: {
  table: Table;
  dataSourceName: string;
}) => {
  const httpClient = useHttpClient();

  return useQuery({
    queryKey: ['supported-query-types'],
    queryFn: async () => {
      return DataSource(httpClient).getSupportedQueryTypes({
        dataSourceName,
        table,
      });
    },
  });
};

export const useRemoteSchemaRelationships = (
  database: string,
  table: QualifiedTable
) => {
  return useMetadata(
    MetadataSelector.getRemoteSchemaRelationships(database, table)
  );
};
