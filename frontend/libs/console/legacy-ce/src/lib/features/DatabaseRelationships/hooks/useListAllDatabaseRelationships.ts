import { DataSource } from '../../DataSource';
import { Table } from '../../hasura-metadata-types';
import { useHttpClient } from '../../Network';
import { useQuery } from 'react-query';
import { useMetadata, MetadataSelectors } from '../../hasura-metadata-api';
import {
  DEFAULT_STALE_TIME,
  generateQueryKeys,
} from '../utils/queryClientUtils';
import { tableRelationships } from '../utils/tableRelationships';

const useFkConstraints = ({
  dataSourceName,
  table,
}: {
  dataSourceName: string;
  table: Table;
}) => {
  const httpClient = useHttpClient();

  return useQuery({
    queryKey: generateQueryKeys.fkConstraints({ table, dataSourceName }),
    queryFn: async () => {
      const result = await DataSource(httpClient).getTableFkRelationships({
        dataSourceName,
        table,
      });
      return result;
    },
    refetchOnWindowFocus: false,
    staleTime: DEFAULT_STALE_TIME,
  });
};

export const useListAllDatabaseRelationships = ({
  dataSourceName,
  table,
}: {
  dataSourceName: string;
  table: Table;
}) => {
  const {
    data: metadataTable,
    isFetching: isMetadataPending,
    isLoading: isMetadataLoading,
    error: metadataError,
  } = useMetadata(MetadataSelectors.findTable(dataSourceName, table));

  const {
    data: fkConstraints,
    isFetching: isDALIntrospectionPending,
    isLoading: isDALIntrospectionLoading,
    error: dalError,
  } = useFkConstraints({ dataSourceName, table });

  return {
    data: tableRelationships(
      metadataTable,
      table,
      dataSourceName,
      fkConstraints
    ),
    isFetching: isMetadataPending || isDALIntrospectionPending,
    isLoading: isMetadataLoading || isDALIntrospectionLoading,
    error: [metadataError, dalError],
  };
};
