import { useQuery } from 'react-query';
import {
  DataSource,
  exportMetadata,
  TableColumn,
} from '../../../../../DataSource';
import { MetadataTable, Table } from '../../../../../hasura-metadata-types';
import { useHttpClient } from '../../../../../Network';
import { areTablesEqual } from '../../../../../hasura-metadata-api';

const tablesQueryKey = (dataSourceName: string) => [
  'tables-with-columns',
  dataSourceName,
];

export type TableWithColumns = {
  metadataTable: MetadataTable;
  columns: TableColumn[];
};

export const useTablesWithColumns = ({
  dataSourceName,
  tablesToLoad,
}: {
  dataSourceName: string;
  tablesToLoad: Table[];
}) => {
  const httpClient = useHttpClient();
  return useQuery<TableWithColumns[], Error>({
    queryKey: [...tablesQueryKey(dataSourceName), tablesToLoad],
    queryFn: async () => {
      const { metadata } = await exportMetadata({
        httpClient,
      });

      if (!metadata) throw Error('metadata not found');

      const currentMetadataSource = metadata.sources?.find(
        source => source.name === dataSourceName
      );

      if (!currentMetadataSource)
        throw Error(`useTables.metadataSource not found`);

      const result: TableWithColumns[] = [];

      for (const metadataTable of currentMetadataSource.tables) {
        if (tablesToLoad.find(t => areTablesEqual(metadataTable.table, t))) {
          const columns = await DataSource(httpClient).getTableColumns({
            dataSourceName,
            table: metadataTable.table,
          });
          result.push({ metadataTable, columns });
        } else {
          result.push({ metadataTable, columns: [] });
        }
      }

      return result;
    },
    refetchOnWindowFocus: false,
  });
};
