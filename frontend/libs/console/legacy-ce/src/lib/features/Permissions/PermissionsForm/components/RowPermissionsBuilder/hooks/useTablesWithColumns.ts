import { useQuery } from 'react-query';
import {
  DataSource,
  exportMetadata,
  TableColumn,
} from '../../../../../DataSource';
import { MetadataTable } from '../../../../../hasura-metadata-types';
import { useHttpClient } from '../../../../../Network';
import { areTablesEqual } from '../../../../../hasura-metadata-api';
import { TableToLoad } from '../components';

export type TableWithColumns = {
  metadataTable: MetadataTable;
  columns: TableColumn[];
  sourceName: string;
};

export const useTablesWithColumns = ({
  tablesToLoad,
}: {
  tablesToLoad: TableToLoad;
}) => {
  const httpClient = useHttpClient();
  return useQuery<TableWithColumns[], Error>({
    queryKey: [tablesToLoad],
    queryFn: async () => {
      const { metadata } = await exportMetadata({
        httpClient,
      });

      if (!metadata) throw Error('metadata not found');

      const result: TableWithColumns[] = [];

      for (const source of metadata.sources) {
        for (const metadataTable of source.tables) {
          if (
            tablesToLoad.find(
              t =>
                areTablesEqual(metadataTable.table, t.table) &&
                source.name === t?.source
            )
          ) {
            const columns = await DataSource(httpClient).getTableColumns({
              dataSourceName: source.name,
              table: metadataTable.table,
            });
            result.push({ metadataTable, columns, sourceName: source.name });
          } else {
            result.push({
              metadataTable,
              columns: [],
              sourceName: source.name,
            });
          }
        }
      }

      return result;
    },
    refetchOnWindowFocus: false,
  });
};
