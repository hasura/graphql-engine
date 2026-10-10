import { useQuery } from '@tanstack/react-query';
import { getDatabaseMethods, TableColumn } from '@hasura/metadata/data-source';
import { Metadata, MetadataTable } from '@hasura/shared/types';
import { TableToLoad } from '../components';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { areTablesEqual } from '@hasura/metadata/helpers';

export type TableWithColumns = {
  metadataTable: MetadataTable;
  columns: TableColumn[];
  sourceName: string;
};

const USE_TABLE_WITH_COLUMNS_QUERY_KEY = 'USE_TABLE_WITH_COLUMNS';

export const useTablesWithColumns = ({
  tablesToLoad,
  metadata,
}: {
  metadata: Metadata['metadata'];
  tablesToLoad: TableToLoad;
}) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return useQuery<TableWithColumns[], Error>({
    queryKey: [USE_TABLE_WITH_COLUMNS_QUERY_KEY, tablesToLoad],
    queryFn: async () => {
      const result: TableWithColumns[] = [];

      for (const source of metadata.sources) {
        for (const metadataTable of source.tables) {
          if (
            tablesToLoad.find(
              (t) =>
                areTablesEqual(metadataTable.table, t.table) &&
                source.name === t?.source,
            )
          ) {
            const databaseMethods = getDatabaseMethods(source.kind);

            const columns = await databaseMethods.introspection.getTableColumns(
              {
                dataSourceName: source.name,
                table: metadataTable.table,
                endpoints,
                fetchJson,
              },
            );

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
