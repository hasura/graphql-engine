import {
  keepPreviousData,
  useQuery,
  useQueryClient,
} from '@tanstack/react-query';
import {
  getDatabaseMethods,
  isSameTableObjectRelationship,
  TableColumn,
  TableFkRelationships,
  UseTableColumnsResult,
} from '@hasura/metadata/data-source';
import { defaultQueryOptions } from '@hasura/metadata/api';
import { Metadata, MetadataTable } from '@hasura/shared/types';
import { TableToLoad } from '../components';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { areTablesEqual } from '@hasura/metadata/helpers';

export type TableWithColumns = {
  metadataTable: MetadataTable;
  columns: TableColumn[];
  /** the foreign keys of a loaded table that has same-table FK relationships */
  foreignKeys: TableFkRelationships[];
  sourceName: string;
};

const USE_TABLE_WITH_COLUMNS_QUERY_KEY = 'USE_TABLE_WITH_COLUMNS';

// an object relationship on the table's own foreign key only names its
// columns, so its target table comes from the database
const hasSameTableFkRelationships = (metadataTable: MetadataTable) =>
  (metadataTable.object_relationships ?? []).some(
    isSameTableObjectRelationship,
  );

export const useTablesWithColumns = ({
  tablesToLoad,
  metadata,
}: {
  metadata: Metadata['metadata'];
  tablesToLoad: TableToLoad;
}) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const queryClient = useQueryClient();

  return useQuery<TableWithColumns[], Error>({
    queryKey: [USE_TABLE_WITH_COLUMNS_QUERY_KEY, tablesToLoad],
    queryFn: () =>
      Promise.all(
        metadata.sources.flatMap((source) =>
          source.tables.map(async (metadataTable) => {
            const isTableToLoad = tablesToLoad.some(
              (t) =>
                areTablesEqual(metadataTable.table, t.table) &&
                source.name === t?.source,
            );
            if (!isTableToLoad) {
              return {
                metadataTable,
                columns: [],
                foreignKeys: [],
                sourceName: source.name,
              };
            }

            const databaseMethods = getDatabaseMethods(source.kind);
            const props = {
              dataSourceName: source.name,
              table: metadataTable.table,
              endpoints,
              fetchJson,
            };

            const getFKRelationships =
              databaseMethods.introspection.getFKRelationships;
            // each table is cached on its own, so loading the tables of a new
            // relationship doesn't introspect the already loaded ones again
            const [{ columns }, foreignKeys] = await Promise.all([
              queryClient.fetchQuery<UseTableColumnsResult>({
                // shared with `useTableColumns`
                queryKey: [source.name, 'column-introspection', props.table],
                queryFn: async () => ({
                  columns:
                    await databaseMethods.introspection.getTableColumns(props),
                  supportedOperators:
                    databaseMethods.introspection.getSupportedOperators(),
                }),
                staleTime: defaultQueryOptions.staleTime,
              }),
              getFKRelationships && hasSameTableFkRelationships(metadataTable)
                ? queryClient
                    .fetchQuery({
                      queryKey: [source.name, 'fk-relationships', props.table],
                      queryFn: () => getFKRelationships(props),
                      staleTime: defaultQueryOptions.staleTime,
                    })
                    // without the foreign keys only these relationships are
                    // missing, so the rest of the builder still works
                    .catch(() => [])
                : [],
            ]);

            return {
              metadataTable,
              columns,
              foreignKeys,
              sourceName: source.name,
            };
          }),
        ),
      ),
    refetchOnWindowFocus: false,
    // the tables stay usable while the ones of a new relationship load
    placeholderData: keepPreviousData,
  });
};
