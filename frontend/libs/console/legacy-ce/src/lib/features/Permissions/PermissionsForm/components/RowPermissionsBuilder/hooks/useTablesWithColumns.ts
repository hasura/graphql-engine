import { keepPreviousData, useQuery } from '@tanstack/react-query';
import {
  getDatabaseMethods,
  isSameTableObjectRelationship,
  TableColumn,
  TableFkRelationships,
} from '@hasura/metadata/data-source';
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
            const props = {
              dataSourceName: source.name,
              table: metadataTable.table,
              endpoints,
              fetchJson,
            };

            const getFKRelationships =
              databaseMethods.introspection.getFKRelationships;
            const [columns, foreignKeys] = await Promise.all([
              databaseMethods.introspection.getTableColumns(props),
              getFKRelationships && hasSameTableFkRelationships(metadataTable)
                ? // without the foreign keys only these relationships are
                  // missing, so the rest of the builder still works
                  getFKRelationships(props).catch(() => [])
                : [],
            ]);

            result.push({
              metadataTable,
              columns,
              foreignKeys,
              sourceName: source.name,
            });
          } else {
            result.push({
              metadataTable,
              columns: [],
              foreignKeys: [],
              sourceName: source.name,
            });
          }
        }
      }

      return result;
    },
    refetchOnWindowFocus: false,
    // the tables stay usable while the ones of a new relationship load
    placeholderData: keepPreviousData,
  });
};
