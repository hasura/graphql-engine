import { getAllTableRelationships } from '../../../../../DatabaseRelationships/utils/tableRelationships';
import { useTablesWithColumns } from './useTablesWithColumns';
import { TableToLoad } from '../components';
import { SuggestedRelationshipWithName } from '../../../../../DatabaseRelationships/components/SuggestedRelationships/hooks/useSuggestedRelationships';
import { TableFkRelationships } from '@hasura/metadata/data-source';
import { Metadata, Source, Table } from '@hasura/shared/types';
import { useState } from 'react';

/**
 * The relationships are read from the metadata; only the target of an object
 * relationship on the table's own foreign key comes from that foreign key.
 */
const toForeignKeyConstraints = (
  foreignKeys: TableFkRelationships[],
): SuggestedRelationshipWithName[] =>
  foreignKeys.map((foreignKey) => ({
    type: 'object',
    from: {
      table: foreignKey.from.table,
      columns: foreignKey.from.columns.map((column) => column.trim()),
    },
    to: {
      table: foreignKey.to.table,
      columns: foreignKey.to.columns.map((column) => column.trim()),
    },
    constraintName: foreignKey.name ?? '',
  }));

export const usePermissionTables = ({
  source,
  metadata,
  table,
}: {
  source: Source;
  table: Table;
  metadata: Metadata['metadata'];
}) => {
  const [tablesToLoad, setTablesToLoad] = useState<TableToLoad>([
    { table, source: source.name },
  ]);

  const { data: tables, isLoading: isLoadingTables } = useTablesWithColumns({
    tablesToLoad,
    metadata,
  });

  if (isLoadingTables)
    return { isLoading: true, tablesToLoad, setTablesToLoad, tables: [] };

  return {
    isLoading: false,
    tablesToLoad,
    setTablesToLoad,
    tables:
      tables?.map(({ metadataTable, columns, foreignKeys, sourceName }) => {
        return {
          table: metadataTable.table,
          dataSource: metadata.sources?.find(
            (source) => source.name === sourceName,
          ),
          relationships: getAllTableRelationships(
            metadataTable,
            sourceName,
            toForeignKeyConstraints(foreignKeys),
          ),
          columns,
          computedFields: metadataTable.computed_fields ?? [],
        };
      }) ?? [],
  };
};
