import { getAllTableRelationships } from '../../../../../DatabaseRelationships/utils/tableRelationships';
import { useTablesWithColumns } from './useTablesWithColumns';
import { TableToLoad } from '../components';
import { useAllSuggestedRelationships } from '../../../../../DatabaseRelationships/components/SuggestedRelationships/hooks/useAllSuggestedRelationships';
import { Metadata, Source, Table } from '@hasura/shared/types';
import { useState } from 'react';

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

  const { suggestedRelationships, isLoadingSuggestedRelationships } =
    useAllSuggestedRelationships({
      source,
      isEnabled: true,
      omitTracked: false,
    });

  if (isLoadingTables || isLoadingSuggestedRelationships)
    return { isLoading: true, tablesToLoad, setTablesToLoad, tables: [] };

  return {
    isLoading: false,
    tablesToLoad,
    setTablesToLoad,
    tables:
      tables?.map(({ metadataTable, columns, sourceName }) => {
        return {
          table: metadataTable.table,
          dataSource: metadata.sources?.find(
            (source) => source.name === sourceName,
          ),
          relationships: getAllTableRelationships(
            metadataTable,
            source.name,
            suggestedRelationships,
          ),
          columns,
          computedFields: metadataTable.computed_fields ?? [],
        };
      }) ?? [],
  };
};
