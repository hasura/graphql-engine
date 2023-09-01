import { getAllTableRelationships } from '../../../../../DatabaseRelationships/utils/tableRelationships';
import { useTablesWithColumns } from './useTablesWithColumns';
import { useSources } from '../../../../../MetadataAPI';
import { TableToLoad, Tables } from '../components';
import { useAllSuggestedRelationships } from '../../../../../DatabaseRelationships/components/SuggestedRelationships/hooks/useAllSuggestedRelationships';

export const usePermissionTables = ({
  dataSourceName,
  tablesToLoad,
}: {
  dataSourceName: string;
  tablesToLoad: TableToLoad;
}): { isLoading: boolean; tables: Tables | null } => {
  const { data: sources, isLoading: isLoadingSources } = useSources();
  const { data: tables, isLoading: isLoadingTables } = useTablesWithColumns({
    tablesToLoad,
  });

  const { suggestedRelationships, isLoadingSuggestedRelationships } =
    useAllSuggestedRelationships({
      dataSourceName,
      isEnabled: true,
      omitTracked: false,
    });

  if (isLoadingTables || isLoadingSuggestedRelationships || isLoadingSources)
    return { isLoading: true, tables: [] };

  return {
    isLoading: false,
    tables:
      tables?.map(({ metadataTable, columns, sourceName }) => {
        return {
          table: metadataTable.table,
          dataSource: sources?.find(source => source.name === sourceName),
          relationships: getAllTableRelationships(
            metadataTable,
            dataSourceName,
            suggestedRelationships
          ),
          columns,
        };
      }) ?? [],
  };
};
