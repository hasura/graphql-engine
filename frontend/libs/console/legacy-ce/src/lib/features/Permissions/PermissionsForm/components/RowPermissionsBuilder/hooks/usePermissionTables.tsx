import { getAllTableRelationships } from '../../../../../DatabaseRelationships/utils/tableRelationships';
import { useTablesWithColumns } from './useTablesWithColumns';
import { useSources } from '../../../../../MetadataAPI';
import { Tables } from '../components';
import { useAllSuggestedRelationships } from '../../../../../DatabaseRelationships/components/SuggestedRelationships/hooks/useAllSuggestedRelationships';
import { Table } from '../../../../../hasura-metadata-types';

export const usePermissionTables = ({
  dataSourceName,
  tablesToLoad,
}: {
  dataSourceName: string;
  tablesToLoad: Table[];
}): { isLoading: boolean; tables: Tables | null } => {
  const { data: sources, isLoading: isLoadingSources } = useSources();
  const { data: tables, isLoading: isLoadingTables } = useTablesWithColumns({
    dataSourceName,
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
      tables?.map(({ metadataTable, columns }) => {
        return {
          table: metadataTable.table,
          dataSource: sources?.find(source => source.name === dataSourceName),
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
