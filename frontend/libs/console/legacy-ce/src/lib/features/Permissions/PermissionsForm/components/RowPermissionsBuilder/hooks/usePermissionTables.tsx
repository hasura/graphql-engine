import { getAllTableRelationships } from '../../../../../DatabaseRelationships/utils/tableRelationships';
import { useTablesWithColumns } from './useTablesWithColumns';
import { useSources } from '../../../../../MetadataAPI';
import { Tables } from '../components';
import { useAllSuggestedRelationships } from '../../../../../DatabaseRelationships/components/SuggestedRelationships/hooks/useAllSuggestedRelationships';

export const usePermissionTables = ({
  dataSourceName,
}: {
  dataSourceName: string;
}): Tables => {
  const { data: sources, isLoading: isLoadingSources } = useSources();
  const { data: tables, isLoading: isLoadingTables } = useTablesWithColumns({
    dataSourceName,
  });

  const { suggestedRelationships, isLoadingSuggestedRelationships } =
    useAllSuggestedRelationships({
      dataSourceName,
      isEnabled: true,
      omitTracked: false,
    });

  if (isLoadingTables || isLoadingSuggestedRelationships || isLoadingSources)
    return [];

  return (
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
    }) ?? []
  );
};
