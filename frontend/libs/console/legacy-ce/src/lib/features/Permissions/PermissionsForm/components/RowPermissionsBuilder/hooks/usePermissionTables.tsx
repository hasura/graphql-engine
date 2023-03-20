import { areTablesEqual } from '../../../../../hasura-metadata-api';
import { tableRelationships } from '../../../../../DatabaseRelationships/utils/tableRelationships';
import { useTablesFkConstraints } from './useTablesFkConstraints';
import { useTablesWithColumns } from './useTablesWithColumns';
import { useSources } from '../../../../../MetadataAPI';
import { Tables } from '../components';

export const usePermissionTables = ({
  dataSourceName,
}: {
  dataSourceName: string;
}): Tables => {
  const { data: sources, isLoading: isLoadingSources } = useSources();
  const { data: tables, isLoading: isLoadingTables } = useTablesWithColumns({
    dataSourceName,
  });
  const { data: fkConstraints, isLoading: isDALIntrospectionLoading } =
    useTablesFkConstraints({ dataSourceName, tables });
  if (isLoadingTables || isDALIntrospectionLoading || isLoadingSources)
    return [];

  return (
    tables?.map(({ metadataTable, columns }) => {
      const relationships = fkConstraints?.find(({ table }) =>
        areTablesEqual(table, metadataTable.table)
      )?.relationships;
      return {
        table: metadataTable.table,
        dataSource: sources?.find(source => source.name === dataSourceName),
        relationships: tableRelationships(
          metadataTable,
          dataSourceName,
          relationships
        ),
        columns,
      };
    }) ?? []
  );
};
