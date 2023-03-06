import {
  areTablesEqual,
  MetadataSelectors,
  useMetadata,
} from '../../../../../hasura-metadata-api';
import { MetadataTable } from '../../../../../hasura-metadata-types';
import { columnsFromSchema } from '../components/utils/columnsFromSchema';
import { getTableDisplayName } from '../../../../../DatabaseRelationships';
import { useIntrospectSchema } from '.';
import { tableRelationships } from '../../../../../DatabaseRelationships/utils/tableRelationships';
import { useTables } from '../../../../../Data/TrackTables/hooks/useTables';
import { useTablesFkConstraints } from './useTablesFkConstraints';

const getTableName = (table: MetadataTable) => {
  // Table name. Replace . with _ because GraphQL doesn't allow . in field names
  if (table?.configuration?.custom_name)
    return table.configuration.custom_name.replace(/\./g, '_');
  return getTableDisplayName(table.table).replace(/\./g, '_');
};

export const usePermissionTables = ({
  dataSourceName,
}: {
  dataSourceName: string;
}) => {
  const { data: metadataTables, isLoading: isLoadingMetadataTables } =
    useMetadata(MetadataSelectors.getTables(dataSourceName));
  const { data: schema } = useIntrospectSchema();
  const { data: tables, isLoading: isLoadingTables } = useTables({
    dataSourceName,
  });
  const { data: fkConstraints, isLoading: isDALIntrospectionLoading } =
    useTablesFkConstraints({ dataSourceName, tables });
  if (isLoadingMetadataTables || isLoadingTables || isDALIntrospectionLoading)
    return [];
  const allColumns = columnsFromSchema(schema);
  return (
    metadataTables?.map(metadataTable => {
      const tableName = getTableName(metadataTable);
      const table = tables?.find(t =>
        areTablesEqual(metadataTable.table, t.table)
      );
      const relationships = fkConstraints?.find(({ table }) =>
        areTablesEqual(table, metadataTable.table)
      )?.relationships;
      return {
        table: metadataTable.table,
        dataSource: dataSourceName,
        relationships: tableRelationships(
          metadataTable,
          table?.table,
          dataSourceName,
          relationships
        ),
        columns: allColumns[tableName] ?? [],
      };
    }) ?? []
  );
};
