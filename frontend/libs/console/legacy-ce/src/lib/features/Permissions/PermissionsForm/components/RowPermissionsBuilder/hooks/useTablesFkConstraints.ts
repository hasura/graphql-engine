import { Table } from '../../../../../hasura-metadata-types';
import { useQuery, UseQueryResult } from 'react-query';
import { useHttpClient } from '../../../../../Network';
import { generateQueryKeys } from '../../../../../DatabaseRelationships/utils/queryClientUtils';
import { DataSource, TableFkRelationships } from '../../../../../DataSource';
import { TableWithColumns } from './useTablesWithColumns';

export function useTablesFkConstraints({
  dataSourceName,
  tables,
}: {
  dataSourceName: string;
  tables: TableWithColumns[] | undefined;
}): UseQueryResult<
  Array<{ table: Table; relationships: TableFkRelationships[] }>
> {
  const httpClient = useHttpClient();

  return useQuery({
    queryKey: generateQueryKeys.manyFkConstraints({ tables, dataSourceName }),
    queryFn: async () => {
      if (!tables) return [];
      const fkConstraints: Array<{
        table: Table;
        relationships: TableFkRelationships[];
      }> = [];
      for (const table of tables) {
        const result = await DataSource(httpClient).getTableFkRelationships({
          dataSourceName,
          table: table.metadataTable.table,
        });
        if (result) {
          fkConstraints.push({
            table: table.metadataTable.table,
            relationships: result,
          });
        }
      }
      return fkConstraints;
    },
    refetchOnWindowFocus: false,
  });
}
