import { useQuery, UseQueryResult } from 'react-query';
import { DataSource } from '../../../../../features/DataSource';
import { useHttpClient } from '../../../../../features/Network';

import { TableObject, ForeignKeyMapping } from '../types';

export type UseTableForeignKeysProps = {
  tables: TableObject[];
  dataSourceName: string;
};

export const useTablesForeignKeys = ({
  tables,
  dataSourceName,
}: UseTableForeignKeysProps): UseQueryResult<ForeignKeyMapping[][]> => {
  const httpClient = useHttpClient();

  return useQuery({
    queryKey: ['tables-fk', JSON.stringify(tables), dataSourceName],
    queryFn: async () => {
      try {
        const foreignKeys = [];
        /* eslint-disable no-await-in-loop */
        for (const table of tables) {
          const response = await DataSource(httpClient).getTableFkRelationships(
            {
              dataSourceName,
              table,
            }
          );
          foreignKeys.push(response);
        }

        return foreignKeys;
      } catch (err: any) {
        throw new Error(err);
      }
    },
    refetchOnWindowFocus: false,
  });
};
