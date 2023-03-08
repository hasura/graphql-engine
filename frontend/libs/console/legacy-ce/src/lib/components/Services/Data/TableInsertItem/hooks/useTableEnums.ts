// eslint-disable-next-line @typescript-eslint/ban-ts-comment
import Endpoints from '../../../../../Endpoints';
import { useHttpClient } from '../../../../../features/Network';
import { useQuery, UseQueryResult } from 'react-query';
import { useTablesForeignKeys } from './useTableForeignKeys';
import { TableObject, ForeignKeyMapping } from '../types';
import { areTablesEqual } from '../../../../../features/hasura-metadata-api';

export type UseTableEnumOptionsProps = {
  tables: TableObject[];
  dataSourceName: string;
};

type UseTableEnumsResponseType = {
  from: string;
  to: string;
  column: string[];
  values: string[];
};

export type UseTableEnumsResponseArrayType = UseTableEnumsResponseType[];

export const useTableEnums = ({
  tables,
  dataSourceName,
}: UseTableEnumOptionsProps): UseQueryResult<UseTableEnumsResponseArrayType> => {
  const httpClient = useHttpClient();

  const { data: foreignKeys } = useTablesForeignKeys({
    tables,
    dataSourceName,
  });

  return useQuery({
    queryKey: ['tables-enum', JSON.stringify(tables), dataSourceName],
    queryFn: async () => {
      try {
        const enums = [];
        if (!foreignKeys) return [];
        /* eslint-disable no-await-in-loop */
        for (const table of tables) {
          const relation = foreignKeys.reduce(
            (tally: ForeignKeyMapping | null, fk: ForeignKeyMapping[]) => {
              const found = fk.find(f => areTablesEqual(f.to.table, table));
              if (!found) return tally;
              return found;
            },
            null
          );
          if (!relation) return [];
          const body = {
            type: 'select',
            args: {
              source: dataSourceName,
              columns: relation.to.column,
              table,
            },
          };

          const url = Endpoints.query;
          const response = await httpClient.post(url, JSON.stringify(body));
          const column = Object.keys(response?.data?.[0])?.[0];
          const values = response?.data?.map(
            (v: Record<string, string>) => v[column]
          );

          enums.push({
            from: relation.from.column[0],
            to: relation.to.table,
            column,
            values,
          });
        }
        return enums;
      } catch (err: any) {
        throw new Error(err);
      }
    },
    enabled: !!foreignKeys,
    refetchOnWindowFocus: true,
  });
};
