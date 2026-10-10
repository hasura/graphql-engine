import { useQuery, UseQueryResult } from '@tanstack/react-query';
import { areTablesEqual } from '@hasura/metadata/helpers';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { QualifiedDataSource, Table } from '@hasura/shared/types';
import { useAppContext } from '@hasura/shared/context';
import { TableFkRelationships } from '../../driver';
import { useTablesForeignKeys } from './useTablesForeignKeys';
import { getTableEnumsQueryKey } from '../../types/queryKey';

export type UseTableEnumOptionsProps = {
  tables: Table[];
  source: QualifiedDataSource;
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
  source,
}: UseTableEnumOptionsProps): UseQueryResult<UseTableEnumsResponseArrayType> => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  const { data: foreignKeys } = useTablesForeignKeys({
    tables,
    source,
  });

  return useQuery({
    queryKey: getTableEnumsQueryKey(source.name, tables),
    queryFn: async () => {
      const enums: any[] = [];
      if (!foreignKeys) return [];

      for (const table of tables) {
        const relation = foreignKeys.reduce(
          (tally, fk) => {
            const found = fk.find((f) => areTablesEqual(f.to.table, table));
            if (!found) return tally;
            return found;
          },
          null as TableFkRelationships | null,
        );

        if (!relation) return [];
        const body = {
          type: 'select',
          args: {
            source: source.name,
            columns: relation.to.columns,
            table,
          },
        };

        const url = endpoints.queryV2;
        const response = await fetchJson<{ data: Record<string, any>[] }>(url, {
          method: 'POST',
          body: JSON.stringify(body),
        });
        const column = Object.keys(response?.data?.[0])?.[0];
        const values = response?.data?.map(
          (v: Record<string, string>) => v[column],
        );

        enums.push({
          from: relation.from.columns[0],
          to: relation.to.table,
          column,
          values,
        });
      }
      return enums;
    },
    enabled: Boolean(foreignKeys?.length),
    refetchOnWindowFocus: false,
  });
};
