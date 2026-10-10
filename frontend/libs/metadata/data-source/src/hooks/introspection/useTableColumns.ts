import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useQuery, UseQueryOptions } from '@tanstack/react-query';
import { useAppContext } from '@hasura/shared/context';
import { QualifiedDataSource, Table } from '@hasura/shared/types';
import { getDatabaseMethods, Operator, TableColumn } from '../../driver';
import { HttpError } from '@hasura/shared/types';
import { defaultQueryOptions } from '@hasura/metadata/api';

export type UseTableColumnsResult = {
  columns: TableColumn[];
  supportedOperators: Operator[];
};

type Options<T = UseTableColumnsResult> = Omit<
  UseQueryOptions<UseTableColumnsResult, HttpError, T>,
  'queryKey'
>;

export const useTableColumns = <T = UseTableColumnsResult>(
  {
    table,
    source,
  }: {
    table: Table | undefined;
    source: QualifiedDataSource | undefined;
  },
  options?: Options<T>,
) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return useQuery({
    queryKey: [source?.name, 'column-introspection', table],
    queryFn: async () => {
      if (!source || !table) {
        throw new Error('source and table are required');
      }

      const dataSource = getDatabaseMethods(source.kind);

      const columns = await dataSource.introspection.getTableColumns({
        endpoints,
        fetchJson,
        dataSourceName: source.name,
        table,
      });

      const supportedOperators =
        dataSource.introspection.getSupportedOperators();

      return {
        columns,
        supportedOperators,
      };
    },
    ...defaultQueryOptions,
    ...options,
    enabled: Boolean(table && source) && options?.enabled !== false,
  });
};
