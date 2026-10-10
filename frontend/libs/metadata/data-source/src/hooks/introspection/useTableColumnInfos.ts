import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useQuery, UseQueryOptions } from '@tanstack/react-query';
import { useAppContext } from '@hasura/shared/context';
import { QualifiedDataSource, Table } from '@hasura/shared/types';
import { getDatabaseMethods, TableColumn } from '../../driver';
import { HttpError } from '@hasura/shared/types';
import { defaultQueryOptions } from '@hasura/metadata/api';

type Options<T = TableColumn[]> = Omit<
  UseQueryOptions<TableColumn[], HttpError, T>,
  'queryKey'
>;

export const useTableColumnInfos = <T = TableColumn[]>(
  {
    table,
    source,
  }: {
    table: Table | null | undefined;
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

      return dataSource.introspection.getTableColumnInfos({
        endpoints,
        fetchJson,
        dataSourceName: source.name,
        table,
      });
    },
    ...defaultQueryOptions,
    ...options,
    enabled: Boolean(table && source) && options?.enabled !== false,
  });
};
