import type { QualifiedDataSource, Table, Where } from '@hasura/shared/types';
import { useQuery, UseQueryOptions } from '@tanstack/react-query';
import { isScalarGraphQLType } from '@hasura/shared/utils';
import { OrderBy } from '@hasura/shared/types';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { getDatabaseMethods, TableColumn, TableRow } from '../../../driver';

/**
 * TODO: We need to set this to 3600 * 1000 (1 hour) after removing the useQuery usage from the component level hooks
 * which seems to be interferring with qc.invalidateQuery effect from trickling down to the leaf level hooks.
 */
export const DEFAULT_STALE_TIME = 0;

export type UseRowsPropType = {
  source: QualifiedDataSource | undefined;
  table: Table | undefined;
  columns: TableColumn[] | undefined;
  options?: {
    where?: Where;
    offset?: number;
    limit?: number;
    order_by?: OrderBy[];
  };
};

export function getBrowseRowsQueryKey({
  source,
  table,
  options,
}: UseRowsPropType) {
  return [source, table, 'browse-rows', JSON.stringify(options)];
}

type QueryOptions<T = TableRow[]> = Omit<
  UseQueryOptions<TableRow[], unknown, T>,
  'queryKey' | 'queryFn'
>;

export function useRows<T = TableRow[]>(
  props: UseRowsPropType,
  options?: QueryOptions<T>,
) {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const queryKey = getBrowseRowsQueryKey(props);

  return useQuery<TableRow[], unknown, T>({
    refetchOnWindowFocus: false,
    staleTime: DEFAULT_STALE_TIME,
    ...options,
    queryKey,
    queryFn: () => {
      if (!props.source || !props.table) {
        throw new Error('Source and table are required');
      }

      return getDatabaseMethods(props.source.kind).query.getTableRows({
        endpoints,
        fetchJson,
        dataSourceName: props.source.name,
        table: props.table,
        columns: props.columns ?? [],
        options: props.options,
      });
    },
    enabled:
      Boolean(props.columns?.length) &&
      Boolean(props.source) &&
      Boolean(props.table) &&
      options?.enabled !== false,
  });
}

export function getRowsColumns(columns: TableColumn[] | undefined): string[] {
  return (
    columns
      ?.filter((column) => {
        return isScalarGraphQLType(column.graphQLProperties?.graphQLType);
      })
      .map((column) => column.name) ?? []
  );
}
