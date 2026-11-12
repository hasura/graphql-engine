import type { QualifiedDataSource, Table, Where } from '@hasura/shared/types';
import { useQuery, UseQueryOptions } from '@tanstack/react-query';
import { OrderBy } from '@hasura/shared/types';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { useMetadata } from '@hasura/metadata/api';
import { getDatabaseMethods, TableColumn, TableRow } from '../../../driver';

const DEFAULT_STALE_TIME = 1000;

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

  // reloaded when the rows mount only, instead of exported on every page
  const { data: metadata, isFetching: isMetadataFetching } = useMetadata(
    undefined,
    { staleTime: 0 },
  );

  return useQuery<TableRow[], unknown, T>({
    refetchOnWindowFocus: false,
    staleTime: DEFAULT_STALE_TIME,
    gcTime: DEFAULT_STALE_TIME,
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
        metadata,
      });
    },
    enabled:
      Boolean(metadata) &&
      !isMetadataFetching &&
      Boolean(props.columns?.length) &&
      Boolean(props.source) &&
      Boolean(props.table) &&
      options?.enabled !== false,
  });
}
