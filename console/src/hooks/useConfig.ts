import Endpoints from '@/Endpoints';
import { useQuery, UseQueryOptions } from 'react-query';
import { useAppSelector } from '../store';
import { Api } from './apiUtils';

export interface Config {
  migrationMode: boolean;
  readOnlyMode: boolean;
}

export function useMigrationMode(
  queryOptions?: UseQueryOptions<{ migration_mode: boolean }, Error, boolean>
) {
  const headers = useAppSelector(s => s.tables.dataHeaders);
  const migrationUrl = Endpoints.hasuractlMigrateSettings;
  return useQuery({
    queryKey: 'migrationMode',
    queryFn() {
      return Api.get<{ migration_mode: boolean }>({
        url: migrationUrl,
        headers,
      });
    },
    ...queryOptions,
    select: d => d.migration_mode,
  });
}

export function useReadOnlyMode(
  queryOptions?: UseQueryOptions<{ readOnlyMode: boolean }, Error, boolean>
) {
  return useQuery({
    queryKey: 'readOnlyMode',
    queryFn: () => Promise.resolve({ readOnlyMode: false }),
    ...queryOptions,
    select: d => d.readOnlyMode,
  });
}
