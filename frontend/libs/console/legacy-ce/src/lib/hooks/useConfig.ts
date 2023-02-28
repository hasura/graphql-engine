import { SERVER_CONSOLE_MODE } from '../constants';
import Endpoints from '../Endpoints';
import { useQuery, UseQueryOptions, UseQueryResult } from 'react-query';
import { useAppSelector } from '../storeHooks';
import { Api } from './apiUtils';
import { useConsoleConfig } from './useEnvVars';

export interface Config {
  migrationMode: boolean;
  readOnlyMode: boolean;
}

export function useMigrationMode(
  queryOptions?: UseQueryOptions<{ migration_mode: boolean }, Error, boolean>
): UseQueryResult<boolean> {
  const headers = useAppSelector(s => s.tables.dataHeaders);
  const migrationUrl = Endpoints.hasuraCliServerMigrateSettings;
  const { mode } = useConsoleConfig();
  return useQuery({
    queryKey: 'migrationMode',
    queryFn() {
      if (mode === SERVER_CONSOLE_MODE)
        return Promise.resolve({ migration_mode: false });

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
