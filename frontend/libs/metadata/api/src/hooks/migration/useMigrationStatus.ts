import { HttpError } from '@hasura/shared/types';
import { useQuery } from '@tanstack/react-query';
import { MIGRATION_STATUS_QUERY_KEY } from '../constants';
import { requestJson } from '@hasura/shared/utils';
import { useState } from 'react';
import { handleMigrationStatusError } from '../../utils/error';
import { useAppContext } from '@hasura/shared/context';
import { getMigrationStatus } from '../../api';

export type MigrationStatus = 'off' | 'healthy' | 'unhealthy';

type Options = {
  staleTime?: number;
  enabled?: boolean;
};

const DEFAULT_SLATE_TIME = 5000;

export function useMigrationStatus(options?: Options) {
  const { endpoints, envVars } = useAppContext();
  const [updateMigrationStatusProgress, setUpdateMigrationStatusProgress] =
    useState(false);

  const queryResult = useQuery<MigrationStatus, HttpError>({
    queryKey: [MIGRATION_STATUS_QUERY_KEY],
    queryFn: async () => {
      if (envVars.consoleMode === 'server') {
        return 'off';
      }

      return getMigrationStatus(endpoints.hasuraCliServerMigrateSettings);
    },
    enabled: options?.enabled ?? envVars.consoleMode === 'cli',
    staleTime:
      options?.staleTime ??
      (envVars.consoleMode === 'server' ? Infinity : DEFAULT_SLATE_TIME),
  });

  const updateMigrationModeStatus = () => {
    if (envVars.consoleMode !== 'cli') {
      return;
    }

    setUpdateMigrationStatusProgress(true);
    const url = endpoints.hasuraCliServerMigrateSettings;
    const putBody = {
      name: 'migration_mode',
      value: (queryResult.data !== 'healthy').toString(),
    };

    const options = {
      method: 'PUT',
      body: JSON.stringify(putBody),
    };

    return requestJson<{ message: string }>(url, options)
      .then(() => queryResult.refetch())
      .catch((err) =>
        handleMigrationStatusError(err, Boolean(envVars.adminSecret)),
      )
      .finally(() => {
        setUpdateMigrationStatusProgress(false);
      });
  };

  return {
    ...queryResult,
    updateMigrationStatusProgress,
    updateMigrationModeStatus,
  };
}
