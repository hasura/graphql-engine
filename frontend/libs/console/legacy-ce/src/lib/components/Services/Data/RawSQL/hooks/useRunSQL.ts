// eslint-disable-next-line no-restricted-imports
import { runSQL, RunSQLResponse } from '../../../../../features/DataSource/api';
import { SupportedDrivers } from '../../../../../features/hasura-metadata-types';
import { useHttpClient } from '../../../../../features/Network';
import { useCallback, useState } from 'react';

/**
 * This run SQL hook is the new implementation of the run sql api using react hooks. Right now, it's used only
 * for gdc based sources since the old rawSQL.js UI needs a rewrite and decoupling from redux
 */
export const useRunSQL = (props: { onError?: (err: unknown) => void }) => {
  const httpClient = useHttpClient();

  const [data, setData] = useState<RunSQLResponse | undefined>();
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState<Error | undefined>();

  const fetchRunSQLResult = useCallback(
    async ({
      driver,
      dataSourceName,
      sql,
    }: {
      dataSourceName: string;
      driver: SupportedDrivers;
      sql: string;
    }) => {
      setData(undefined);
      setIsLoading(true);

      try {
        const result = await runSQL({
          httpClient,
          source: {
            kind: driver,
            name: dataSourceName,
          },
          sql,
        });

        setData(result);
        setIsLoading(false);
      } catch (err) {
        setError(err as Error);
        if (props.onError) {
          props.onError(err);
        }
      } finally {
        setIsLoading(false);
      }
    },
    [httpClient]
  );

  return { fetchRunSQLResult, data, isLoading, error };
};
