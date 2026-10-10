import { useState, useEffect } from 'react';
import { DataSource, Feature, Operator } from '@hasura/metadata/data-source';
import { useIsUnmounted, useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';

export const useDatabaseOperators = ({
  dataSourceName,
}: {
  dataSourceName: string;
}) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const [operators, setOperators] = useState<Operator[]>([]);
  const isUnMounted = useIsUnmounted();

  useEffect(() => {
    async function fetchTableOperators() {
      const result = await DataSource({
        url: endpoints.metadata,
        fetchJson,
      }).getSupportedOperators({
        dataSourceName,
      });

      if (isUnMounted()) {
        return;
      }

      setOperators(result === Feature.NotImplemented ? [] : result);
    }
    fetchTableOperators();
  }, [dataSourceName, isUnMounted]);

  return operators;
};
