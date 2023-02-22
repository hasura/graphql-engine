import { useEffect, useState } from 'react';
import { Table } from '../../hasura-metadata-types';
import { useHttpClient } from '../../Network';
import { useIsUnmounted } from '../../../components/Services/Data/DataSources/CreateDataSource/Neon/useIsUnmounted';
import { DataSource } from '../../DataSource';

export function useDefaultQueryRoot({
  dataSourceName,
  table,
}: {
  dataSourceName: string;
  table: Table;
}) {
  const httpClient = useHttpClient();
  const [defaultQueryRoot, setDefaultQueryRoot] = useState<string>('');
  const isUnMounted = useIsUnmounted();

  useEffect(() => {
    async function fetchTableOperators() {
      const result = await DataSource(httpClient).getDefaultQueryRoot({
        dataSourceName,
        table,
      });

      if (isUnMounted()) {
        return;
      }

      if (typeof result === 'string') {
        setDefaultQueryRoot(result);
      }
    }
    fetchTableOperators();
  }, [dataSourceName, isUnMounted]);

  return defaultQueryRoot;
}
