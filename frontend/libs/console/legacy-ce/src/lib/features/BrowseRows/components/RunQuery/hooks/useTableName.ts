import { useIsUnmounted } from '../../../../../components/Services/Data/Common/tsUtils';
import { DataSource, getTableName } from '../../../../DataSource';
import { useHttpClient } from '../../../../Network';
import { useState, useEffect } from 'react';

type UseTableNameProps = {
  dataSourceName: string;
  table: unknown;
};

export const useTableName = ({ dataSourceName, table }: UseTableNameProps) => {
  const httpClient = useHttpClient();
  const [tableName, setTableName] = useState('');
  const isUnMounted = useIsUnmounted();

  useEffect(() => {
    async function fetchTableHierarchy() {
      const databaseHierarchy = await DataSource(
        httpClient
      ).getDatabaseHierarchy({ dataSourceName });

      if (isUnMounted()) {
        return;
      }

      const aTableName = getTableName(table, databaseHierarchy);
      setTableName(aTableName);
    }
    fetchTableHierarchy();
  }, [dataSourceName, isUnMounted, table]);

  return tableName;
};
