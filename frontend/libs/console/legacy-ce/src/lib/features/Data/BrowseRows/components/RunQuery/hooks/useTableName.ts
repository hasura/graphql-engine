import { useIsUnmounted, useAuthFetchJson } from '@hasura/shared/hooks';
import { DataSource, getTableName } from '@hasura/metadata/data-source';
import { useState, useEffect } from 'react';

type UseTableNameProps = {
  dataSourceName: string;
  table: unknown;
};

export const useTableName = ({ dataSourceName, table }: UseTableNameProps) => {
  const fetchJson = useAuthFetchJson();
  const [tableName, setTableName] = useState('');
  const isUnMounted = useIsUnmounted();

  useEffect(() => {
    if (isUnMounted()) {
      return;
    }

    async function fetchTableHierarchy() {
      const databaseHierarchy = await DataSource(
        fetchJson,
      ).getDatabaseHierarchy({ dataSourceName });

      const aTableName = getTableName(table, databaseHierarchy);
      setTableName(aTableName);
    }
    fetchTableHierarchy();
  }, [dataSourceName, isUnMounted, table]);

  return tableName;
};
