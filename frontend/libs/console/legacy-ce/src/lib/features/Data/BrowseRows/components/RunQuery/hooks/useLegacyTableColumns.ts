import { useIsUnmounted, useAuthFetchJson } from '@hasura/shared/hooks';
import { DataSource, TableColumn } from '@hasura/metadata/data-source';
import { useEffect, useState } from 'react';

type UseTableColumnsProps = {
  dataSourceName: string;
  table: unknown;
};

export const useLegacyTableColumns = ({
  dataSourceName,
  table,
}: UseTableColumnsProps) => {
  const fetchJson = useAuthFetchJson();
  const [tableColumns, setTableColumns] = useState<TableColumn[]>([]);
  const isUnMounted = useIsUnmounted();

  useEffect(() => {
    async function fetchTableColumns() {
      const tableColumnDefinitions = await DataSource(
        fetchJson,
      ).getTableColumns({
        dataSourceName,
        table,
      });

      if (isUnMounted()) {
        return;
      }
      setTableColumns(tableColumnDefinitions);
    }
    fetchTableColumns();
  }, [dataSourceName, table, isUnMounted]);

  return tableColumns;
};
