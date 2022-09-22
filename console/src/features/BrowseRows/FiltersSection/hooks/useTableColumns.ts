import { useState, useEffect } from 'react';
import { DataSource, TableColumn } from '@/features/DataSource';
import { useHttpClient } from '@/features/Network';
import { useIsUnmounted } from '@/components/Services/Data';

type UseTableColumnsProps = {
  dataSourceName: string;
  table: unknown;
};

export const useTableColumns = ({
  dataSourceName,
  table,
}: UseTableColumnsProps) => {
  const httpClient = useHttpClient();
  const [tableColumns, setTableColumns] = useState<TableColumn[]>([]);
  const isUnMounted = useIsUnmounted();

  useEffect(() => {
    async function fetchTableColumns() {
      const tableColumnDefinitions = await DataSource(
        httpClient
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
  }, [dataSourceName, httpClient, table, isUnMounted]);

  return tableColumns;
};
