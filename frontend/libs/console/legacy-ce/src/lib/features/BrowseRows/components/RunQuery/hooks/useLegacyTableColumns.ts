import { useIsUnmounted } from '../../../../../components/Services/Data/Common/tsUtils';
import { DataSource, TableColumn } from '../../../../DataSource';
import { useHttpClient } from '../../../../Network';
import { useEffect, useState } from 'react';

type UseTableColumnsProps = {
  dataSourceName: string;
  table: unknown;
};

export const useLegacyTableColumns = ({
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
  }, [dataSourceName, table, isUnMounted]);

  return tableColumns;
};
