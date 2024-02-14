import { hasuraToast } from '../../new-components/Toasts';
import { useInsertRow, FormData } from '../Data/hooks/useInsertRow';
import { useListAllTableColumns } from '../Data/hooks/useListAllTableColumns';
import { getPlaceholder } from './utils/getPlaceholder';
import { Table } from '../hasura-metadata-types/source/table';
import { InsertRowForm, InsertRowFormProps } from './InsertRowForm';
import { useTableInfo } from '../Data/hooks/useTableInfo';
import { useMetadata } from '../hasura-metadata-api';
import {
  NATIVE_DRIVERS,
  NativeDrivers,
  SupportedDrivers,
} from '../hasura-metadata-types';
import { columnDataType } from '../DataSource/utils';

type InsertRowFormContainerProps = {
  dataSourceName: string;
  table: Table;
};

export const InsertRowFormContainer = ({
  dataSourceName,
  table,
}: InsertRowFormContainerProps) => {
  const { columns: tableColumns, isLoading: isLoadingColumns } =
    useListAllTableColumns(dataSourceName, table);

  const { data: driver, isLoading: isLoadingMetadata } = useMetadata(
    m => m.metadata.sources.find(source => source.name === dataSourceName)?.kind
  );

  const isGdc = !NATIVE_DRIVERS.includes((driver || '') as NativeDrivers);

  const { data: tableInfo, isLoading: isLoadingTableInfo } = useTableInfo({
    dataSourceName,
    table,
    isEnabled: isGdc,
  });

  const isLoading = isLoadingColumns || isLoadingTableInfo || isLoadingMetadata;

  const onInsertSuccess = () =>
    hasuraToast({
      type: 'success',
      title: 'Success',
      message: 'The row has been successfully inserted',
    });

  const onInsertError = (error: Error) => {
    hasuraToast({
      type: 'error',
      title: 'Error while inserting row',
      message: error.message,
    });
  };

  const { insertRow, isLoading: isInserting } = useInsertRow({
    dataSourceName,
    table,
    onSuccess: onInsertSuccess,
    onError: onInsertError,
  });

  const onInsertRow = async (formData: FormData) => {
    await insertRow(formData);
  };

  const columnsDefinitions = tableInfo?.columns;

  const columns: InsertRowFormProps['columns'] = tableColumns.map(column => {
    const columnInfo = columnsDefinitions?.find(
      columnDefinition => columnDefinition.name === column.name
    );

    const dataType = columnInfo?.type || column.dataType;

    return {
      ...column,
      insertable:
        typeof columnInfo?.insertable !== 'undefined'
          ? columnInfo?.insertable
          : true,
      description: columnInfo?.description || '',
      dataType,
      placeholder: getPlaceholder(columnDataType(dataType)),
    };
  });

  return (
    <InsertRowForm
      columns={columns}
      isInserting={isInserting}
      isLoading={isLoading}
      onInsertRow={onInsertRow}
      driver={driver as SupportedDrivers}
    />
  );
};
