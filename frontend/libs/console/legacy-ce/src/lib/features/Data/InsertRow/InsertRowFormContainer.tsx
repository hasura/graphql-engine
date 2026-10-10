import { Source, Table } from '@hasura/shared/types';
import { InsertRowForm, InsertRowFormProps } from './InsertRowForm';
import { SupportedDriver } from '@hasura/shared/types';
import {
  getTableColumnDataTypePlaceholder,
  useInsertRows,
  useTableColumns,
} from '@hasura/metadata/data-source';

type InsertRowFormContainerProps = {
  source: Source;
  table: Table;
  initialValues?: Record<string, unknown>;
  submitLabel?: string;
  onSuccess?: () => void;
};

export const InsertRowFormContainer = ({
  source,
  table,
  initialValues,
  submitLabel,
  onSuccess,
}: InsertRowFormContainerProps) => {
  const { data: tableColumns, isLoading: isLoadingColumns } = useTableColumns({
    source,
    table,
  });
  const driver = source.kind;

  const { data: tableInfo, isLoading: isLoadingTableInfo } = useTableColumns({
    source,
    table,
  });

  const isLoading = isLoadingColumns || isLoadingTableInfo;
  const { mutate: insertRows, isPending: isInserting } = useInsertRows();

  const onInsertRow = async (formData: Record<string, unknown>) => {
    if (!source) {
      return;
    }

    await insertRows(
      {
        table,
        source,
        objects: [formData],
        columns,
        defaultColumns: [],
      },
      { onSuccess },
    );
  };

  const columnsDefinitions = tableInfo?.columns;

  const columns: InsertRowFormProps['columns'] =
    tableColumns?.columns.map((column) => {
      const columnInfo = columnsDefinitions?.find(
        (columnDefinition) => columnDefinition.name === column.name,
      );

      return {
        ...column,
        insertable: true,
        description: '',
        placeholder: columnInfo?.consoleDataType
          ? getTableColumnDataTypePlaceholder(columnInfo.consoleDataType)
          : '',
      };
    }) ?? [];

  return (
    <InsertRowForm
      columns={columns}
      isInserting={isInserting}
      isLoading={isLoading}
      onInsertRow={onInsertRow}
      driver={driver as SupportedDriver}
      initialValues={initialValues}
      submitLabel={submitLabel}
    />
  );
};
