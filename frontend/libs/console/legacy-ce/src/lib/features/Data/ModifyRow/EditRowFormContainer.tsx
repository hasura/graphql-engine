import { Source, Table } from '@hasura/shared/types';
import { SupportedDriver } from '@hasura/shared/types';
import {
  getTableColumnDataTypePlaceholder,
  useEditRows,
  useTableColumns,
} from '@hasura/metadata/data-source';
import { EditRowForm, EditRowFormProps } from './EditRowForm';

type EditRowFormContainerProps = {
  source: Source;
  table: Table;
  row: Record<string, unknown>;
  onSuccess?: () => void;
};

export const EditRowFormContainer = ({
  source,
  table,
  row,
  onSuccess,
}: EditRowFormContainerProps) => {
  const { data: tableColumnQueryResult, isLoading } = useTableColumns({
    source,
    table,
  });
  const driver = source.kind;

  const { mutate: editRow, isPending: isSubmitting } = useEditRows();

  const columns: EditRowFormProps['columns'] =
    tableColumnQueryResult?.columns.map((column) => ({
      ...column,
      editable: true,
      description: '',
      placeholder: column.consoleDataType
        ? getTableColumnDataTypePlaceholder(column.consoleDataType)
        : '',
    })) ?? [];

  const primaryKeyColumns = (tableColumnQueryResult?.columns ?? []).filter(
    (column) => column.isPrimaryKey,
  );

  const onEditRow = (set: Record<string, unknown>) => {
    if (!source || Object.keys(set).length === 0) {
      return;
    }

    editRow(
      {
        table,
        source,
        set,
        where: primaryKeyColumns.reduce(
          (acc, column) => {
            acc[column.name] = row[column.name];
            return acc;
          },
          {} as Record<string, unknown>,
        ),
        columns: tableColumnQueryResult?.columns ?? [],
        defaultColumns: [],
      },
      { onSuccess },
    );
  };

  return (
    <EditRowForm
      row={row}
      columns={columns}
      isSubmitting={isSubmitting}
      isLoading={isLoading}
      onEditRow={onEditRow}
      driver={driver as SupportedDriver}
    />
  );
};
