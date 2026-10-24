import { Source, Table } from '@hasura/shared/types';
import {
  getTableColumnDataTypePlaceholder,
  useEditRows,
  useEnumColumnValues,
  useTableColumns,
} from '@hasura/metadata/data-source';
import { areTablesEqual } from '@hasura/metadata/helpers';
import { EditRowForm, EditRowFormProps } from './EditRowForm';
import { ReloadEnumValuesButton } from '../InsertRow/components/ReloadEnumValuesButton';

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
  const { data: enumColumnValues } = useEnumColumnValues({ source, table });
  // the GraphQL enum type only reflects updated rows after a metadata reload
  const isEnumTable = source.tables.some(
    (t) => t.is_enum && areTablesEqual(t.table, table),
  );

  const { mutate: editRow, isPending: isSubmitting } = useEditRows();

  const columns: EditRowFormProps['columns'] =
    tableColumnQueryResult?.columns.map((column) => ({
      ...column,
      editable: true,
      description: '',
      placeholder: column.consoleDataType
        ? getTableColumnDataTypePlaceholder(column.consoleDataType)
        : '',
      enumValues: enumColumnValues?.[column.name],
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
      actions={
        isEnumTable ? (
          <ReloadEnumValuesButton sourceName={source.name} />
        ) : undefined
      }
    />
  );
};
