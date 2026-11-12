import { useState } from 'react';
import { Source, Table } from '@hasura/shared/types';
import { useAppContext } from '@hasura/shared/context';
import { useMigrationStatus } from '@hasura/metadata/api';
import { areTablesEqual, isPostgresFlavour } from '@hasura/metadata/helpers';
import { InsertRowForm, InsertRowFormProps } from './InsertRowForm';
import { ReloadEnumValuesButton } from './components/ReloadEnumValuesButton';
import {
  getTableColumnDataTypePlaceholder,
  useEnumColumnValues,
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

  const { data: tableInfo, isLoading: isLoadingTableInfo } = useTableColumns({
    source,
    table,
  });
  const { data: enumColumnValues } = useEnumColumnValues({ source, table });
  // the GraphQL enum type only reflects new rows after a metadata reload
  const isEnumTable = source.tables.some(
    (t) => t.is_enum && areTablesEqual(t.table, table),
  );

  const { envVars } = useAppContext();
  const { data: migrationStatus } = useMigrationStatus();
  const [isMigration, setIsMigration] = useState(false);
  // only the Postgres-family drivers can write the insertion as a migration
  const canCreateMigration =
    envVars.consoleMode === 'cli' &&
    migrationStatus === 'healthy' &&
    isPostgresFlavour(source.kind);

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
        isMigration: canCreateMigration && isMigration,
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
        enumValues: enumColumnValues?.[column.name],
      };
    }) ?? [];

  return (
    <InsertRowForm
      columns={columns}
      isInserting={isInserting}
      isLoading={isLoading}
      onInsertRow={onInsertRow}
      initialValues={initialValues}
      submitLabel={submitLabel}
      actions={
        isEnumTable ? (
          <ReloadEnumValuesButton sourceName={source.name} />
        ) : undefined
      }
      migration={
        canCreateMigration
          ? { checked: isMigration, onChange: setIsMigration }
          : undefined
      }
    />
  );
};
