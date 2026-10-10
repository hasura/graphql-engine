import { Button, SkeletonList } from '@hasura/shared/ui';
import { useState } from 'react';
import { ColumnRow } from '../InsertRow/components/ColumnRow';
import { convertTableValue } from '../InsertRow/InsertRowForm.utils';
import { SupportedDriver } from '@hasura/shared/types';
import {
  TableColumn,
  useSupportedDataTypes,
} from '@hasura/metadata/data-source';
import { Flex } from '@radix-ui/themes';
import useDataRowForm from './hooks/useDataRowForm';

export type EditRowFormProps = {
  row: Record<string, unknown>;
  columns: (TableColumn & {
    placeholder: string;
    editable: boolean;
    description: string;
  })[];
  isSubmitting: boolean;
  isLoading: boolean;
  onEditRow: (formData: Record<string, unknown>) => void;
  driver: SupportedDriver;
};

export const EditRowForm: React.FC<EditRowFormProps> = ({
  row,
  columns,
  isSubmitting = false,
  isLoading = false,
  onEditRow,
  driver,
}) => {
  const { data: supportedDataTypes } = useSupportedDataTypes(driver);
  const {
    onColumnUpdate,
    handleDefaultValueColumns,
    handleNullChecks,
    transformValues,
  } = useDataRowForm({ initialValues: row });

  const onChange = (e: {
    columnName: string;
    selectionType: 'default' | 'value' | 'null';
    value?: unknown;
  }): void => {
    if (e.selectionType === 'default') {
      handleDefaultValueColumns(e.columnName, true);
      return;
    }

    if (e.selectionType === 'null') {
      handleNullChecks(e.columnName, true);
      return;
    }

    handleNullChecks(e.columnName, false);
    handleDefaultValueColumns(e.columnName, false);

    const columnDefinition = columns.find(
      (column) => column.name === e.columnName,
    );
    onColumnUpdate(
      e.columnName,
      convertTableValue(e.value, columnDefinition?.dataType),
    );
  };

  const [resetToken, setResetToken] = useState('');
  const onResetForm = () => {
    setResetToken(Math.random().toString());
  };

  const onSubmit = () => {
    const set = transformValues(columns);
    onEditRow(set);
  };

  if (isLoading) {
    return <SkeletonList count={3} containerClassName="p-4 gap-2" />;
  }

  return (
    <form
      onSubmit={(e) => {
        e.preventDefault();
        onSubmit();
      }}
    >
      <Flex direction="column" gap="3">
        {columns.map((column) => (
          <ColumnRow
            key={column.name}
            label={column.name}
            name={column.name}
            supportedDataTypes={supportedDataTypes}
            onChange={onChange}
            placeholder={column.placeholder}
            isDisabled={
              !column.editable ||
              column?.value_generated?.type === 'auto_increment'
            }
            isDefaultDisabled={false}
            isNullDisabled={!column.nullable}
            resetToken={resetToken}
            dataType={
              column?.value_generated?.type === 'auto_increment'
                ? `${column.dataType} Auto Increment`
                : column.dataType
            }
            driver={driver}
            initialValue={row[column.name]}
          />
        ))}
      </Flex>
      <Flex gap="3" justify="end" className="mt-6">
        <Button mode="default" onClick={() => onResetForm()}>
          Reset
        </Button>
        <Button mode="primary" loading={isSubmitting} type="submit">
          Save row
        </Button>
      </Flex>
    </form>
  );
};
