import { Button, SkeletonList } from '@hasura/shared/ui';
import { useState } from 'react';
import { ColumnRow } from './components/ColumnRow';
import { convertTableValue } from './InsertRowForm.utils';
import { SupportedDriver } from '@hasura/shared/types';
import {
  TableColumn,
  useSupportedDataTypes,
} from '@hasura/metadata/data-source';
import { Flex } from '@radix-ui/themes';

export type InsertRowFormProps = {
  columns: (TableColumn & {
    placeholder: string;
    insertable: boolean;
    description: string;
  })[];
  isInserting: boolean;
  isLoading: boolean;
  onInsertRow: (formData: Record<string, unknown>) => void;
  driver: SupportedDriver;
  initialValues?: Record<string, unknown>;
  submitLabel?: string;
};

export const InsertRowForm: React.FC<InsertRowFormProps> = ({
  columns,
  isInserting = false,
  isLoading = false,
  onInsertRow,
  driver,
  initialValues,
  submitLabel = 'Insert row',
}) => {
  const { data: supportedDataTypes } = useSupportedDataTypes(driver);
  const [values, setValues] = useState<Record<string, unknown>[]>(() =>
    initialValues
      ? Object.entries(initialValues)
          .filter(([, value]) => value !== undefined)
          .map(([columnName, value]) => ({ [columnName]: value }))
      : [],
  );

  const onInsert = () => {
    const adaptedValues = values.reduce<Record<string, unknown>>(
      (acc, value) => {
        const columnName = Object.keys(value)[0];
        const columnValue = Object.values(value)[0];

        const columnDefinition = columns.find(
          (column) => column.name === columnName,
        );

        const finalColumnValue = convertTableValue(
          columnValue,
          columnDefinition?.dataType,
        );

        const formData: Record<string, unknown> = {
          [columnName]: finalColumnValue,
        };

        return {
          ...acc,
          ...formData,
        };
      },
      {},
    );

    onInsertRow(adaptedValues);
  };

  const onChange = (e: {
    columnName: string;
    selectionType: 'default' | 'value' | 'null';
    value?: unknown;
  }): void => {
    setValues((prev) => {
      const prevValue = prev.find(
        (value) => Object.keys(value)[0] === e.columnName,
      );
      if (prevValue) {
        if (e.selectionType === 'default') {
          return prev.filter((value) => Object.keys(value)[0] !== e.columnName);
        }

        if (e.selectionType === 'null') {
          return prev.reduce<Record<string, unknown>[]>((acc, value) => {
            if (Object.keys(value)[0] === e.columnName) {
              return [...acc, { [e.columnName]: null }];
            }
            return [...acc, value];
          }, []);
        }

        if (e.selectionType === 'value') {
          return prev.reduce<Record<string, unknown>[]>((acc, value) => {
            if (Object.keys(value)[0] === e.columnName) {
              return [...acc, { [e.columnName]: e.value }];
            }
            return [...acc, value];
          }, []);
        }
      }

      if (e.selectionType === 'null') {
        return [...prev, { [e.columnName]: null }];
      }

      if (e.selectionType === 'value') {
        return [...prev, { [e.columnName]: e.value }];
      }

      return prev;
    });
  };

  const [resetToken, setResetToken] = useState('');
  const onResetForm = () => {
    setResetToken(Math.random().toString());
  };

  if (isLoading) {
    return <SkeletonList count={3} containerClassName="p-4 gap-2" />;
  }

  return (
    <form
      onSubmit={(e) => {
        e.preventDefault();
        onInsert();
      }}
    >
      <Flex direction="column" gap="3" className="p-4">
        {columns.map((column) => (
          <ColumnRow
            key={column.name}
            label={column.name}
            name={column.name}
            supportedDataTypes={supportedDataTypes}
            onChange={onChange}
            placeholder={column.placeholder}
            isDisabled={!column.insertable}
            // TODO-NEXT: disable if the column has no default value
            isDefaultDisabled={false}
            isNullDisabled={!column.nullable}
            resetToken={resetToken}
            dataType={
              column?.value_generated?.type === 'auto_increment'
                ? `${column.dataType} Auto Increment`
                : column.dataType
            }
            driver={driver}
            initialValue={initialValues?.[column.name]}
          />
        ))}
      </Flex>
      <Flex gap="3">
        <Button mode="default" onClick={() => onResetForm()}>
          Reset
        </Button>
        <Button mode="primary" loading={isInserting} type="submit">
          {submitLabel}
        </Button>
      </Flex>
    </form>
  );
};
