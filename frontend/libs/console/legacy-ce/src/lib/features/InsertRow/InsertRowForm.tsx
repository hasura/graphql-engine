import { Button } from '../../new-components/Button';
import { useState } from 'react';
import { InsertRowArgs } from '../DataSource';
import { ColumnRow } from './components/ColumnRow';
import { FormData } from '../Data/hooks/useInsertRow';
import Skeleton from 'react-loading-skeleton';
import { convertTableValue } from './InsertRowForm.utils';
import { ListAllTableColumn } from '../Data/hooks/useListAllTableColumns';
import { SupportedDrivers } from '../hasura-metadata-types';

export type InsertRowFormProps = {
  columns: (ListAllTableColumn & {
    placeholder: string;
    insertable: boolean;
    description: string;
  })[];
  isInserting: boolean;
  isLoading: boolean;
  onInsertRow: (formData: FormData) => void;
  driver: SupportedDrivers;
};

export const InsertRowForm: React.VFC<InsertRowFormProps> = ({
  columns,
  isInserting = false,
  isLoading = false,
  onInsertRow,
  driver,
}) => {
  const [values, setValues] = useState<InsertRowArgs['rowValues'][]>([]);

  const onInsert = () => {
    const adaptedValues = values.reduce<FormData>((acc, value) => {
      const columnName = Object.keys(value)[0];
      const columnValue = Object.values(value)[0];

      const columnDefinition = columns.find(
        column => column.name === columnName
      );

      const finalColumnValue = convertTableValue(
        columnValue,
        columnDefinition?.dataType
      );

      const formData: FormData = {
        [columnName]: {
          option: 'value',
          value: finalColumnValue,
        },
      };

      return {
        ...acc,
        ...formData,
      };
    }, {});

    onInsertRow(adaptedValues);
  };

  const onChange = (e: {
    columnName: string;
    selectionType: 'default' | 'value' | 'null';
    value?: unknown;
  }): void => {
    setValues(prev => {
      const prevValue = prev.find(
        value => Object.keys(value)[0] === e.columnName
      );
      if (prevValue) {
        if (e.selectionType === 'default') {
          return prev.filter(value => Object.keys(value)[0] !== e.columnName);
        }

        if (e.selectionType === 'null') {
          return prev.reduce<InsertRowArgs['rowValues'][]>((acc, value) => {
            if (Object.keys(value)[0] === e.columnName) {
              return [...acc, { [e.columnName]: null }];
            }
            return [...acc, value];
          }, []);
        }

        if (e.selectionType === 'value') {
          return prev.reduce<InsertRowArgs['rowValues'][]>((acc, value) => {
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
    return (
      <div className="p-md gap-2">
        <Skeleton width={200} height={20} />
        <Skeleton width={200} height={20} />
        <Skeleton width={200} height={20} />
      </div>
    );
  }

  return (
    <form
      onSubmit={e => {
        e.preventDefault();
        onInsert();
      }}
      className="w-full bg-white p-4 rounded-sm border my-2"
    >
      <div className="flex flex-col mb-6 gap-3 max-w-screen-md">
        {columns.map(column => (
          <ColumnRow
            key={column.name}
            label={column.name}
            name={column.name}
            onChange={onChange}
            placeholder={column.placeholder}
            isDisabled={
              !column.insertable ||
              column?.value_generated?.type === 'auto_increment'
            }
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
          />
        ))}
      </div>
      <div className="flex gap-3">
        <Button onClick={() => onResetForm()}>Reset</Button>
        <Button mode="primary" isLoading={isInserting} type="submit">
          Insert row
        </Button>
      </div>
    </form>
  );
};
