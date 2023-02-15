import { Button } from '@/new-components/Button';
import { useState } from 'react';
import { ListAllTableColumnsReturn } from '../Data/hooks/useListAllTableColumns';
import { InsertRowArgs } from '../DataSource';
import { ColumnRow } from './components/ColumnRow';
import { FormData } from '../Data/hooks/useInsertRow';
import Skeleton from 'react-loading-skeleton';

export type InsertRowFormProps = {
  columns: ListAllTableColumnsReturn['columns'];
  isInserting: boolean;
  isLoading: boolean;
  onInsertRow: (formData: FormData) => void;
};

export const InsertRowForm: React.VFC<InsertRowFormProps> = ({
  columns,
  isInserting = false,
  isLoading = false,
  onInsertRow,
}) => {
  const [values, setValues] = useState<InsertRowArgs['rowValues'][]>([]);

  const onInsert = () => {
    const adaptedValues = values.reduce<FormData>((acc, value) => {
      const formData: FormData = {
        [Object.keys(value)[0]]: {
          option: 'value',
          value: Object.values(value)[0],
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
    <form onSubmit={() => onInsert()}>
      <div className="flex flex-col my-6 gap-2 max-w-screen-sm">
        {columns.map(column => (
          <ColumnRow
            key={column.name}
            label={column.name}
            name={column.name}
            onChange={onChange}
            // disable if the column is auto-increment or auto-generated
            isDisabled={false}
            // disable if the column has no default value
            isDefaultDisabled={false}
            isNullDisabled={!column.nullable}
            resetToken={resetToken}
          />
        ))}
      </div>
      <div className="flex gap-3">
        <Button onClick={() => onResetForm()}>Reset</Button>
        <Button
          mode="primary"
          onClick={() => onInsert()}
          isLoading={isInserting}
          type="submit"
        >
          Insert row
        </Button>
      </div>
    </form>
  );
};
