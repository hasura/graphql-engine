import {
  isColumnAutoIncrement,
  TableColumn,
} from '@hasura/metadata/data-source';
import { useState } from 'react';

type UseDataRowFormProps = {
  initialValues: Record<string, unknown>;
};

const useDataRowForm = (props: UseDataRowFormProps) => {
  const [touchedValues, setTouchedValues] = useState<Record<string, boolean>>(
    {},
  );
  const [values, setValues] = useState<Record<string, unknown>>(
    props.initialValues ?? {},
  );
  const [defaultValueColumns, setDefaultValueColumns] = useState<
    Record<string, boolean>
  >({});

  const [nullCheckedValues, setNullCheckedValues] = useState<
    Record<string, boolean>
  >({});

  const handleDefaultValueColumns = (columnName: string, value: boolean) => {
    setDefaultValueColumns({
      ...defaultValueColumns,
      [columnName]: value,
    });
    setTouchedValues((prev) => ({
      ...prev,
      [columnName]: true,
    }));
  };

  const handleNullChecks = (columnName: string, value: boolean) => {
    setNullCheckedValues({
      ...nullCheckedValues,
      [columnName]: value,
    });
    setTouchedValues((prev) => ({
      ...prev,
      [columnName]: true,
    }));
  };

  const onColumnUpdate = (columnName: string, value: unknown) => {
    setValues({ ...values, [columnName]: value });
    setTouchedValues((prev) => ({
      ...prev,
      [columnName]: true,
    }));
  };

  const transformValues = (columns: TableColumn[]) => {
    return (
      columns.reduce((tally, col) => {
        const colName = col.name;
        if (!touchedValues[colName] || defaultValueColumns[colName]) {
          return tally;
        }

        if (nullCheckedValues[colName]) {
          return {
            ...tally,
            [colName]: null,
          };
        }

        const isAutoIncrement = isColumnAutoIncrement(col);
        if (!isAutoIncrement && typeof values[colName] !== 'undefined') {
          return { ...tally, [colName]: values[colName] };
        }
        return tally;
      }, {}) ?? {}
    );
  };

  return {
    touchedValues,
    values,
    defaultValueColumns,
    handleDefaultValueColumns,
    onColumnUpdate,
    handleNullChecks,
    transformValues,
  };
};

export default useDataRowForm;
