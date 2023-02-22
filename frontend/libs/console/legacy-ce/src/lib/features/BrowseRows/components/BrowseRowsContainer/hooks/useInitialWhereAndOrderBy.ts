import { TableColumn } from '../../../../DataSource';
import { Table } from '../../../../hasura-metadata-types';
import { getLSItem, setLSItem } from '../../../../../utils';
import { useEffect, useState } from 'react';
import { DataGridOptions } from '../../DataGrid/DataGrid';
import { convertUrlToDataGridOptions } from '../../DataGrid/DataGrid.utils';

type GetUniqueTableKeyProps = {
  table: Table;
  dataSourceName: string;
};

const getUniqueTableKey = ({
  table,
  dataSourceName,
}: GetUniqueTableKeyProps) => {
  if (Array.isArray(table)) {
    return `${dataSourceName}.${table.join('-')}.query`;
  }

  return `${dataSourceName}.${table}.query`;
};

type WhereAndOrderBy = Pick<DataGridOptions, 'where' | 'order_by'>;

const getWhereAndOrderByFromLocalStorage = ({
  table,
  dataSourceName,
}: GetUniqueTableKeyProps): WhereAndOrderBy | undefined => {
  const localStorageKey = getUniqueTableKey({ table, dataSourceName });
  const localUserQueryString = localStorageKey
    ? getLSItem(localStorageKey)
    : '';

  if (localUserQueryString) {
    return JSON.parse(localUserQueryString) as WhereAndOrderBy;
  }

  return undefined;
};

type SetWhereAndOrderByToLocalStorage = {
  whereAndOrderBy: WhereAndOrderBy;
} & GetUniqueTableKeyProps;

const setWhereAndOrderByToLocalStorage = ({
  table,
  dataSourceName,
  whereAndOrderBy,
}: SetWhereAndOrderByToLocalStorage) => {
  const localStorageKey = getUniqueTableKey({ table, dataSourceName });
  setLSItem(localStorageKey, JSON.stringify(whereAndOrderBy));
};

type UseInitialWhereAndOrderByProps = {
  columns: TableColumn[] | undefined;
  table: Table;
  dataSourceName: string;
};

export const useInitialWhereAndOrderBy = ({
  columns,
  table,
  dataSourceName,
}: UseInitialWhereAndOrderByProps) => {
  const [options, setOptions] = useState<DataGridOptions | undefined>(
    undefined
  );

  const localStorageWhereAndOrderBy = getWhereAndOrderByFromLocalStorage({
    table,
    dataSourceName,
  });
  const [initialWhereAndOrderBy] = useState(localStorageWhereAndOrderBy);

  const [initialUrlSearchParams] = useState(window.location.search);

  useEffect(() => {
    if (columns) {
      if (initialUrlSearchParams) {
        const newOptions = convertUrlToDataGridOptions(
          initialUrlSearchParams,
          columns
        );

        const hasWhere = newOptions.where && newOptions.where?.length > 0;
        const hasOrderBy =
          newOptions.order_by && newOptions.order_by?.length > 0;

        if (hasWhere || hasOrderBy) {
          setWhereAndOrderByToLocalStorage({
            table,
            dataSourceName,
            whereAndOrderBy: {
              where: newOptions?.where || [],
              order_by: newOptions?.order_by || [],
            },
          });
          setOptions(newOptions);
          return;
        }
      }

      if (initialWhereAndOrderBy) {
        const newOptions: DataGridOptions = {
          where: initialWhereAndOrderBy.where,
          order_by: initialWhereAndOrderBy.order_by,
        };
        setOptions(newOptions);
      }
    }
  }, [columns]);

  const onUpdateOptions = (_options: DataGridOptions) => {
    const whereAndOrderBy: WhereAndOrderBy = {
      where: _options?.where || [],
      order_by: _options?.order_by || [],
    };
    setWhereAndOrderByToLocalStorage({
      table,
      dataSourceName,
      whereAndOrderBy,
    });
  };

  return {
    options,
    onUpdateOptions,
  };
};
