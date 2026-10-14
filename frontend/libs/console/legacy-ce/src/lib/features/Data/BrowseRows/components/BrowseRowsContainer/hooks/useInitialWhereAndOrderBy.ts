import { TableColumn } from '@hasura/metadata/data-source';
import { Table } from '@hasura/shared/types';
import { getLSItem, setLSItem } from '@hasura/shared/utils';
import { useEffect, useMemo, useState } from 'react';
import { DataGridOptions } from '../../DataGrid/DataGrid';
import { convertUrlToDataGridOptions } from '../../DataGrid/DataGrid.utils';

type GetUniqueTableKeyProps = {
  table: Table;
  dataSourceName: string;
};

export const getUniqueTableKey = ({
  table,
  dataSourceName,
}: GetUniqueTableKeyProps) => {
  if (Array.isArray(table)) {
    return `${dataSourceName}.${table.join('-')}.query`;
  }

  // object tables (e.g. `{ schema, name }`) must be serialized, otherwise all
  // of them share the same `[object Object]` key
  return `${dataSourceName}.${JSON.stringify(table)}.query`;
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
  const localStorageWhereAndOrderBy = getWhereAndOrderByFromLocalStorage({
    table,
    dataSourceName,
  });
  const [initialWhereAndOrderBy] = useState(localStorageWhereAndOrderBy);

  const [initialUrlSearchParams] = useState(window.location.search);

  /**
   * The initial grid options are fully derived from the (async) columns, the
   * URL search params captured on mount, and the persisted local-storage value.
   * Because this is a pure computation it lives in render (via useMemo) instead
   * of being mirrored into state through an effect. `shouldPersist` records
   * whether the derived options came from the URL and therefore need to be
   * written back to local storage (a side effect handled below).
   */
  const initialOptions = useMemo<{
    options: DataGridOptions | undefined;
    shouldPersist: boolean;
  }>(() => {
    if (!columns) {
      return { options: undefined, shouldPersist: false };
    }

    if (initialUrlSearchParams) {
      const newOptions = convertUrlToDataGridOptions(
        initialUrlSearchParams,
        columns,
      );

      const hasWhere = newOptions.where && newOptions.where?.length > 0;
      const hasOrderBy = newOptions.order_by && newOptions.order_by?.length > 0;

      if (hasWhere || hasOrderBy) {
        return { options: newOptions, shouldPersist: true };
      }
    }

    if (initialWhereAndOrderBy) {
      return {
        options: {
          where: initialWhereAndOrderBy.where,
          order_by: initialWhereAndOrderBy.order_by,
        },
        shouldPersist: false,
      };
    }

    return { options: undefined, shouldPersist: false };
  }, [columns, initialUrlSearchParams, initialWhereAndOrderBy]);

  const options = initialOptions.options;

  // Persisting to local storage is a side effect, so it stays in an effect.
  useEffect(() => {
    if (initialOptions.shouldPersist && initialOptions.options) {
      setWhereAndOrderByToLocalStorage({
        table,
        dataSourceName,
        whereAndOrderBy: {
          where: initialOptions.options.where || [],
          order_by: initialOptions.options.order_by || [],
        },
      });
    }
  }, [initialOptions, table, dataSourceName]);

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
