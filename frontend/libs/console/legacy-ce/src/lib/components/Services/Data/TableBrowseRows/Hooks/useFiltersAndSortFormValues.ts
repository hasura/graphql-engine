import { useMemo } from 'react';
import {
  getFiltersAndSortFromUrlQueryParams,
  UserQuery,
  adaptFormValuesToQuery,
  convertUserQueryToFiltersAndSortFormValues,
} from '../../../../../features/BrowseRows';
import { setLSItem, getLSItem } from '../../../../../utils';
import { Table } from '../../../../../dataSources';
import { TableColumn } from '../../../../../features/DataSource';

type Props = {
  sourceName: string;
  tableSchema: Table | undefined;
};

type SourceNameAndTable = {
  sourceName: string;
  tableSchema: Table | undefined;
};
export const getUniqueTableKey = ({
  sourceName,
  tableSchema,
}: SourceNameAndTable) => {
  if (!tableSchema) {
    return null;
  }

  return `${sourceName}.${tableSchema?.table_schema}.${tableSchema?.table_name}.query`;
};

export const saveUserQueryToLocalStorage = (
  localStorageKey: string,
  newUserQuery: UserQuery
) => {
  setLSItem(localStorageKey, JSON.stringify(newUserQuery));
};

export const defaultUserQuery: UserQuery = {
  where: { $and: [] },
  order_by: [],
};

export const getFiltersAndSortFromLocalStorage = ({
  sourceName,
  tableSchema,
}: SourceNameAndTable): UserQuery => {
  if (!tableSchema) {
    return {
      order_by: [],
      where: { $and: [] },
    };
  }
  const localStorageKey = getUniqueTableKey({ sourceName, tableSchema });
  const localUserQueryString = localStorageKey
    ? getLSItem(localStorageKey)
    : '';

  if (localUserQueryString) {
    return JSON.parse(localUserQueryString) as UserQuery;
  }

  return defaultUserQuery;
};

export const useFiltersAndSortFormValues = ({
  sourceName,
  tableSchema,
}: Props) => {
  const filterAndSortFromQueryParams = getFiltersAndSortFromUrlQueryParams();
  const filtersAndSort = filterAndSortFromQueryParams;

  const initialUserQueryFromUrlParams = useMemo(
    () =>
      adaptFormValuesToQuery(
        filtersAndSort,
        (tableSchema?.columns || []).map(column => ({
          name: column.column_name,
          dataType: column.data_type as TableColumn['dataType'],
          consoleDataType: 'string',
        }))
      ),
    // NOTE: this processing is needed only when the component is loaded for the first time — do not change the dependency array
    []
  );

  const initialUserQuery = initialUserQueryFromUrlParams;

  const onRunQuery = (newUserQuery: UserQuery) => {
    if (!tableSchema) {
      return;
    }

    const uniqueTableKey = getUniqueTableKey({ sourceName, tableSchema });
    if (uniqueTableKey) {
      saveUserQueryToLocalStorage(uniqueTableKey, newUserQuery);
    }
  };

  const areFiltersAndSortsEmpty =
    filterAndSortFromQueryParams.filters.length === 0 &&
    filterAndSortFromQueryParams.sorts.length === 0;

  if (areFiltersAndSortsEmpty) {
    const localUserQuery = getFiltersAndSortFromLocalStorage({
      sourceName,
      tableSchema,
    });

    if (localUserQuery) {
      onRunQuery(localUserQuery);
      const newFiltersAndSort =
        convertUserQueryToFiltersAndSortFormValues(localUserQuery);

      return {
        filtersAndSort: newFiltersAndSort,
        onRunQuery,
        userQuery: localUserQuery,
      };
    }
  }

  return {
    filtersAndSort,
    onRunQuery,
    initialUserQuery,
  };
};
