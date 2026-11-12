import { TableRow, TableColumn } from '@hasura/metadata/data-source';
import { RowSelectionState } from '@tanstack/react-table';
import { DataGridOptions } from './DataGrid';
import { WhereClause } from '@hasura/shared/types';
import { isArrayString, isJsonString } from '@hasura/shared/utils';

export type AdaptSelectedRowIdsToWhereClauseArgs = {
  rowsId: RowSelectionState;
  rows: TableRow[] | undefined;
  primaryKeys: string[];
};

export const adaptSelectedRowIdsToWhereClause = ({
  rowsId,
  rows,
  primaryKeys,
}: AdaptSelectedRowIdsToWhereClauseArgs): WhereClause[] => {
  const selectedRows = (rows || []).filter(
    (row, index) => rowsId[index] === true,
  );

  const whereClause: WhereClause[] = selectedRows.map((row) => {
    return primaryKeys.reduce((acc, primaryKey) => {
      acc[primaryKey] = {
        _eq: row[primaryKey],
      };

      return acc;
    }, {} as WhereClause);
  });

  return whereClause;
};

/**
 * Filters are GraphQL boolean expressions, but links and saved queries from
 * older consoles use the RQL operators (`$eq`). `$ne` is the only one that
 * isn't a plain rename.
 */
export const fromLegacyOperator = (operator: string) => {
  if (operator === '$ne') return '_neq';
  return operator.startsWith('$') ? `_${operator.slice(1)}` : operator;
};

/** Renames the legacy RQL operators of saved filters, e.g. `$eq` -> `_eq`. */
export const fromLegacyWhere = (
  where: DataGridOptions['where'],
): DataGridOptions['where'] =>
  where?.map((condition) =>
    Object.fromEntries(
      Object.entries(condition).map(([column, comparison]) => [
        column,
        Object.fromEntries(
          Object.entries(comparison).map(([operator, value]) => [
            fromLegacyOperator(operator),
            value,
          ]),
        ),
      ]),
    ),
  );

export type FilterConditions = ({ filter: string } | { sort: string })[];

export const mapWhereAndSortConditions = (
  options: DataGridOptions,
): FilterConditions => {
  const { where = [], order_by = [] } = options;

  const whereQueryString = where.map((whereCondition) => {
    const columnName = Object.keys(whereCondition)[0];
    const operator = Object.keys(whereCondition[columnName])[0];
    const value = whereCondition[columnName][operator];
    // a list (`_in`) is kept as JSON, so it reads back as a list
    const valueText = typeof value === 'string' ? value : JSON.stringify(value);

    const filterQueryString = `${columnName};${operator};${valueText}`;

    return { filter: filterQueryString };
  });

  const orderQueryString = order_by.map((orderCondition) => {
    const columnName = orderCondition.column;
    const sortOrder = orderCondition.type;

    const sortQueryString = `${columnName};${sortOrder}`;

    return { sort: sortQueryString };
  });

  return [...whereQueryString, ...orderQueryString];
};

export const replaceFiltersInUrl = (
  currentSearch: string,
  newFilterConditions: FilterConditions,
): string => {
  const searchParams = new URLSearchParams(currentSearch);
  searchParams.delete('filter');
  searchParams.delete('sort');

  newFilterConditions.forEach((newFilterCondition) => {
    if ('filter' in newFilterCondition) {
      searchParams.append('filter', newFilterCondition.filter);
    }

    if ('sort' in newFilterCondition) {
      searchParams.append('sort', newFilterCondition.sort);
    }
  });

  return searchParams.toString();
};

type Args = {
  options: DataGridOptions;
  search: string;
};

export const applyWhereAndSortConditionsToQueryString = ({
  options,
  search,
}: Args) => {
  const whereAndSortMap = mapWhereAndSortConditions(options);
  const searchQueryString = replaceFiltersInUrl(search, whereAndSortMap);
  return searchQueryString;
};

export const convertValueToGraphQL = (
  value: string,
  column: TableColumn,
): number | string | boolean => {
  const scalarType = column.graphQLProperties?.scalarType || column.dataType;

  if (value.includes('[')) {
    const values = value.replace('[', '').replace(']', '').split(',');
    if (scalarType === 'decimal' || scalarType === 'float') {
      return JSON.stringify(values.map((_value) => parseFloat(_value)));
    }

    if (scalarType === 'int') {
      return JSON.stringify(values.map((_value) => parseInt(_value, 10)));
    }

    if (scalarType === 'string') {
      return JSON.stringify(values.map((_value) => _value.trim().toString()));
    }
  }

  if (scalarType === 'decimal' || scalarType === 'float') {
    return parseFloat(value);
  }

  if (scalarType === 'int') {
    return parseInt(value, 10);
  }

  if (scalarType === 'boolean') {
    return value === 'true';
  }

  return value;
};

export const convertUrlToDataGridOptions = (
  search: string,
  columns: TableColumn[] | undefined = [],
): DataGridOptions => {
  const searchParams = new URLSearchParams(search);

  const baseOption: DataGridOptions = {
    where: [],
    order_by: [],
  };

  const searchParamsArray: [string, string][] = Array.from(
    searchParams.entries(),
  );

  return searchParamsArray.reduce<DataGridOptions>((acc, value) => {
    const key = value[0];
    if (key === 'database' || key === 'table') {
      return acc;
    }

    if (key === 'filter') {
      const where = acc?.where || [];
      const [columnName, legacyOperator, filterValue] = value[1].split(';');
      const operator = fromLegacyOperator(legacyOperator);

      const column = columns.find((_column) => _column.name === columnName);
      // a JSON list, e.g. the value of `_in`, and a JSON document of a JSON
      // column, e.g. the value of `_contains`, are used as-is
      const isJsonDocument =
        column?.consoleDataType === 'json' &&
        filterValue.trim().startsWith('{') &&
        isJsonString(filterValue);
      const convertedValue =
        isArrayString(filterValue) || isJsonDocument
          ? JSON.parse(filterValue)
          : column
            ? convertValueToGraphQL(filterValue, column)
            : filterValue;

      return {
        ...acc,
        where: [
          ...where,
          {
            [columnName]: {
              [operator]: convertedValue,
            },
          },
        ],
      };
    }

    if (key === 'sort') {
      const order_by = acc?.order_by || [];
      const [columnName, orderType] = value[1].split(';');
      if (orderType === 'asc' || orderType === 'desc') {
        return {
          ...acc,
          order_by: [
            ...order_by,
            {
              column: columnName,
              type: orderType,
            },
          ],
        };
      }
    }

    return acc;
  }, baseOption);
};
