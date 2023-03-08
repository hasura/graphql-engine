import { TableRow, WhereClause, TableColumn } from '../../../DataSource';
import { DataGridOptions } from './DataGrid';

export type AdaptSelectedRowIdsToWhereClauseArgs = {
  rowsId: Record<number, boolean>;
  rows: TableRow[] | undefined;
  primaryKeys: string[];
};

export const adaptSelectedRowIdsToWhereClause = ({
  rowsId,
  rows,
  primaryKeys,
}: AdaptSelectedRowIdsToWhereClauseArgs): WhereClause[] => {
  const selectedRows = (rows || []).filter(
    (row, index) => rowsId[index] === true
  );

  const baseObject: WhereClause[] = primaryKeys.map(primaryKey => ({
    [primaryKey]: { _in: [] },
  }));

  const whereClause: WhereClause[] = selectedRows.reduce((acc, row) => {
    primaryKeys.forEach(primaryKey => {
      // eslint-disable-next-line no-underscore-dangle
      const primaryKeyObject = acc.find(
        val => Object.keys(val)[0] === primaryKey
      );
      if (primaryKeyObject) {
        const value = row[primaryKey];
        // eslint-disable-next-line no-underscore-dangle
        const array = primaryKeyObject[primaryKey]._in as unknown[];

        array.push(value);
      }
    });

    return [...acc];
  }, baseObject);

  return whereClause;
};

export type FilterConditions = ({ filter: string } | { sort: string })[];

export const mapWhereAndSortConditions = (
  options: DataGridOptions
): FilterConditions => {
  const { where = [], order_by = [] } = options;

  const whereQueryString = where.map(whereCondition => {
    const columnName = Object.keys(whereCondition)[0];
    const operator = Object.keys(whereCondition[columnName])[0];
    const value = whereCondition[columnName][operator];

    const filterQueryString = `${columnName};${operator};${value}`;

    return { filter: filterQueryString };
  });

  const orderQueryString = order_by.map(orderCondition => {
    const columnName = orderCondition.column;
    const sortOrder = orderCondition.type;

    const sortQueryString = `${columnName};${sortOrder}`;

    return { sort: sortQueryString };
  });

  return [...whereQueryString, ...orderQueryString];
};

export const replaceFiltersInUrl = (
  currentSearch: string,
  newFilterConditions: FilterConditions
): string => {
  const searchParams = new URLSearchParams(currentSearch);
  searchParams.delete('filter');
  searchParams.delete('sort');

  newFilterConditions.forEach(newFilterCondition => {
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
  column: TableColumn
): number | string | boolean => {
  const scalarType = column.graphQLProperties?.scalarType || column.dataType;

  if (value.includes('[')) {
    const values = value.replace('[', '').replace(']', '').split(',');
    if (scalarType === 'decimal' || scalarType === 'float') {
      return JSON.stringify(values.map(_value => parseFloat(_value)));
    }

    if (scalarType === 'int') {
      return JSON.stringify(values.map(_value => parseInt(_value, 10)));
    }

    if (scalarType === 'string') {
      return JSON.stringify(values.map(_value => _value.trim().toString()));
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
  columns: TableColumn[] | undefined = []
): DataGridOptions => {
  const searchParams = new URLSearchParams(search);

  const baseOption: DataGridOptions = {
    where: [],
    order_by: [],
  };

  const searchParamsArray: [string, string][] = Array.from(
    searchParams.entries()
  );

  return searchParamsArray.reduce<DataGridOptions>((acc, value) => {
    const key = value[0];
    if (key === 'database' || key === 'table') {
      return acc;
    }

    if (key === 'filter') {
      const where = acc?.where || [];
      const [columnName, operator, filterValue] = value[1].split(';');

      const column = columns.find(_column => _column.name === columnName);
      const convertedValue = column
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
