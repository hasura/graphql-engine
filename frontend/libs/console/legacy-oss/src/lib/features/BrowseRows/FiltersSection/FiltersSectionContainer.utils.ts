import { SelectItem } from '@/components/Common/SelectInputSplitField/SelectInputSplitField';
import { TableColumn } from '@/features/DataSource';
import { createHistory } from 'history';
import { ThunkDispatch } from 'redux-thunk';
import { AnyAction } from 'redux';
import { ReduxState } from '@/types';
import { TableSchema } from '@/components/Services/Data/TableBrowseRows/utils';
import { Integers, Reals } from '../../../components/Services/Data/constants';
import { vMakeTableRequests } from '../../../components/Services/Data/TableBrowseRows/ViewActions';
import { sortPlaceholder } from './FiltersSection';
import { FiltersAndSortFormValues, OrderCondition, UserQuery } from './types';

export const columnPlaceholder = '-- column --';

const convertArray = (arrayString: string): number[] | string[] =>
  JSON.parse(arrayString);

const convertValue = (
  value: string | string[] | number[],
  tableColumnType: string
) => {
  if (Array.isArray(value)) {
    return value;
  }

  if (tableColumnType === 'integer') {
    return parseInt(value, 10);
  }

  if (tableColumnType === 'boolean') {
    return Boolean(value);
  }

  return value;
};

export const adaptFormValuesToQuery = (
  formValues: FiltersAndSortFormValues,
  columnDataTypes: TableColumn[]
): UserQuery => {
  const where =
    formValues?.filter?.map(filter => {
      const columnDataType = columnDataTypes.find(
        col => col.name === filter.column
      );

      let partialValue: string | string[] | number[] = filter.value;
      if (filter.operator === '$in' || filter.operator === '$nin') {
        partialValue = convertArray(filter.value);
      }

      const value = !columnDataType
        ? filter.value
        : convertValue(partialValue, columnDataType.dataType);

      return {
        [filter.column]: {
          [filter.operator]: value,
        },
      };
    }) ?? [];

  const orderBy =
    formValues?.sort?.map(order => {
      const orderCondition: OrderCondition = {
        column: order.column,
        type: order.order,
        nulls: 'last',
      };
      return orderCondition;
    }) ?? [];

  return {
    where: {
      $and: where,
    },
    order_by: orderBy,
  };
};

type SetParamsArgs = {
  filters: string[];
  sorts: string[];
};

const setParams = (query: SetParamsArgs = { filters: [], sorts: [] }) => {
  const searchParams = new URLSearchParams();
  query.filters.forEach(filter => searchParams.append('filter', filter));
  query.sorts.forEach(sort => searchParams.append('sort', sort));
  return searchParams.toString();
};

export const setUrlParams = (
  whereAnd: UserQuery['where']['$and'],
  orderBy: UserQuery['order_by']
) => {
  const sorts = orderBy
    .filter(order => order.column)
    .map(order => `${order.column};${order.type}`);
  const filters = whereAnd
    .filter(
      where => Object.keys(where).length === 1 && Object.keys(where)[0] !== ''
    )
    .map(where => {
      const col = Object.keys(where)[0];
      const op = Object.keys(where[col])[0];
      const value = where[col][op];
      return `${col};${op};${value}`;
    });
  const url = setParams({ filters, sorts });
  const history = createHistory();

  history.push({
    pathname: history.getCurrentLocation().pathname,
    search: `?${url}`,
  });
};

const parseArray = (val: string | number | boolean | string[] | number[]) => {
  if (Array.isArray(val)) return val;
  if (typeof val === 'string') {
    try {
      return JSON.parse(val);
    } catch (err) {
      return '';
    }
  }

  return val;
};

const defaultColumns: SelectItem[] = [
  {
    label: columnPlaceholder,
    value: columnPlaceholder,
    disabled: true,
  },
];

export const getColumns = (columns: string[]) => {
  const columnsSelectItems: SelectItem[] = columns.map((columnName: string) => {
    return {
      label: columnName,
      value: columnName,
    };
  });

  return defaultColumns.concat(columnsSelectItems);
};

export const filterValidUserQuery = (userQuery: UserQuery): UserQuery => {
  const filteredWhere = userQuery.where.$and.filter(
    condition => Object.keys(condition)[0] !== columnPlaceholder
  );
  const filteredOrderBy = userQuery.order_by.filter(
    order => order.type !== sortPlaceholder
  );
  return {
    ...userQuery,
    where: { $and: filteredWhere },
    order_by: filteredOrderBy,
  };
};

type RunFilterQuery = {
  tableSchema: TableSchema;
  whereAnd: UserQuery['where']['$and'];
  orderBy: UserQuery['order_by'];
  limit: number;
  offset: number;
};

export const runFilterQuery =
  ({ tableSchema, whereAnd, orderBy, limit, offset }: RunFilterQuery) =>
  (dispatch: ThunkDispatch<ReduxState, unknown, AnyAction>) => {
    const whereClauses = whereAnd.filter(w => {
      const colName = Object.keys(w)[0].trim();
      if (colName === '') {
        return false;
      }
      const opName = Object.keys(w[colName])[0].trim();
      if (opName === '') {
        return false;
      }
      return true;
    });
    const finalWhereClauses = whereClauses.map(whereClause => {
      const colName = Object.keys(whereClause)[0];
      const opName = Object.keys(whereClause[colName])[0];
      const val = whereClause[colName][opName];

      if (['$in', '$nin'].includes(opName)) {
        whereClause[colName][opName] = parseArray(val);
        return whereClause;
      }

      const colType =
        tableSchema?.columns?.find(column => column.column_name === colName)
          ?.data_type || '';

      if (Integers.indexOf(colType) > 0 && typeof val === 'string') {
        whereClause[colName][opName] = parseInt(val, 10);
        return whereClause;
      }
      if (Reals.indexOf(colType) > 0 && typeof val === 'string') {
        whereClause[colName][opName] = parseFloat(val);
        return whereClause;
      }
      if (colType === 'boolean') {
        if (val === 'true') {
          whereClause[colName][opName] = true;
          return whereClause;
        }
        if (val === 'false') {
          whereClause[colName][opName] = false;
          return whereClause;
        }
      }
      return whereClause;
    });
    const newQuery = {
      where: { $and: finalWhereClauses },
      limit,
      offset,
      order_by: orderBy.filter(w => w.column.trim() !== ''),
    };
    dispatch({ type: 'ViewTable/V_SET_QUERY_OPTS', queryStuff: newQuery });
    dispatch(vMakeTableRequests());
  };
