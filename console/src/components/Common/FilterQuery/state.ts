import React from 'react';
import {
  OrderBy,
  TableDefinition,
  getSelectQuery,
} from '../utils/v1QueryUtils';
import requestAction from '../../../utils/requestAction';
import endpoints from '../../../Endpoints';
import {
  FilterState,
  ValueFilter,
  Filter,
  parseFilter,
  RunQuery,
} from './utils';

const defaultFilter: ValueFilter = {
  kind: 'value',
  key: '',
  value: '',
  operator: '$eq',
};
const defaultSort: OrderBy = {
  column: '',
  type: 'asc',
};
const defaultState: FilterState = {
  filters: [defaultFilter],
  sorts: [defaultSort],
  limit: 10,
  offset: 0,
};

export const useFilterQuery = (
  table: TableDefinition,
  dispatch: any,
  presets: {
    filters: Filter[];
    sorts: OrderBy[];
  },
  relationships?: string[]
) => {
  const [state, setState] = React.useState(defaultState);
  const [rows, setRows] = React.useState<any[]>([]);
  const [count, setCount] = React.useState<number>();
  const [loading, setLoading] = React.useState(false);
  const [error, setError] = React.useState(false);

  const runQuery: RunQuery = (offset, limit) => {
    console.log('Making query with');
    console.log(offset === undefined ? state.offset : offset);
    console.log(limit === undefined ? state.limit : limit);
    setLoading(true);
    setError(false);

    const where = {
      $and: [...state.filters, ...presets.filters]
        .filter(f => !!f.key && !!f.value)
        .map(f => parseFilter(f)),
    };

    const orderBy = [...state.sorts.filter(f => !!f.column), ...presets.sorts];

    const query = getSelectQuery(
      'select',
      table,
      ['*', ...(relationships || []).map(r => ({ name: r, columns: ['*'] }))],
      where,
      offset === undefined ? state.offset : offset,
      limit === undefined ? state.limit : limit,
      orderBy
    );
    const countQuery = getSelectQuery(
      'count',
      table,
      ['*', ...(relationships || []).map(r => ({ name: r, columns: ['*'] }))],
      where,
      undefined,
      undefined,
      orderBy
    );

    const options = {
      method: 'POST',
      body: JSON.stringify(query),
    };

    dispatch(
      requestAction(endpoints.query, options, undefined, undefined, true, true)
    ).then(
      (data: any[]) => {
        setRows(data);
        setLoading(false);
        if (offset !== undefined) {
          setState(s => ({ ...s, offset }));
        }
        if (limit !== undefined) {
          setState(s => ({ ...s, limit }));
        }
        dispatch(
          requestAction(
            endpoints.query,
            {
              method: 'POST',
              body: JSON.stringify(countQuery),
            },
            undefined,
            undefined,
            true,
            true
          )
        ).then((countData: any) => {
          setCount(countData.count);
        });
      },
      () => {
        setError(true);
        setLoading(false);
      }
    );
  };

  React.useEffect(() => {
    runQuery();
  }, []);

  return {
    rows,
    loading,
    error,
    runQuery,
    state,
    count,
    setState: {
      sorts: (sorts: OrderBy[]) => {
        const newSorts = [...sorts];
        if (!sorts.length || sorts[sorts.length - 1].column) {
          newSorts.push(defaultSort);
        }
        setState(s => ({
          ...s,
          sorts: newSorts,
        }));
      },
      filters: (filters: ValueFilter[]) => {
        const newFilters = [...filters];
        if (
          !filters.length ||
          filters[filters.length - 1].value ||
          filters[filters.length - 1].key
        ) {
          newFilters.push(defaultFilter);
        }
        setState(s => ({
          ...s,
          filters: newFilters,
        }));
      },
      offset: (o: number) => {
        setState(s => ({
          ...s,
          offset: o,
        }));
      },
      limit: (l: number) => {
        setState(s => ({
          ...s,
          limit: l,
        }));
      },
    },
  };
};
