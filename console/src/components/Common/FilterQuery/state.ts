import React from 'react';
import { OrderBy, makeOrderBy } from '../utils/v1QueryUtils';
import requestAction from '../../../utils/requestAction';
import { Dispatch } from '../../../types';
import endpoints from '../../../Endpoints';
import {
  makeFilterState,
  SetFilterState,
  ValueFilter,
  makeValueFilter,
  Filter,
  RunQuery,
} from './types';

import { Nullable } from '../utils/tsUtils';
import { isNotDefined } from '../utils/jsUtils';
import { parseFilter } from './utils';
import { QualifiedTable } from '../../../metadata/types';
import { getLogSql } from '../../../metadata/metadataTableUtils';

const defaultFilter = makeValueFilter('', null, '');
const defaultSort = makeOrderBy('', 'asc');

const defaultState = makeFilterState([defaultFilter], [defaultSort], 10, 0);

export const useFilterQuery = (
  table: QualifiedTable,
  dispatch: Dispatch,
  presets: {
    filters: Filter[];
    sorts: OrderBy[];
  },
  relationships: Nullable<string[]>
) => {
  const [state, setState] = React.useState(defaultState);
  const [rows, setRows] = React.useState<any[]>([]);
  const [count, setCount] = React.useState<number>();
  const [loading, setLoading] = React.useState(false);
  const [error, setError] = React.useState(false);

  const runQuery: RunQuery = (runQueryOpts = {}) => {
    setLoading(true);
    setError(false);

    const { offset, limit, sorts: newSorts } = runQueryOpts;

    const where = {
      $and: [...state.filters, ...presets.filters]
        .filter(f => !!f.key && !!f.value)
        .map(f => parseFilter(f)),
    };

    // const orderBy = newSorts || [
    //   ...state.sorts.filter(f => !!f.column),
    //   ...presets.sorts,
    // ];

    const offsetValue = isNotDefined(offset) ? state.offset : offset;
    const limitValue = isNotDefined(limit) ? state.limit : limit;

    const query = getLogSql(
      'select',
      where.$and[0].cron_event.trigger_name.$eq ?? '',
      table,
      relationships ?? [],
      limitValue ?? 10,
      offsetValue ?? 0
    );

    const countQuery = getLogSql(
      'count',
      where.$and[0].cron_event.trigger_name.$eq ?? '',
      table,
      relationships ?? [],
      undefined,
      undefined
    );

    const options = {
      method: 'POST',
      body: JSON.stringify(query),
    };

    dispatch(
      requestAction(endpoints.query, options, undefined, undefined, true, true)
    ).then(
      (data: any) => {
        const receivedData = data.result.slice(1);
        const formattedData =
          receivedData.map((val: any) => ({
            id: val[0],
            event_id: val[1],
            status: val[2],
            created_at: val[5],
            request: val[3],
            response: val[4],
            tries: val[10],
            next_retry_at: val[12],
          })) ?? [];

        setRows(formattedData);
        setLoading(false);
        if (offset !== undefined) {
          setState(s => ({ ...s, offset }));
        }
        if (limit !== undefined) {
          setState(s => ({ ...s, limit }));
        }
        if (newSorts) {
          setState(s => ({
            ...s,
            sorts: newSorts,
          }));
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
        ).then((countData: { count: number }) => {
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

  const setter: SetFilterState = {
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
  };

  return {
    rows,
    loading,
    error,
    runQuery,
    state,
    count,
    setState: setter,
  };
};
