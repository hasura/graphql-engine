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
import { QualifiedTable } from '../../../metadata/types';
import {
  getScheduledEvents,
  getEventInvocations,
} from '../../../metadata/queryUtils';
import { EventKind } from '../../Services/Events/types';
import { isNotDefined } from '../utils/jsUtils';
import { getDataTriggerLogsQuery } from '../../../metadata/metadataTableUtils';

const defaultFilter = makeValueFilter('', null, '');
const defaultSort = makeOrderBy('', 'asc');
const defaultState = makeFilterState([defaultFilter], [defaultSort], 10, 0);

export type TriggerOperation = 'pending' | 'processed' | 'invocation';

export const useFilterQuery = (
  table: QualifiedTable,
  dispatch: Dispatch,
  presets: {
    filters: Filter[];
    sorts: OrderBy[];
  },
  relationships: Nullable<string[]>,
  triggerOp: TriggerOperation,
  triggerType: EventKind,
  triggerName?: string,
  currentSource?: string
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

    const offsetValue = isNotDefined(offset) ? state.offset : offset;
    const limitValue = isNotDefined(limit) ? state.limit : limit;

    let query = {};
    let endpoint = endpoints.metadata;

    if (triggerType === 'scheduled') {
      if (triggerOp !== 'invocation') {
        query = getScheduledEvents(
          'one_off',
          limitValue ?? 10,
          offsetValue ?? 0,
          triggerOp
        );
      } else {
        query = getEventInvocations(
          'one_off',
          limitValue ?? 10,
          offsetValue ?? 0
        );
      }
    } else if (triggerType === 'cron') {
      if (triggerOp !== 'invocation') {
        query = getScheduledEvents(
          'cron',
          limitValue ?? 10,
          offsetValue ?? 0,
          triggerOp,
          triggerName
        );
      } else {
        query = getEventInvocations(
          'cron',
          limitValue ?? 10,
          offsetValue ?? 0,
          triggerName
        );
      }
    } else if (triggerType === 'data') {
      endpoint = endpoints.query;
      if (triggerName) {
        query = getDataTriggerLogsQuery(
          triggerOp,
          currentSource || 'default',
          triggerName,
          limitValue,
          offsetValue
        );
      } else {
        return; // fixme: this should just be an error saying that there's no trigger name provided
      }
    }

    const options = {
      method: 'POST',
      body: JSON.stringify(query),
    };

    dispatch(
      requestAction(endpoint, options, undefined, undefined, true, true)
    ).then(
      (data: any) => {
        if (triggerType === 'data') {
          // formatting of the data
          const allKeys = data.result[0];
          const resultsData = data.result.slice(1);
          const formattedData: Record<string, any>[] = [];
          resultsData.forEach((values: string[]) => {
            const dataObj: Record<string, any> = {};
            allKeys.forEach((key: string, idx: number) => {
              if (!dataObj[key]) {
                // to avoid duplicate keys in the results
                if (triggerOp !== 'invocation' && key === 'event_id') {
                  dataObj.invocation_id = dataObj.id;
                  dataObj.id = values[idx];
                } else if (key === 'request' || key === 'response') {
                  try {
                    dataObj[key] = JSON.parse(values[idx]);
                  } catch {
                    dataObj[key] = values[idx];
                  }
                } else {
                  dataObj[key] = values[idx];
                }
              }
            });
            formattedData.push(dataObj);
          });

          if (limitValue && offsetValue) {
            setRows(formattedData.slice(offsetValue, offsetValue + limitValue));
          } else {
            setRows(formattedData);
          }
          setCount(formattedData.length);
        } else if (triggerOp !== 'invocation') {
          setRows(data?.events ?? []);
        } else {
          setRows(data?.invocations ?? []);
        }

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
        if (triggerType !== 'data') {
          setCount(data?.count ?? 10);
        }
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
