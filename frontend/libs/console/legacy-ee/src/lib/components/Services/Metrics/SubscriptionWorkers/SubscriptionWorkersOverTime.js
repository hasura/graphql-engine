import React, { useState } from 'react';

import { useQuery } from '@apollo/react-hooks';

import {
  fetchExecutionTimeOvertime,
  fetchPostgresQueriesOvertime,
  fetchSubscribersOvertime,
} from './graphql.queries';

import { filterByType, getTimeRangeValue, getTimeInterval } from './utils';
import { removeSecondsMiliseconds, getWhereClause } from '../Error/utils';
import SWOverTimeChart from './SWOverTimeChart';

import { FILTER_MAP } from './constants';

import { TIME_RANGE_SYMBOL } from '../constants';
import { getTimeIntervalFromRange } from '../utils';
import moment from 'moment';

import styles from '../Metrics.module.scss';

const SubscriptionWorkersOverTime = props => {
  const defaultState = {
    now: new Date().toISOString(),
  };

  const [chartState] = useState(defaultState);
  const { filters, projectId, rowData } = props;

  const args = {
    from_time: getTimeRangeValue(),
    to_time: chartState.now,
    operation_ids: [],
    operation_names: [],
    // operation_types: [],
    user_roles: [],
    time_interval: getTimeInterval(),
  };

  const getFilterObj = getWhereClause(FILTER_MAP, filters);

  const rawTimeRangeFilter = filterByType(filters, TIME_RANGE_SYMBOL);
  const timeRangeFilter =
    rowData && rowData.start && rowData.end
      ? [
          {
            value: {
              start: rowData.start,
              end: moment(rowData.end).add(1, 'minute'), // exclude case start = end
            },
          },
        ]
      : rawTimeRangeFilter;

  timeRangeFilter.forEach(e => {
    if (typeof e.value === 'string') {
      args.from_time = getTimeRangeValue(e.value);
      args.time_interval = getTimeInterval(e.value);
    } else {
      const fDate = new Date(e.value.start);
      const toDate = new Date(e.value.end);
      args.from_time = removeSecondsMiliseconds(fDate).toISOString();
      args.to_time = removeSecondsMiliseconds(toDate).toISOString();
      args.time_interval = getTimeIntervalFromRange(e.value.start, e.value.end);
    }
  });

  // selected row's poller_id is highest priority
  const pollerIds =
    rowData && rowData.pollerId
      ? `{${rowData.pollerId}}`
      : getFilterObj.poller_ids;

  const vars = {
    // operation_names: getFilterObj.operation_name,
    project_ids: '{}',
    from_time: args.from_time,
    to_time: args.to_time,
    status: getFilterObj.status,
    poller_ids: pollerIds,
    time_interval: args.time_interval,
  };

  if (projectId) {
    vars.project_ids = `{${projectId}}`;
  }

  const {
    loading: executionTimeLoading,
    error: executionTimeError,
    data: executionTimeData,
  } = useQuery(fetchExecutionTimeOvertime, {
    variables: vars,
  });

  const {
    loading: postgresLoading,
    error: postgresError,
    data: postgresData,
  } = useQuery(fetchPostgresQueriesOvertime, {
    variables: vars,
  });

  const {
    loading: subscribersLoading,
    error: subscribersError,
    data: subscribersData,
  } = useQuery(fetchSubscribersOvertime, {
    variables: vars,
  });

  const getExecutionTimeData = () => {
    return executionTimeData.search_average_poller_execution_time_over_time;
  };

  const getPostgresQueriesData = () => {
    return postgresData.search_total_subscription_postgres_queries_over_time;
  };

  const getSubscribersData = () => {
    return subscribersData.search_total_subscription_subcribers_over_time;
  };

  if (executionTimeLoading || postgresLoading || subscribersLoading) {
    return 'Loading ...';
  }
  if (executionTimeError || postgresError || subscribersError) {
    return 'Error fetching';
  }

  return (
    <div className={'col-md-12 ' + styles.noPadd}>
      <div className={styles.chartBox}>
        <SWOverTimeChart
          executionTimeData={getExecutionTimeData()}
          postgresData={getPostgresQueriesData()}
          subscribersData={getSubscribersData()}
        />
      </div>
    </div>
  );
};

export default SubscriptionWorkersOverTime;
