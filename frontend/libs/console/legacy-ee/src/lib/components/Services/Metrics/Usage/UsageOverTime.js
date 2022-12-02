import React, { useState } from 'react';

import { useQuery } from '@apollo/react-hooks';

import { searchUsageOverTime } from './graphql.queries';

import UsageOverTimeChart from './UsageOverTimeChart';

import { filterByType, getTimeRangeValue, getTimeInterval } from './utils';
import { removeSecondsMiliseconds, getWhereClause } from '../Error/utils';

import { FILTER_MAP } from './constants';

import { TIME_RANGE_SYMBOL } from '../constants';

import styles from '../Metrics.module.scss';

const UsageOverTime = props => {
  const defaultState = {
    now: new Date().toISOString(),
  };

  const [chartState] = useState(defaultState);
  const { filters, projectId } = props;

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

  const timeRangeFilter = filterByType(filters, TIME_RANGE_SYMBOL);

  timeRangeFilter.forEach(e => {
    args.from_time = getTimeRangeValue(e.value);
    args.time_interval = getTimeInterval(e.value);
  });

  timeRangeFilter.forEach(e => {
    if (typeof e.value === 'string') {
      args.from_time = getTimeRangeValue(e.value);
      args.time_interval = getTimeInterval(e.value);
    } else {
      const fDate = new Date(e.value.start);
      const toDate = new Date(e.value.end);
      args.from_time = removeSecondsMiliseconds(fDate).toISOString();
      args.to_time = removeSecondsMiliseconds(toDate).toISOString();
      args.time_interval = '1 hour';
    }
  });

  const vars = {
    operation_ids: getFilterObj.operation_id,
    operation_names: getFilterObj.operation_name,
    project_ids: '{}',
    user_roles: getFilterObj.user_role,
    client_names: getFilterObj.client_name,
  };

  if (projectId) {
    vars.project_ids = `{${projectId}}`;
  }

  const variables = {
    args: {
      ...args,
      ...vars,
    },
  };

  const { loading, error, data } = useQuery(searchUsageOverTime, {
    variables,
  });

  if (loading) {
    return 'Loading ...';
  }
  if (error) {
    return 'Error fetching';
  }

  return (
    <div className={'col-md-12 ' + styles.noPadd}>
      <div className={styles.chartBox}>
        <UsageOverTimeChart data={data.search_usage_over_time} />
      </div>
    </div>
  );
};

export default UsageOverTime;
