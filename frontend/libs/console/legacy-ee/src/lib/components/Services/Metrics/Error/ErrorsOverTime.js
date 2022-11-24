import React from 'react';

import { useQuery } from '@apollo/react-hooks';

import { searchErrorsOverTime } from './graphql.queries';

import ErrorsOverTimeChart from './ErrorOverTimeChart.js';

import { filterByType, getTimeRangeValue } from './utils';

import { FILTER_MAP } from './constants';

import { TIME_RANGE_SYMBOL } from '../constants';

import { getWhereClause, removeSecondsMiliseconds } from './utils';

import { getTimeInterval } from '../Usage/utils';

import styles from '../Metrics.module.scss';

const ErrorsOverTime = props => {
  /* Get the list of filter types by filtering the type key of the filters list itself */
  const { filters: appliedFilters, projectId } = props;

  const getFilterObj = getWhereClause(FILTER_MAP, appliedFilters);
  let fromTime = getTimeRangeValue();
  let now = new Date();
  now.setSeconds(0, 0);
  let timeInterval = getTimeInterval();

  const timeRangeFilter = filterByType(appliedFilters, TIME_RANGE_SYMBOL);

  timeRangeFilter.forEach(e => {
    if (typeof e.value === 'string') {
      fromTime = getTimeRangeValue(e.value);
      timeInterval = getTimeInterval(e.value);
    } else {
      const fDate = new Date(e.value.start);
      const toDate = new Date(e.value.end);
      fromTime = removeSecondsMiliseconds(fDate).toISOString();
      now = removeSecondsMiliseconds(toDate);
      timeInterval = '1 hour';
    }
  });

  delete getFilterObj.operation_type;

  const variables = {
    fromTime: fromTime,
    toTime: now.toISOString(),
    ...getFilterObj,
    project_id: '{}',
    timeInterval: timeInterval,
  };

  if (projectId) {
    variables.project_id = `{${projectId}}`;
  }

  const { loading, error, data } = useQuery(searchErrorsOverTime, {
    variables: variables,
  });

  if (loading) {
    return 'Loading ...';
  }
  if (error) {
    return 'Error fetching';
  }

  return (
    <div className={'col-md-12'}>
      <div className={styles.chartBox}>
        <ErrorsOverTimeChart data={data.search_errors_over_time} />
      </div>
    </div>
  );
};

export default ErrorsOverTime;
