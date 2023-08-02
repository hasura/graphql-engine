import React, { useState } from 'react';
import { Analytics, REDACT_EVERYTHING } from '@hasura/console-legacy-ce';

import StatsPanel from '../StatsPanel/StatsPanel';
import {
  applyFilterByQueryParam,
  transformToTypeValueArr,
  updateQsHistory,
} from '../utils';
import BrowseRows from './BrowseRows';
import {
  FILTER_MAP,
  NO_TITLE_MAP,
  singleSelectFilters,
  TITLE_MAP,
} from './constants';
import { fetchFiltersData } from './graphql.queries';
import SubscriptionWorkersOverTime from './SubscriptionWorkersOverTime';
import { retrieveDefaultDropdownOptions, retrieveFilterData } from './utils';

export const SubscriptionWorkers = props => {
  const { RenderLink, projectId, dispatch, queryParams, privileges } = props;
  const [rowData, setRowData] = useState(null);

  const onFilterChangeCb = nextFilters => {
    if (nextFilters.length > 0) {
      const qs = `?filters=${window.encodeURI(JSON.stringify(nextFilters))}`;
      updateQsHistory(qs);
    } else {
      updateQsHistory();
    }
  };

  const getTitle = value => {
    return TITLE_MAP[value];
  };

  const getEmptyTitle = value => {
    return NO_TITLE_MAP[value];
  };

  const updateRowData = row => {
    const rowInfo = {};
    if (row) {
      rowInfo.end = row.last_activity_time;
      rowInfo.start = row.started;
      rowInfo.pollerId = row.worker_id;
      setRowData(rowInfo);
    } else {
      setRowData(null);
    }
  };

  const filtersData = transformToTypeValueArr(FILTER_MAP);

  return (
    <StatsPanel
      singleSelectFilters={singleSelectFilters}
      getTitle={getTitle}
      getEmptyTitle={getEmptyTitle}
      filters={filtersData}
      initialFiltersState={queryParams.parsedFilter}
      retrieveFilterData={retrieveFilterData}
      onFilterChangeCb={onFilterChangeCb}
      retrieveDefaultDropdownOptions={retrieveDefaultDropdownOptions}
      query={fetchFiltersData}
      projectId={projectId}
    >
      {({ filters, groups }) => {
        return (
          <Analytics
            name="MonitoringSubscriptionWorkers"
            {...REDACT_EVERYTHING}
          >
            <div className="infoWrapper">
              <SubscriptionWorkersOverTime
                filters={filters}
                projectId={projectId}
                rowData={rowData}
              />
              <BrowseRows
                RenderLink={RenderLink}
                filters={filters}
                groupBys={groups}
                label={'Workers'}
                projectId={projectId}
                dispatch={dispatch}
                updateRowData={updateRowData}
                privileges={privileges}
              />
            </div>
          </Analytics>
        );
      }}
    </StatsPanel>
  );
};

const mapStateToProps = (state, ownProps) => {
  const { location } = ownProps;
  const { search } = location;
  const project = state.main.project;
  const preAppliedFilter = applyFilterByQueryParam(search, true);
  const idToken = state.main.oAuthResponse.id_token;
  const accessToken = state.main.oAuthResponse.access_token;
  return {
    ...ownProps,
    queryParams: preAppliedFilter,
    projectId: project.id,
    idToken,
    accessToken,
    privileges: project.privileges,
  };
};

export const subscriptionWorkersConnector = connect =>
  connect(mapStateToProps)(SubscriptionWorkers);
