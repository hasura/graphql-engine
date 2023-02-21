import React from 'react';
import { fetchFiltersData } from './graphql.queries';
import StatsPanel from '../StatsPanel/StatsPanel';
import { updateQsHistory, transformToTypeValueArr } from '../utils';
import { retrieveFilterData, retrieveDefaultDropdownOptions } from './utils';
import BrowseRows from './BrowseRows';
import { applyFilterByQueryParam } from '../utils';
import { Analytics, REDACT_EVERYTHING } from '@hasura/console-legacy-ce';

import {
  TITLE_MAP,
  NO_TITLE_MAP,
  singleSelectFilters,
  FILTER_MAP,
} from './constants';

export const Websockets = props => {
  const { queryParams, RenderLink, projectId } = props;

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
          <Analytics name="MonitoringWebsocket" {...REDACT_EVERYTHING}>
            <div className="infoWrapper">
              <BrowseRows
                RenderLink={RenderLink}
                filters={filters}
                groupBys={groups}
                label={'Websockets List'}
                projectId={projectId}
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
  };
};

export const websocketsConnector = connect =>
  connect(mapStateToProps)(Websockets);
