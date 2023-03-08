import React from 'react';
import { Analytics, REDACT_EVERYTHING } from '@hasura/console-legacy-ce';

import { updateQsHistory, applyFilterByQueryParam } from '../utils';

import { fetchFiltersData } from './graphql.queries';

import BrowserRows from './BrowseRows';

import {
  OPERATIONS_TITLE_MAP,
  OPERATIONS_NO_TITLE_MAP,
  OPERATIONS_FILTER_MAP,
  singleSelectFilters,
} from './constants';

import { retrieveFilterData, retrieveDefaultDropdownOptions } from '../utils';

import StatsPanel from '../StatsPanel/StatsPanel';

import { transformToTypeValueArr } from '../utils';

export const Operations = props => {
  const { queryParams, projectId, projectConfig } = props;

  const onFilterChangeCb = nextFilters => {
    if (nextFilters.length > 0) {
      const qs = `?filters=${window.encodeURI(JSON.stringify(nextFilters))}`;
      updateQsHistory(qs);
    } else {
      updateQsHistory();
    }
  };

  const getTitle = value => {
    return OPERATIONS_TITLE_MAP[value];
  };

  const getEmptyTitle = value => {
    return OPERATIONS_NO_TITLE_MAP[value];
  };

  const filtersData = transformToTypeValueArr(OPERATIONS_FILTER_MAP);

  return (
    <StatsPanel
      projectId={projectId}
      singleSelectFilters={singleSelectFilters}
      getTitle={getTitle}
      getEmptyTitle={getEmptyTitle}
      filters={filtersData}
      initialFiltersState={queryParams.parsedFilter}
      retrieveFilterData={retrieveFilterData}
      onFilterChangeCb={onFilterChangeCb}
      retrieveDefaultDropdownOptions={retrieveDefaultDropdownOptions}
      query={fetchFiltersData}
    >
      {({ filters }) => {
        return (
          <Analytics name="Operations" {...REDACT_EVERYTHING}>
            <div className="infoWrapper">
              <BrowserRows
                filters={filters}
                label={'Operations List'}
                projectId={projectId}
                projectConfig={projectConfig}
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
  const projectConfig = state.metrics.projectConfig;
  return {
    ...ownProps,
    queryParams: preAppliedFilter,
    projectId: project.id,
    projectConfig,
  };
};

const operationsConnector = connect => connect(mapStateToProps)(Operations);

export default operationsConnector;
