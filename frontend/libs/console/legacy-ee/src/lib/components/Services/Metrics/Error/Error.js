import React from 'react';

import { fetchFiltersData } from './graphql.queries';

import {
  DEFAULT_GROUP_BY,
  FILTER_MAP,
  singleSelectFilters,
  GROUP_BY_COLUMNS,
} from './constants';
import { TITLE_MAP, NO_TITLE_MAP } from '../constants';

import {
  transformToTypeValueArr,
  applyFilterByQueryParam,
  retrieveFilterData,
  retrieveDefaultDropdownOptions,
  onFilterChangeCb,
  onGroupByChangeCb,
} from '../utils';

import StatsPanel from '../StatsPanel/StatsPanel';

import BrowserRows from './BrowseRows';

import ErrorsOverTime from './ErrorsOverTime';

import styles from '../Metrics.module.scss';

export const Error = props => {
  const { queryParams, projectId, RenderLink } = props;
  // const { preFilters } = props;

  const getTitle = value => {
    return TITLE_MAP[value];
  };

  const getEmptyTitle = value => {
    return NO_TITLE_MAP[value];
  };

  const filtersData = transformToTypeValueArr(FILTER_MAP);

  return (
    <StatsPanel
      projectId={projectId}
      singleSelectFilters={singleSelectFilters}
      groupByCols={GROUP_BY_COLUMNS}
      getTitle={getTitle}
      getEmptyTitle={getEmptyTitle}
      initialGroupBysState={queryParams.parsedGroupbys}
      filters={filtersData}
      initialFiltersState={queryParams.parsedFilter}
      retrieveFilterData={retrieveFilterData}
      onFilterChangeCb={onFilterChangeCb}
      retrieveDefaultDropdownOptions={retrieveDefaultDropdownOptions}
      query={fetchFiltersData}
      onGroupByChangeCb={onGroupByChangeCb}
    >
      {({ filters, groups }) => {
        return (
          <div className="infoWrapper">
            <div className={styles.subHeader}>Errors over time</div>
            <ErrorsOverTime
              filters={filters}
              groupBys={groups}
              projectId={projectId}
            />
            <BrowserRows
              RenderLink={RenderLink}
              filters={filters}
              groupBys={groups}
              projectId={projectId}
              label={'Frequent errors'}
            />
          </div>
        );
      }}
    </StatsPanel>
  );
};

const mapStateToProps = (state, ownProps) => {
  const { location } = ownProps;
  const { search } = location;
  const preAppliedFilter = applyFilterByQueryParam(
    search,
    true,
    DEFAULT_GROUP_BY
  );
  const project = state.main.project;
  return {
    ...ownProps,
    queryParams: preAppliedFilter,
    projectId: project.id,
  };
};

const errorConnector = connect => connect(mapStateToProps)(Error);
export default errorConnector;
