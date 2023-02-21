import React from 'react';
import { Analytics, REDACT_EVERYTHING } from '@hasura/console-legacy-ce';

import { fetchFiltersData } from '../Error/graphql.queries';
import {
  applyFilterByQueryParam,
  transformToTypeValueArr,
  retrieveFilterData,
  retrieveDefaultDropdownOptions,
  onGroupByChangeCb,
  onFilterChangeCb,
} from '../utils';

import {
  TITLE_MAP,
  NO_TITLE_MAP,
  DEFAULT_GROUP_BY,
  FILTER_MAP,
  singleSelectFilters,
  GROUP_BY_COLUMNS,
} from './constants';

import StatsPanel from '../StatsPanel/StatsPanel';

import BrowserRows from './BrowseRows';

import UsageOverTime from './UsageOverTime';

import styles from '../Metrics.module.scss';

const Usage = props => {
  const { queryParams, projectId, RenderLink } = props;

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
          <Analytics name="MonitoringUsage" {...REDACT_EVERYTHING}>
            <div className="infoWrapper">
              <div className={styles.subHeader}>Usage</div>
              <UsageOverTime filters={filters} projectId={projectId} />
              <BrowserRows
                projectId={projectId}
                RenderLink={RenderLink}
                filters={filters}
                groupBys={groups}
                label={'Query List'}
              />
            </div>
          </Analytics>
        );
      }}
    </StatsPanel>
  );

  /*
  <div className={styles.usageWrapper}>
    <div className={styles.filterBtnWrapper}>
      <div className={styles.subHeader}>
        Group By {renderSelectedGroupCount()}
      </div>
      {renderResetGroupButton()}
    </div>
    <GroupBy onChange={toggleGroupBy} groupBys={groupBys} />
    <div className={styles.filterBtnWrapper}>
      <div className={styles.subHeader}>
        Filter {renderSelectedFiltersCount()}
      </div>
      {renderSelectedFilters()}
      {renderResetFilterButton()}
    </div>
    <Filters onChange={toggleFilter} filters={filters} />
    <div className={styles.subHeader}>Usage </div>
    <UsageOverTime filters={filters} />
    <BrowserRows groupBys={groupBys} filters={filters} label={'Query List'} />
  </div>
  */
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
const usageConnector = connect => connect(mapStateToProps)(Usage);

export { Usage };

export default usageConnector;
