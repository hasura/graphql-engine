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
import { useSearchParams } from 'react-router';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';

const Usage = () => {
  const [searchParams] = useSearchParams();
  const queryParams = applyFilterByQueryParam(
    searchParams,
    true,
    DEFAULT_GROUP_BY,
  );
  const getTitle = (value) => {
    return TITLE_MAP[value];
  };

  const getEmptyTitle = (value) => {
    return NO_TITLE_MAP[value];
  };

  const filtersData = transformToTypeValueArr(FILTER_MAP);

  return (
    <StatsPanel
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
      {({ filters, groups, projectId }) => {
        return (
          <Analytics name="MonitoringUsage" {...REDACT_EVERYTHING}>
            <div className="infoWrapper">
              <div className={styles['subHeader']}>Usage</div>
              <UsageOverTime filters={filters} projectId={projectId} />
              <BrowserRows
                projectId={projectId}
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
};

export default Usage;
