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
import { useSearchParams } from 'react-router';
import { useProjectInfo } from '../../../../hooks/useProjectInfo';

export const Error = () => {
  const [searchParams] = useSearchParams();
  const { data: projectInfo } = useProjectInfo();
  const preAppliedFilter = applyFilterByQueryParam(
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

  if (!projectInfo) {
    return <div>Loading...</div>;
  }

  return (
    <StatsPanel
      singleSelectFilters={singleSelectFilters}
      groupByCols={GROUP_BY_COLUMNS}
      getTitle={getTitle}
      getEmptyTitle={getEmptyTitle}
      initialGroupBysState={preAppliedFilter.parsedGroupbys}
      filters={filtersData}
      initialFiltersState={preAppliedFilter.parsedFilter}
      retrieveFilterData={retrieveFilterData}
      onFilterChangeCb={onFilterChangeCb}
      retrieveDefaultDropdownOptions={retrieveDefaultDropdownOptions}
      query={fetchFiltersData}
      onGroupByChangeCb={onGroupByChangeCb}
    >
      {({ filters, groups }) => {
        return (
          <div className="infoWrapper">
            <div className={styles['subHeader']}>Errors over time</div>
            <ErrorsOverTime
              filters={filters}
              groupBys={groups}
              projectId={projectInfo.id}
            />
            <BrowserRows
              filters={filters}
              groupBys={groups}
              projectId={projectInfo.id}
              label={'Frequent errors'}
            />
          </div>
        );
      }}
    </StatsPanel>
  );
};

export default Error;
