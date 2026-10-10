import {
  updateQsHistory,
  applyFilterByQueryParam,
  retrieveFilterData,
  retrieveDefaultDropdownOptions,
  transformToTypeValueArr,
} from '../utils';
import { fetchFiltersData } from './graphql.queries';
import BrowserRows from './BrowseRows';
import {
  OPERATIONS_TITLE_MAP,
  OPERATIONS_NO_TITLE_MAP,
  OPERATIONS_FILTER_MAP,
  singleSelectFilters,
} from './constants';

import StatsPanel from '../StatsPanel/StatsPanel';
import { useSearchParams } from 'react-router';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';

export const Operations = () => {
  const [searchParams] = useSearchParams();
  const queryParams = applyFilterByQueryParam(searchParams, true);

  const onFilterChangeCb = (nextFilters) => {
    if (nextFilters.length > 0) {
      const qs = `?filters=${window.encodeURI(JSON.stringify(nextFilters))}`;
      updateQsHistory(qs);
    } else {
      updateQsHistory();
    }
  };

  const getTitle = (value) => {
    return OPERATIONS_TITLE_MAP[value];
  };

  const getEmptyTitle = (value) => {
    return OPERATIONS_NO_TITLE_MAP[value];
  };

  const filtersData = transformToTypeValueArr(OPERATIONS_FILTER_MAP);

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
    >
      {({ filters, projectId }) => {
        return (
          <Analytics name="Operations" {...REDACT_EVERYTHING}>
            <div className="infoWrapper">
              <BrowserRows
                filters={filters}
                label={'Operations List'}
                projectId={projectId}
              />
            </div>
          </Analytics>
        );
      }}
    </StatsPanel>
  );
};

export default Operations;
