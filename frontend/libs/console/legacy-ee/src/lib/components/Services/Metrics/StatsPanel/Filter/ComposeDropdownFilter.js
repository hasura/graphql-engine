import React from 'react';
import { useQuery } from '@apollo/react-hooks';

import { getSelectedFiltersCount, getJson } from './utils';

import FilterTypeDropdown from './FilterTypeDropdown';

import LoadingSpinner from '../../Common/LoadingSpinner';
import {
  transportFilters,
  TRANSPORT_SYMBOL,
  subscriptionWorkerFilters,
  SUBSCRIPTION_STATUS_SYMBOL,
  websocketStatusFilters,
  WEBSOCKET_STATUS_SYMBOL,
  SELECT_ALL_SYMBOL,
  SELECT_ALL_NAME_DISPLAY,
} from '../../constants';

import styles from '../../Metrics.module.scss';

const ComposeDropdownFilter = ({
  query,
  projectId,
  retrieveFilterData,
  filters,
  values,
  onChange,
  getTitle,
  getEmptyTitle,
  selectAll,
}) => {
  const variables = {};
  if (projectId) {
    variables.projectId = projectId;
  }
  const { loading, error, data } = useQuery(query, {
    variables: { ...variables },
    fetchPolicy: 'cache-first',
  });
  const loadingIcon = (
    <div className={`${styles.selectBox} ${styles.verticalAlignMiddle}`}>
      <LoadingSpinner />
    </div>
  );

  if (loading) {
    return loadingIcon;
  }
  if (error) {
    return 'Error occured';
  }

  if (!data) {
    return null;
  }

  const dropdownFilters = filters.map((filter, key) => {
    /* Here value denotes type of the filter */
    const { value } = filter;
    const transports = transportFilters.map(v => ({
      [TRANSPORT_SYMBOL]: v,
    }));
    const subscriptionWorkerStatus = subscriptionWorkerFilters.map(v => ({
      [SUBSCRIPTION_STATUS_SYMBOL]: v,
    }));
    const websocketStatus = websocketStatusFilters.map(p => ({
      [WEBSOCKET_STATUS_SYMBOL]: p,
    }));
    const d = retrieveFilterData(
      {
        ...data,
        transports,
        subscriptionWorkerStatus,
        websocketStatus,
      },
      filter
    );
    const selectedFilters = values.filter(v => v.type === value);

    const selectedValues = getJson(selectedFilters, 'value');
    const emptyTitle = getEmptyTitle(value);
    const title = getTitle(value);
    const dropdownItems = d.map((f, i) => {
      const val = f[value];
      const dropdownTitle =
        (typeof val === 'string' && (val.length === 0 ? emptyTitle : val)) ||
        emptyTitle;
      return {
        title: dropdownTitle,
        filterId: `${value}-${i}`,
      };
    });
    if (dropdownItems.length > 2) {
      dropdownItems.push({
        title: SELECT_ALL_NAME_DISPLAY,
        filterId: SELECT_ALL_SYMBOL,
      });
    }
    const selectedFiltersCount = getSelectedFiltersCount(selectedFilters);
    const onFilterChange = onChange(value);
    return (
      <FilterTypeDropdown
        key={key}
        id={value}
        title={title}
        displayValue={selectedFiltersCount}
        options={dropdownItems}
        onChange={onFilterChange}
        selectedValues={selectedValues}
        selectAll={selectAll}
      />
    );
  });

  return dropdownFilters;
};

export default ComposeDropdownFilter;
