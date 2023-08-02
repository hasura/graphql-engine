import React, { useState } from 'react';
import GroupBys from './Group/GroupBys';
import Filters from './Filter/Filters';

import { nextGroupValue, nextFilterValue } from './utils';
import { SELECT_ALL_NAME_DISPLAY } from '../constants';

/* Accepts
 * children: render prop function
 * initialFiltersState: initial group by configuration
 * */

const StatsPanel = ({
  initialGroupBysState,
  groupByCols,
  getTitle,
  getEmptyTitle,
  filters,
  initialFiltersState = [],
  query,
  retrieveFilterData,
  onFilterChangeCb,
  retrieveDefaultDropdownOptions,
  singleSelectFilters,
  children,
  projectId,
  onGroupByChangeCb,
}) => {
  /*
   * Accepts initialGroupByState from the caller and initializes it
   * It will persist the following state using useState
   * group: [string]
   * filter: [filterType]
   *
   * filterType : {
   *  type: string, // Type of filter
   *  value: string || object // Corresponding value to be filtered by for the above type
   * }
   * */
  /*
   * selectedGroupByCol: [string]
   * modifyGroupBys: () => void
   * */
  const [groups, updateGroup] = useState(initialGroupBysState || []);
  const [filtersData, updateFilter] = useState(initialFiltersState || []);

  /*
   * Filters state modifier
   * */

  const onFilterChange = (type, value) => {
    const isSingleSelect = singleSelectFilters.indexOf(type) !== -1;
    const nextState = nextFilterValue(filtersData, type, value, isSingleSelect);
    const finalState =
      nextState && nextState.length ? nextState : initialFiltersState;
    updateFilter(finalState);
    if (onFilterChange && typeof onFilterChange === 'function') {
      onFilterChangeCb(finalState, groups);
    }
  };

  const resetFilters = () => {
    updateFilter(initialFiltersState);
    if (onFilterChange && typeof onFilterChange === 'function') {
      onFilterChangeCb(initialFiltersState, groups);
    }
  };

  /*
   * Filters select All options
   * */

  const selectAll = (type, options, unSelect = false) => {
    const isSingleSelect = singleSelectFilters.indexOf(type) !== -1;
    const filterValues = filtersData.map(val => {
      return val.value;
    });
    // Tracks all the options which are either unselected or selected depending upon what the action is (Select All or Unselect All resp.)
    let nextOptions = [];

    if (unSelect) {
      nextOptions = options;
    } else {
      nextOptions = options.filter(val => !filterValues.includes(val.title));
    }

    const nextState = nextOptions
      .filter(v => v.title !== SELECT_ALL_NAME_DISPLAY)
      .reduce(
        (acc, v) => nextFilterValue(acc, type, v.title, isSingleSelect),
        filtersData
      );
    updateFilter(nextState);
    if (selectAll && typeof selectAll === 'function') {
      onFilterChangeCb(nextState);
    }
  };

  /*
   * Group by state modifier
   * */

  const onGroupByChange = value => {
    const nextGroup = nextGroupValue(groups, value);
    onGroupByChangeCb(nextGroup, filtersData);
    updateGroup(nextGroup);
  };

  const resetGroupBys = () => {
    updateGroup([]);
    if (onGroupByChange && typeof onGroupByChange === 'function') {
      onGroupByChangeCb([], filtersData);
    }
  };

  const renderGroupBys = () => {
    if (groupByCols) {
      return (
        <GroupBys
          onChange={onGroupByChange}
          values={groupByCols}
          selected={groups}
          reset={resetGroupBys}
          getTitle={getTitle}
        />
      );
    }
    return null;
  };

  const renderFilters = () => {
    if (filters) {
      return (
        <Filters
          projectId={projectId}
          reset={resetFilters}
          query={query}
          retrieveFilterData={retrieveFilterData}
          getTitle={getTitle}
          getEmptyTitle={getEmptyTitle}
          onChange={onFilterChange}
          filters={filters}
          values={filtersData}
          retrieveDefaultDropdownOptions={retrieveDefaultDropdownOptions}
          selectAll={selectAll}
        />
      );
    }
    return null;
  };

  return (
    <React.Fragment>
      {renderGroupBys()}
      {renderFilters()}
      {children({
        filters: filtersData,
        groups: groups,
      })}
    </React.Fragment>
  );
};

export default StatsPanel;
