import React from 'react';

import {
  FILTER_TYPE_INPUT,
  FILTER_TYPE_DROPDOWN_DEFAULT,
  FILTER_TYPE_DROPDOWN,
  FILTER_TYPE_CHECKBOX,
} from '../../constants';

import { splitByType, compose } from './utils';

import FilterTypeInput from './FilterTypeInput';

import TimeRangeFilter from './TimeRangeFilter';
import FilterTypeCheckbox from './FilterTypeCheckbox';
import ComposeDropdownFilter from './ComposeDropdownFilter';

/*
 * Dropdown filters are not rendered. The pattern ensures that data for
 * all the dropdowns are fetched at once so that to avoid
 * additional round trips for each dropdown filter
 *
 * input:
 *   query - GraphQL query to fetch the filter data
 *   TODO: retrieveFilterData - function which when given data and filters precipitated type (operation_id, operation_type), should returns an array of value
 *   TODO: filters : [{
 *    type: FILTER_TYPE_DROPDOWN,
 *    value: 'operation_id',
 *   }]
 * */

const GenerateFilters = ({
  query,
  projectId,
  retrieveFilterData,
  retrieveDefaultDropdownOptions,
  getTitle,
  getEmptyTitle,
  onChange,
  filters,
  values,
  selectAll,
}: any) => {
  const filtersHtml = [];
  const [dropdownFilters, nonDropdownFilters] = splitByType(
    filters,
    FILTER_TYPE_DROPDOWN
  );
  if (dropdownFilters.length === 0 && nonDropdownFilters.length === 0) {
    return null;
  }
  const hOrderFn = compose(onChange);
  nonDropdownFilters.forEach(
    (filter: { type: any; value: any }, i: React.Key | null | undefined) => {
      const { type, value } = filter;
      const onFilterChange = hOrderFn(value);
      switch (type) {
        case FILTER_TYPE_INPUT:
          filtersHtml.push(
            <FilterTypeInput
              key={i}
              id={value}
              title={getTitle(value)}
              onChange={onFilterChange}
            />
          );
          break;
        case FILTER_TYPE_DROPDOWN_DEFAULT:
          filtersHtml.push(
            <TimeRangeFilter
              key={i}
              id={value}
              title={getTitle(value)}
              onChange={onFilterChange}
              filters={values}
              options={retrieveDefaultDropdownOptions(value)}
            />
          );
          break;
        case FILTER_TYPE_CHECKBOX:
          filtersHtml.push(
            <FilterTypeCheckbox
              key={i}
              id={value}
              title={getTitle(value)}
              onChange={onFilterChange}
              filters={values}
            />
          );
          break;
        default:
          console.error('Unsupported type');
      }
    }
  );

  if (dropdownFilters.length > 0) {
    filtersHtml.push(
      <ComposeDropdownFilter
        key="dropdown_filter_wrapper"
        query={query}
        projectId={projectId}
        retrieveFilterData={retrieveFilterData}
        getTitle={getTitle}
        getEmptyTitle={getEmptyTitle}
        onChange={hOrderFn}
        filters={dropdownFilters}
        values={values}
        selectAll={selectAll}
      />
    );
  }

  return (
    <div
      className="grid"
      style={{
        backgroundColor: '#f5f5f5',
        gridTemplateColumns: 'repeat(auto-fit, minmax(300px, 1fr))',
        gridGap: '2px',
      }}
    >
      {filtersHtml}
    </div>
  );
};

export default GenerateFilters;
