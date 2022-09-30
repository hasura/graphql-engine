import React from 'react';

import {
  FILTER_TYPE_INPUT,
  FILTER_TYPE_DROPDOWN_DEFAULT,
  FILTER_TYPE_DROPDOWN,
  FILTER_TYPE_CHECKBOX,
} from '../Error/constants';

import FilterTypeDropdown from '../Error/FilterTypeDropdown';
import FilterTypeInput from '../Error/FilterTypeInput';

import TimeRangeFilter from '../Error/TimeRangeFilter';
import FilterTypeCheckbox from '../Operations/FilterTypeCheckbox';

const GenerateFilters = ({
  idMap,
  titleMap,
  nodeMap,
  subNodeMap,
  emptyTitleMap,
  queryMap,
  onChange,
  filters,
  values,
}) => {
  const filtersHtml = [];
  Object.keys(filters).forEach((g, i) => {
    if (filters[g] === FILTER_TYPE_DROPDOWN) {
      filtersHtml.push(
        <FilterTypeDropdown
          key={i}
          id={idMap[g]}
          title={titleMap[g]}
          node={nodeMap[g]}
          subNode={subNodeMap[g]}
          emptyTitle={emptyTitleMap[g]}
          onChange={onChange}
          filters={values}
          query={queryMap[g]}
        />
      );
    }
    if (filters[g] === FILTER_TYPE_INPUT) {
      filtersHtml.push(
        <FilterTypeInput
          key={i}
          id={idMap[g]}
          title={titleMap[g]}
          onChange={onChange}
        />
      );
    }
    if (filters[g] === FILTER_TYPE_DROPDOWN_DEFAULT) {
      filtersHtml.push(
        <TimeRangeFilter
          key={i}
          id={idMap[g]}
          title={titleMap[g]}
          onChange={onChange}
          filters={values}
        />
      );
    }
    if (filters[g] === FILTER_TYPE_CHECKBOX) {
      filtersHtml.push(
        <FilterTypeCheckbox
          key={i}
          id={idMap[g]}
          title={titleMap[g]}
          onChange={onChange}
          filters={values}
        />
      );
    }
  });
  return filtersHtml;
};

export default GenerateFilters;
