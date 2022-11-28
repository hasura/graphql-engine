import React from 'react';
import FilterComponent from './FilterComponent';

import { filterByType, getJson } from './utils';

import CustomDatePickerMenuItem from '../../Common/CustomMenuItems/CustomDatePickerMenuItem';

const TimeRangeFilter = ({ onChange, filters, id, title, options }) => {
  const selectedFilters = filterByType(filters, id);
  const selectedValues = getJson(selectedFilters, 'value');
  const timeRangeFilterList = options.map((t, i) => {
    return {
      title: t,
      filterId: `${id}-${i}`,
    };
  });
  const customOnChange = value => {
    if (value !== 'custom') {
      onChange(value);
    }
  };
  const applyFilter = value => {
    onChange(value);
  };
  const getDisplayValue = () => {
    if (selectedFilters.length > 0 && !('Custom' in selectedValues)) {
      return selectedFilters[0].value;
    } else if ('Custom' in selectedValues) {
      return 'Custom';
    }
    return title;
  };
  return (
    <FilterComponent
      id={id}
      title={title}
      displayValue={getDisplayValue()}
      options={timeRangeFilterList}
      onChange={customOnChange}
      selectedValues={selectedValues}
    >
      <CustomDatePickerMenuItem
        title="Custom Date"
        value="custom"
        onApply={applyFilter}
      />
    </FilterComponent>
  );
};

export default TimeRangeFilter;
