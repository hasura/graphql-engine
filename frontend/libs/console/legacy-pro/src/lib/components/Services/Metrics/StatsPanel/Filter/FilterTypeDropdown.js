import React from 'react';
import FilterComponent from './FilterComponent';

const FilterTypeDropdown = ({
  id,
  title,
  displayValue,
  options,
  onChange,
  selectedValues,
  selectAll,
}) => {
  return (
    <FilterComponent
      id={id}
      title={title}
      displayValue={displayValue}
      options={options}
      onChange={onChange}
      selectedValues={selectedValues}
      selectAll={selectAll}
    />
  );
};

export default FilterTypeDropdown;
