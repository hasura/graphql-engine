import React from 'react';

import FilterCheckboxComponent from './FilterCheckboxComponent';

import { filterByType } from './utils';

const FilterTypeCheckbox = ({ id, title, onChange, filters }) => {
  const selectedValues = filterByType(filters, id);

  const selectedOption = selectedValues.length;
  return (
    <FilterCheckboxComponent
      id={id}
      title={title}
      onChange={onChange}
      checked={selectedOption > 0}
    />
  );
};

export default FilterTypeCheckbox;
