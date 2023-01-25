import React from 'react';

import FilterInputComponent from './FilterInputComponent';

const FilterTypeInput = ({ onChange, id, title }) => {
  return (
    <FilterInputComponent
      id={id}
      filterTitle={title}
      placeholder="Type and enter to filter"
      onChange={onChange}
    />
  );
};

export default FilterTypeInput;
