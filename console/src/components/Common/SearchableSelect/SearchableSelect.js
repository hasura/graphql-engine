import React from 'react';
import Select, { components } from 'react-select';
import PropTypes from 'prop-types';

/* Custom option component for select to render option with title */
const CustomOption = props => {
  return (
    <div title={props.data.description || ''}>
      <components.Option {...props} />
    </div>
  );
};

/*
 * Searchable select box component
 *  1) options: Accepts options
 *  2) value: selectedValue
 *  3) onChange: function to call on change of value
 *  4) bsClass: Wrapper class
 *  5) customStyle: Custom style
 * */
const SearchableSelectBox = ({
  options,
  onChange,
  value,
  bsClass,
  customStyle,
}) => {
  /* Select element style customization */

  return [
    <Select
      isSearchable
      components={{ Option: CustomOption }}
      classNamePrefix={`${bsClass}`}
      placeholder="column_type"
      options={options}
      onChange={onChange}
      value={value}
      styles={customStyle}
    />,
  ];
};

SearchableSelectBox.propTypes = {
  value: PropTypes.string.isRequired,
  onChange: PropTypes.function.isRequired,
  options: PropTypes.array.isRequired,
  bsClass: PropTypes.string,
  customStyle: PropTypes.object,
};

export default SearchableSelectBox;
