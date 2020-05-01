import React, { ReactText } from 'react';
import Select, {
  components,
  createFilter,
  OptionProps,
  OptionTypeBase,
  ValueType,
} from 'react-select';

import { isArray, isObject } from '../utils/jsUtils';

type Option = { label: string; value: string };

const CustomOption: React.FC<OptionProps<OptionTypeBase>> = props => {
  return (
    <div
      title={props.data.description || ''}
      data-test={`data_test_column_type_value_${props.data.value}`}
    >
      <components.Option {...props} />
    </div>
  );
};

type Props = {
  options: OptionTypeBase | ReactText[];
  onChange: (value: ValueType<OptionTypeBase>) => void;
  value?: Option | string;
  bsClass: string;
  styleOverrides: Record<PropertyKey, any>;
  placeholder: string;
  filterOption: 'prefix' | 'fulltext';
  onInputChange?: (v: string) => void;
  isClearable?: boolean;
  onMenuClose?: () => void;
};
const SearchableSelect: React.FC<Props> = ({
  options,
  onChange,
  value,
  bsClass,
  styleOverrides,
  placeholder,
  filterOption,
  onInputChange,
  onMenuClose,
}) => {
  const customStyles: Record<string, any> = {};
  if (styleOverrides) {
    Object.keys(styleOverrides).forEach(comp => {
      customStyles[comp] = (provided: object) => ({
        ...provided,
        ...styleOverrides[comp],
      });
    });
  }

  let customFilter;
  switch (filterOption) {
    case 'prefix':
      customFilter = createFilter({ matchFrom: 'start' });
      break;
    case 'fulltext':
      customFilter = createFilter({ matchFrom: 'any' });
      break;
    default:
      customFilter = null;
  }

  // handle simple options
  if (isArray(options) && !isObject((options as unknown[])[0])) {
    options = options.map((op: string) => {
      return { value: op, label: op };
    });
  }

  if (value && !isObject(value)) {
    value = { value: value as string, label: value as string };
  }

  return (
    <Select
      isSearchable
      components={{ Option: CustomOption }}
      classNamePrefix={bsClass}
      placeholder={placeholder}
      options={options as Option[]}
      onChange={onChange}
      value={value as Option}
      styles={customStyles}
      filterOption={customFilter}
      onInputChange={onInputChange}
      onMenuClose={onMenuClose}
    />
  );
};

export default SearchableSelect;
