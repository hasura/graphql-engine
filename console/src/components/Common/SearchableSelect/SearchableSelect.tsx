import React, { ReactText, useState, useMemo } from 'react';
import Select, {
  components,
  createFilter,
  OptionProps,
  OptionTypeBase,
  ValueType,
} from 'react-select';

import { isArray, isObject } from '../utils/jsUtils';

const { Option } = components;

const CustomOption: React.FC<OptionProps<OptionTypeBase>> = props => {
  return (
    <div
      title={props.data.description || ''}
      data-test={`data_test_column_type_value_${props.data.value}`}
    >
      <Option {...props} />
    </div>
  );
};

type Option = { label: string; value: string };

export interface SearchableSelectProps {
  options: OptionTypeBase | ReactText[];
  onChange: (value: ValueType<OptionTypeBase> | string) => void;
  value?: Option | string;
  bsClass?: string;
  styleOverrides?: Record<PropertyKey, any>;
  placeholder: string;
  filterOption: 'prefix' | 'fulltext';
  isCreatable?: boolean;
  createNewOption?: (v: string) => ValueType<OptionTypeBase>;
}
const SearchableSelect: React.FC<SearchableSelectProps> = ({
  options,
  onChange,
  value,
  bsClass,
  styleOverrides,
  placeholder,
  filterOption,
  isCreatable,
  createNewOption,
}) => {
  const [searchValue, setSearchValue] = useState<string | null>(null);
  const [isFocused, setIsFocused] = useState(false);

  const inputValue = useMemo(() => {
    // if input is not focused we don't want to show inputValue
    if (!isFocused) return;

    // if user is searching we don't want to control the input
    if (searchValue !== null) return;

    // otherwise we display last selected option
    // this way typing after selecting an options is allowed
    return typeof value === 'string' ? value : value?.label;
  }, [searchValue, value, isFocused]);

  const onMenuClose = () => {
    if (searchValue && createNewOption) onChange(createNewOption(searchValue));
    setSearchValue(null);
  };

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

  if (isCreatable && createNewOption) {
    if (searchValue) {
      options = [createNewOption(searchValue), ...(options as Option[])];
    }
  }

  return (
    <Select
      isSearchable
      blurInputOnSelect
      components={{ Option: CustomOption }}
      classNamePrefix={bsClass}
      placeholder={placeholder}
      options={options as Option[]}
      onChange={onChange}
      value={value as Option}
      onFocus={() => setIsFocused(true)}
      onBlur={() => setIsFocused(false)}
      inputValue={inputValue}
      onInputChange={s => setSearchValue(s)}
      onMenuClose={onMenuClose}
      styles={customStyles}
      filterOption={searchValue ? customFilter : null}
    />
  );
};

export default SearchableSelect;
