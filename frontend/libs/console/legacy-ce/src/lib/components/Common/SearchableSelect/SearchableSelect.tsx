import React, { ReactText, useState, useMemo, useEffect, useRef } from 'react';
import { ReactSelect } from './../../../new-components/Form';
import {
  components,
  createFilter,
  OptionProps,
  OnChangeValue,
} from 'react-select';

import { isArray, isObject } from '../utils/jsUtils';

const { Option } = components;

const CustomOption: React.FC<OptionProps<any>> = props => {
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
  options: any | ReactText[];
  onChange: (value: OnChangeValue<any, boolean> | string) => void;
  value?: Option | string;
  bsClass?: string;
  styleOverrides?: Record<PropertyKey, any>;
  placeholder?: string;
  filterOption?: 'prefix' | 'fulltext';
  isCreatable?: boolean;
  onSearchValueChange?: (v: string | undefined | null) => void;
  createNewOption?: (v: string) => OnChangeValue<any, boolean>;
  isClearable?: boolean;
}

const SearchableSelect: React.FC<SearchableSelectProps> = ({
  options,
  onChange,
  value,
  bsClass,
  styleOverrides,
  placeholder = 'Select...',
  filterOption = 'prefix',
  isCreatable,
  createNewOption,
  onSearchValueChange,
  isClearable,
}) => {
  const [searchValue, setSearchValue] = useState<string | null>(null);
  const [localValue, setLocalValue] = useState<Option | null>(null);
  const [isFocused, setIsFocused] = useState(false);
  const selectedItem = useRef<OnChangeValue<any, boolean> | string>(null);

  const inputValue = useMemo(() => {
    // if input is not focused we don't want to show inputValue
    if (!isFocused) return;

    // if user is searching we don't want to control the input
    if (searchValue !== null) return searchValue; // when the user focus back to input

    // otherwise we display last selected option
    // this way typing after selecting an options is allowed
    return typeof value === 'string' ? value : value?.label;
  }, [searchValue, value, isFocused]);
  useEffect(() => {
    if (onSearchValueChange) onSearchValueChange(searchValue);
  }, [searchValue]);
  useEffect(() => {
    let tempValue: Option | null;
    if (value === '') {
      setLocalValue(null);
    } else {
      if (value && !isObject(value)) {
        tempValue = { value: value as string, label: value as string };
      } else {
        tempValue = value as Option;
      }
      setLocalValue(tempValue);
    }
  }, [value]);

  const onMenuClose = () => {
    if (selectedItem.current) onChange(selectedItem.current);
    else if (createNewOption && isCreatable)
      onChange(createNewOption(searchValue || ''));

    setIsFocused(false);
    setSearchValue(null);
    selectedItem.current = null;
  };
  const onSelect = React.useCallback(
    (v: OnChangeValue<any, boolean>) => {
      let tempValue: Option | null;

      // Force update when field is cleared
      if (v === null) {
        onChange('');
      }

      if (v && !isObject(v)) {
        tempValue = { value: v as string, label: v as string };
      } else {
        tempValue = v as Option;
      }
      setLocalValue(tempValue);
      selectedItem.current = tempValue;
    },
    [selectedItem]
  );
  const onFocus = () => {
    setIsFocused(true);
    let ipValue;
    if (!inputValue) {
      if (typeof value === 'string') ipValue = value;
      else ipValue = value?.label || '';
      setSearchValue(String(ipValue));
    }
  };

  const customStyles: Record<string, any> = {};
  if (styleOverrides) {
    Object.keys(styleOverrides).forEach(comp => {
      customStyles[comp] = (provided: Record<string, unknown>) => ({
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
    options = options.map(op => {
      return { value: op, label: op };
    });
  }

  return (
    <ReactSelect
      isSearchable
      isClearable={isClearable}
      blurInputOnSelect
      components={{ Option: CustomOption }}
      classNamePrefix={bsClass}
      placeholder={placeholder}
      options={options as Option[]}
      onChange={onSelect}
      value={localValue}
      onFocus={onFocus}
      onBlur={onMenuClose}
      defaultInputValue={inputValue}
      onInputChange={(s: string) => setSearchValue(s)}
      styles={customStyles}
      filterOption={searchValue ? customFilter : null}
      className="search-select"
    />
  );
};

export default SearchableSelect;
