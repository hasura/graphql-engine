import React, { ReactText, useState, useMemo } from 'react';
import Select, {
  components,
  createFilter,
  OptionProps,
  OptionTypeBase,
  ValueType,
  OptionsType,
} from 'react-select';
import CreatableSelect from 'react-select/creatable'
import mapValues from 'lodash/mapValues';
import compact from 'lodash/compact';

import { isObject, isNotDefined } from '../utils/jsUtils';

const { Option } = components;

const CustomOption: React.FC<OptionProps<Option>> = props => {
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

function isOption(value: any): value is Option {
  return isObject(value)
}

const toOption = (value?: Option | string): Option | undefined =>  {
  if(isNotDefined(value)) return undefined
  return isOption(value) ? value : { value, label: value }
}

const filters = {
  'prefix': createFilter({ matchFrom: 'start' }),
  'fulltext': createFilter({ matchFrom: 'any' }),
}

export interface SearchableSelectProps {
  options: (Option | string)[];
  onChange: (value: ValueType<Option>) => void;
  value?: Option | string;
  bsClass?: string;
  styleOverrides?: Record<PropertyKey, any>;
  placeholder: string;
  filterOption: 'prefix' | 'fulltext';
  isCreatable?: boolean;
  createNewOption?: (v: string) => Option;
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
  // Convert simple string options to actual Options
  const optionValue = toOption(value)
  const optionsArray: OptionsType<Option> = compact(options.map(toOption))

  const customStyles = mapValues(styleOverrides, (style: any) =>
    (provided: object) => ({ ...provided, style })
  )

  const selectProps = {
    blurInputOnSelect: true,
    components: { Option: CustomOption },
    classNamePrefix: bsClass,
    placeholder: placeholder,
    options: optionsArray,
    onChange,
    value: optionValue,
    styles: customStyles,
  }

  console.log(isCreatable)

  return isCreatable ? <CreatableSelect {...selectProps} /> : <Select {...selectProps } filterOption={filters[filterOption]} />;
};

export default SearchableSelect;
