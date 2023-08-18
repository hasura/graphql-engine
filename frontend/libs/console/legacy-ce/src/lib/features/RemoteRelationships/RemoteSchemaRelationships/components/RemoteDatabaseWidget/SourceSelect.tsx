import { Table } from '../../../../hasura-metadata-types';
import {
  FieldWrapper,
  FieldWrapperPassThroughProps,
} from '../../../../../new-components/Form';
import isEqual from 'lodash/isEqual';
import get from 'lodash/get';
import React from 'react';
import { Controller, FieldError, useFormContext } from 'react-hook-form';
import { FaTable } from 'react-icons/fa';
import Select, {
  components,
  ControlProps,
  OptionProps,
  SingleValueProps,
} from 'react-select';
import './index.css';

export interface SourceOption {
  value: { type: 'table'; dataSourceName: string; table: Table };
  label: string;
}

type SearchableSelectProps = FieldWrapperPassThroughProps & {
  options: SourceOption[];
  name: string;
  disabled?: boolean;
};

const Option = (props: OptionProps<SourceOption>) => {
  return (
    <components.Option {...props}>
      <span className="flex items-center gap-2">
        <span className="text-gray-500">{<FaTable />}</span>
        <span className="text-gray-800">{props.label}</span>
      </span>
    </components.Option>
  );
};

const SingleValue = (props: SingleValueProps<SourceOption>) => {
  // console.log('!!', props.data.value);
  return (
    <components.SingleValue {...props}>
      <span className="flex items-center gap-2">
        <span className="text-gray-500">
          <FaTable />
        </span>
        <span className="text-gray-800">{props.data.label}</span>
      </span>
    </components.SingleValue>
  );
};

const selectStyles = {
  option: (base: any, state: OptionProps<SourceOption>) => {
    return {
      ...base,
      backgroundColor: state.isFocused ? '#ededed' : null,
      color: '#333333',
    };
  },
  control: (base: any, state: ControlProps<SourceOption>) => {
    return {
      ...base,
      border: state.isFocused ? '1px solid #FACC14' : null,
      boxShadow: 'none',
    };
  },
  // menuPortal: (base: any) => ({ ...base, zIndex: 9999 }),
  // menuPortal: (provided: any) => ({ ...provided, zIndex: 9999 }),
  // menu: (base: any) => ({ ...base, zIndex: 9999 }),
};

export const SourceSelect = ({
  options,
  name,
  disabled,
  ...wrapperProps
}: SearchableSelectProps) => {
  const {
    formState: { errors },
    control,
  } = useFormContext();

  const maybeError = get(errors, name) as FieldError | undefined;
  return (
    <FieldWrapper id={name} {...wrapperProps} error={maybeError}>
      <Controller
        control={control}
        name={name}
        render={({ field: { onChange, onBlur, value, ref } }) => (
          <Select
            classNamePrefix="my-select"
            onBlur={onBlur}
            value={options.find(c => isEqual(c.value, value))}
            onChange={val => onChange((val as SourceOption).value)}
            ref={ref}
            components={{ Option, SingleValue }}
            options={options}
            styles={selectStyles}
            isDisabled={disabled}
            aria-labelledby={name}
            name={name}
            // menuPortalTarget={document.body}
            // menuPosition={'fixed'}
            // menuContainerStyle={{ zIndex: 5 }}
          />
        )}
      />
    </FieldWrapper>
  );
};
