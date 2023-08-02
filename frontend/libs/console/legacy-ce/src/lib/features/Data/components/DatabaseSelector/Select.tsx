import clsx from 'clsx';
import React from 'react';
import { FieldError } from 'react-hook-form';
import { FieldWrapper } from '../../../../new-components/Form/FieldWrapper';

interface TSelect extends React.ComponentProps<'select'> {
  options: string[];
  error?: FieldError;
  icon?: React.ReactElement;
  label: string;
}

export const Select = ({
  options,
  placeholder,
  disabled,
  error,
  ...props
}: TSelect) => (
  <FieldWrapper
    id={props.name}
    error={error}
    labelIcon={props.icon}
    label={props.label}
  >
    <select
      {...props}
      className={clsx(
        'block w-full h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus:outline-0 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400',
        disabled
          ? 'cursor-not-allowed bg-gray-100 border-gray-100'
          : 'hover:border-gray-400'
      )}
      disabled={disabled}
      value={props.value}
      id={props.name}
    >
      {placeholder ? (
        <option disabled value="">
          {placeholder}
        </option>
      ) : null}

      {options.map((op, i) => (
        <option value={op} key={i}>
          {op}
        </option>
      ))}
    </select>
  </FieldWrapper>
);
