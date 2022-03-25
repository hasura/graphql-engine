import React, { ReactText } from 'react';
import get from 'lodash.get';
import { FieldError, useFormContext } from 'react-hook-form';
import { FieldWrapper, FieldWrapperPassThroughProps } from './FieldWrapper';

type SelectItem = {
  label: ReactText;
  value: ReactText;
  disabled?: boolean;
};

export type SelectProps = FieldWrapperPassThroughProps & {
  name: string;
  options: SelectItem[];
  placeholder?: string;
  disabled?: boolean;
};

export const Select: React.VFC<SelectProps> = ({
  options,
  placeholder,
  name,
  disabled = false,
  ...wrapperProps
}: SelectProps) => {
  const {
    register,
    formState: { errors },
  } = useFormContext();

  const maybeError = get(errors, name) as FieldError | undefined;

  return (
    <FieldWrapper {...wrapperProps} error={maybeError}>
      <select
        className={`block w-full h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus:outline-0 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400 ${
          disabled
            ? 'cursor-not-allowed bg-gray-100 border-gray-100'
            : 'hover:border-gray-400'
        }`}
        disabled={disabled}
        {...register(name)}
      >
        {placeholder ? (
          <option disabled value="">
            {placeholder}
          </option>
        ) : null}

        {options.map(({ label, value, disabled: optionDisabled = false }) => (
          <option key={value} {...{ value, disabled: optionDisabled }}>
            {label}
          </option>
        ))}
      </select>
    </FieldWrapper>
  );
};
