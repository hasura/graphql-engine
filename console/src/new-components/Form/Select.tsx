import React, { ReactText } from 'react';
import get from 'lodash.get';
import { FieldError, useFormContext, useWatch } from 'react-hook-form';
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
};

export const Select: React.VFC<SelectProps> = ({
  options,
  placeholder,
  name,
  ...wrapperProps
}: SelectProps) => {
  const {
    register,
    formState: { errors },
    control,
  } = useFormContext();

  const selectedValue = useWatch({ control, name, defaultValue: undefined });

  const maybeError = get(errors, name) as FieldError | undefined;

  return (
    <FieldWrapper {...wrapperProps} error={maybeError}>
      <select
        className="block w-full h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus:outline-0 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400"
        {...register(name)}
      >
        {placeholder ? (
          <option disabled selected={selectedValue === undefined}>
            {placeholder}
          </option>
        ) : null}

        {options.map(({ label, value, disabled = false }) => (
          <option {...{ value, disabled }}>{label}</option>
        ))}
      </select>
    </FieldWrapper>
  );
};
