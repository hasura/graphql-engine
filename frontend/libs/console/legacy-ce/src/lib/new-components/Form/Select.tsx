import React, { ReactText } from 'react';
import get from 'lodash/get';
import clsx from 'clsx';
import { FieldError, useFormContext } from 'react-hook-form';
import { FieldWrapper, FieldWrapperPassThroughProps } from './FieldWrapper';

type SelectItem = {
  label: ReactText;
  value: any;
  disabled?: boolean;
};

export type SelectProps = FieldWrapperPassThroughProps & {
  /**
   * The field name
   */
  name: string;
  /**
   * The options to display in the select
   */
  options: SelectItem[];
  /**
   * The placeholder text to display when the field is not valued
   */
  placeholder?: string;
  /**
   * Flag to indicate if the field is disabled
   */
  disabled?: boolean;
  /**
   * The value of the field
   */
  value?: string;
  /**
   * The input field classes
   */
  selectClassName?: string;
};

export const Select: React.VFC<SelectProps> = ({
  name,
  options,
  placeholder,
  dataTest,
  disabled = false,
  value: val,
  selectClassName,
  ...wrapperProps
}) => {
  const {
    register,
    formState: { errors },
    watch,
  } = useFormContext();

  const maybeError = get(errors, name) as FieldError | undefined;
  const watchValue = watch(name);

  return (
    <FieldWrapper id={name} {...wrapperProps} error={maybeError}>
      <select
        id={name}
        className={clsx(
          'block w-full h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus-visible:outline-0 focus-visible:ring-2 focus-visible:ring-yellow-200 focus-visible:border-yellow-400',
          watchValue && watchValue !== '' ? 'text-black' : 'text-gray-500',
          disabled
            ? 'cursor-not-allowed bg-gray-200 border-gray-200 hover:border-gray-200'
            : 'hover:border-gray-400',
          selectClassName
        )}
        disabled={disabled}
        value={val || watchValue}
        data-test={dataTest}
        data-testid={wrapperProps.dataTestId || name}
        {...register(name)}
        // onChange={e => {
        //   register(name).onChange(e);

        // }}
      >
        {placeholder ? (
          <option value="" data-default-selected hidden>
            {placeholder}
          </option>
        ) : null}

        {options.map(({ label, value, disabled: optionDisabled = false }) => (
          <option
            key={value}
            data-testid={(wrapperProps.dataTestId || name) + '-' + value}
            {...{ value, disabled: optionDisabled }}
          >
            {label}
          </option>
        ))}
      </select>
    </FieldWrapper>
  );
};
