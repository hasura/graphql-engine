import React from 'react';
import get from 'lodash/get';
import { Controller, FieldError, useFormContext } from 'react-hook-form';
import { FieldWrapper, FieldWrapperPassThroughProps } from '../FieldWrapper';
import DatePicker, { DatePickerProps } from 'react-datepicker';
import { Input, InputProps } from '../../base';

export type DateTimePickerFieldProps = FieldWrapperPassThroughProps & {
  /**
   * The textarea name
   */
  name: string;
  /**
   * The textarea placeholder
   */
  placeholder?: string;
  /**
   * Flag to indicate if the field is disabled
   */
  disabled?: boolean;

  fieldProps?: {
    picker: Omit<
      DatePickerProps,
      | 'disabled'
      | 'dates'
      | 'selectsMultiple'
      | 'selectsRange'
      | 'showMonthYearDropdown'
      | 'showTimeSelect'
      | 'dateFormat'
      | 'onChange'
      | 'selected'
      | 'placeholderText'
    >;
    input?: Omit<InputProps, 'disabled' | 'placeholder' | 'value' | 'onChange'>;
  };
};

export const DateTimePickerField: React.FC<DateTimePickerFieldProps> = ({
  name,
  placeholder,
  disabled,
  dataTest,
  fieldProps,
  ...wrapperProps
}) => {
  const {
    control,
    formState: { errors },
  } = useFormContext();

  const maybeError = get(errors, name) as FieldError | undefined;

  return (
    <FieldWrapper id={name} {...wrapperProps} error={maybeError}>
      <Controller
        name={name}
        control={control}
        render={({ field }) => {
          const { value, onChange, disabled: fieldDisabled, ...rest } = field;
          return (
            <DatePicker
              {...fieldProps}
              {...rest}
              id={name}
              selected={value instanceof Date ? value : new Date(value)}
              onChange={onChange}
              showTimeSelect
              dateFormat="Pp"
              aria-invalid={maybeError ? 'true' : 'false'}
              data-test={dataTest}
              disabled={disabled || fieldDisabled}
              data-testid={name}
              customInput={
                <Input
                  {...fieldProps?.input}
                  placeholder={placeholder}
                  disabled={disabled}
                  containerClassName="w-[200px]"
                />
              }
            />
          );
        }}
      />
    </FieldWrapper>
  );
};
