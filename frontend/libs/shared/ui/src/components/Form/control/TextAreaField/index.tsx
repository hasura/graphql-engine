import React from 'react';
import { Controller, useFormContext } from 'react-hook-form';
import { FieldWrapper, FieldWrapperPassThroughProps } from '../FieldWrapper';
import { TextArea, TextAreaProps } from '../../base';

export type TextAreaFieldProps = FieldWrapperPassThroughProps & {
  /**
   * The textarea name
   */
  name: string;
  /**
   * The textarea visible rows number
   */
  rows?: number;
  /**
   * The textarea placeholder
   */
  placeholder?: string;
  /**
   * Flag to indicate if the field is disabled
   */
  disabled?: boolean;

  fieldProps?: Omit<TextAreaProps, 'disabled' | 'placeholder' | 'rows'>;
};

export const TextAreaField: React.FC<TextAreaFieldProps> = ({
  rows = 3,
  name,
  placeholder,
  disabled,
  dataTest,
  fieldProps,
  ...wrapperProps
}) => {
  const { control } = useFormContext();

  return (
    <Controller
      name={name}
      control={control}
      render={({ field, fieldState }) => (
        <FieldWrapper id={name} {...wrapperProps} error={fieldState.error}>
          <TextArea
            {...fieldProps}
            {...field}
            id={name}
            aria-invalid={!fieldState.invalid ? 'true' : 'false'}
            data-test={dataTest}
            invalid={fieldState.invalid}
            placeholder={placeholder}
            disabled={disabled}
            data-testid={name}
          />
        </FieldWrapper>
      )}
    />
  );
};
