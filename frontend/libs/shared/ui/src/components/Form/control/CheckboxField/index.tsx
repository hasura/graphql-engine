import { Controller, FieldError, useFormContext } from 'react-hook-form';
import { Checkbox, CheckboxProps } from '../../base';
import get from 'lodash/get';
import { FieldWrapper, FieldWrapperPassThroughProps } from '../FieldWrapper';
import { ReactNode } from 'react';

export type CheckboxFieldProps = FieldWrapperPassThroughProps & {
  /**
   * The textarea name
   */
  name: string;
  /**
   * Flag to indicate if the field is disabled
   */
  disabled?: boolean;

  children?: ReactNode;

  fieldProps?: Omit<CheckboxProps, 'disabled' | 'children' | 'value'>;
};

export const CheckboxField = ({
  name,
  disabled,
  fieldProps,
  children,
  ...wrapperProps
}: CheckboxFieldProps) => {
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
          return (
            <Checkbox
              {...fieldProps}
              {...field}
              onChange={(value) => {
                fieldProps?.onChange?.(value);
                field.onChange(value);
              }}
              aria-invalid={maybeError ? 'true' : 'false'}
              invalid={Boolean(maybeError)}
              disabled={disabled}
              data-testid={name}
            >
              {children}
            </Checkbox>
          );
        }}
      />
    </FieldWrapper>
  );
};
