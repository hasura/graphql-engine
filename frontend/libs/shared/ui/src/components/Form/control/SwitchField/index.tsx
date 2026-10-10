import { Controller, useFormContext } from 'react-hook-form';
import { Switch, SwitchProps } from '../../base';
import { FieldWrapper, FieldWrapperPassThroughProps } from '../FieldWrapper';
import { ReactNode } from 'react';

export type SwitchFieldProps = FieldWrapperPassThroughProps & {
  /**
   * The textarea name
   */
  name: string;
  /**
   * Flag to indicate if the field is disabled
   */
  disabled?: boolean;

  children?: ReactNode;

  fieldProps?: Omit<SwitchProps, 'disabled' | 'children' | 'value' | 'loading'>;
};

export const SwitchField = ({
  name,
  disabled,
  fieldProps,
  children,
  ...wrapperProps
}: SwitchFieldProps) => {
  const { control } = useFormContext();

  return (
    <Controller
      name={name}
      control={control}
      render={({ field, fieldState }) => {
        return (
          <FieldWrapper id={name} {...wrapperProps} error={fieldState.error}>
            <Switch
              {...fieldProps}
              {...field}
              loading={wrapperProps?.loading}
              onChange={(value) => {
                fieldProps?.onChange?.(value);
                field.onChange(value);
              }}
              aria-invalid={!fieldState.invalid ? 'true' : 'false'}
              invalid={fieldState.invalid}
              disabled={disabled}
              data-testid={name}
            >
              {children}
            </Switch>
          </FieldWrapper>
        );
      }}
    />
  );
};
