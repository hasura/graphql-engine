import React, { ReactText } from 'react';
import clsx from 'clsx';
import get from 'lodash/get';
import { FieldError, useFormContext, Controller } from 'react-hook-form';
import { FieldWrapper, FieldWrapperPassThroughProps } from './FieldWrapper';
import { Checkbox } from './Checkbox';

export type CheckboxItem = {
  label: ReactText;
  value: ReactText;
  disabled?: boolean;
};

export type CheckboxProps = FieldWrapperPassThroughProps & {
  /**
   * The checkbox name
   */
  name: string;
  /**
   * The options to display with checkboxes
   */
  options: CheckboxItem[];
  /**
   * The checkbox list orientation
   */
  orientation?: 'vertical' | 'horizontal';
  /**
   * Flag to indicate if the field is disabled
   */
  disabled?: boolean;
  /**
   * Removing styling only necessary for the error placeholder
   */
  noErrorPlaceholder?: boolean;
};

export const CheckboxesField: React.FC<CheckboxProps> = ({
  name,
  options = [],
  orientation = 'vertical',
  disabled = false,
  dataTest,
  noErrorPlaceholder,
  ...wrapperProps
}: CheckboxProps) => {
  const {
    control,
    formState: { errors },
  } = useFormContext();

  const maybeError = get(errors, name) as FieldError | undefined;
  return (
    <FieldWrapper
      noErrorPlaceholder={noErrorPlaceholder}
      id={name}
      {...wrapperProps}
      error={maybeError}
    >
      <div
        className={clsx(
          'flex flex-wrap',
          { vertical: 'flex-col', horizontal: 'flex-row ' }[orientation]
        )}
      >
        <Controller
          name={name}
          control={control}
          render={({ field }) => (
            <>
              {options.map(
                ({ label, value, disabled: optionDisabled = false }) => {
                  return (
                    <div key={value} className="flex items-center">
                      <Checkbox
                        {...field}
                        name={name}
                        checked={field.value?.includes(value)}
                        onCheckedChange={checked => {
                          if (!field.value) {
                            field.value = [];
                          }
                          if (checked && !field.value.includes(value)) {
                            field.onChange([...field.value, value]);
                          }
                          if (!checked && field.value.includes(value)) {
                            field.onChange(
                              field.value.filter((v: string) => v !== value)
                            );
                          }
                        }}
                        invalid={!!maybeError}
                        aria-invalid={maybeError ? 'true' : 'false'}
                        aria-label={wrapperProps.label}
                        disabled={disabled || optionDisabled}
                        data-test={dataTest}
                        data-testid={`${name}-${value}`}
                      >
                        {label}
                      </Checkbox>{' '}
                    </div>
                  );
                }
              )}
            </>
          )}
        />
      </div>
    </FieldWrapper>
  );
};
