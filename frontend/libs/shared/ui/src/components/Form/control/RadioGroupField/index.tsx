import React from 'react';
import get from 'lodash/get';
import { Controller, FieldError, useFormContext } from 'react-hook-form';
import { FieldWrapper, FieldWrapperPassThroughProps } from '../FieldWrapper';
import { RadioGroup, RadioGroupItemProps, RadioGroupProps } from '../../base';

export type RadioGroupFieldProps = FieldWrapperPassThroughProps & {
  /**
   * The radio name
   */
  name: string;

  /**
   * The options to display with radioes
   */
  options: RadioGroupItemProps[];
  /**
   * The radio list orientation
   */
  orientation?: 'vertical' | 'horizontal';
  /**
   * Flag to indicate if the field is disabled
   */
  disabled?: boolean;
  /**
   * Optional props of the radio group.
   */
  fieldProps?: Omit<
    RadioGroupProps,
    'options' | 'orientation' | 'disabled' | 'loading'
  >;
};

export const RadioGroupField: React.FC<RadioGroupFieldProps> = ({
  name,
  dataTest,
  fieldProps,
  options,
  orientation,
  disabled,
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
        render={({ field }) => (
          <RadioGroup
            {...fieldProps}
            {...field}
            loading={wrapperProps.loading}
            options={options}
            orientation={orientation}
            disabled={disabled}
          />
        )}
      />
    </FieldWrapper>
  );
};
