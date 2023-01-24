import React from 'react';
import { Controller, FieldError, useFormContext } from 'react-hook-form';
import get from 'lodash.get';
import { FieldWrapper } from './FieldWrapper';
import { AdvancedSelect, AdvancedSelectProps } from './AdvancedSelect';

export const AdvancedSelectField: React.VFC<AdvancedSelectProps> = ({
  dataTest,
  disabled = false,
  isMulti = false,
  name,
  options,
  placeholder,
  value,
  ...wrapperProps
}) => {
  const {
    register,
    formState: { errors },
    watch,
    control,
  } = useFormContext();

  const maybeError: FieldError | undefined = get(errors, name);

  const { onBlur, ref } = register(name);
  const watchValue = watch(name);

  return (
    <FieldWrapper id={name} {...wrapperProps} error={maybeError}>
      {/* Based on https://react-hook-form.com/get-started/#IntegratingwithUIlibraries */}
      <Controller
        name={name}
        control={control}
        render={({ field }) => (
          <AdvancedSelect
            dataTest={dataTest}
            disabled={disabled}
            field={field}
            isMulti={isMulti}
            name={name}
            onBlur={onBlur}
            options={options}
            placeholder={placeholder}
            ref={ref}
            value={value || watchValue}
          />
        )}
      />
    </FieldWrapper>
  );
};
