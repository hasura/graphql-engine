import React from 'react';
import { Controller, useFormContext } from 'react-hook-form';
import { FieldWrapper, FieldWrapperPassThroughProps } from '../FieldWrapper';
import { Select, SelectItemProps, SelectProps } from '../../base/Select';
import { Select as ThemeSelect } from '@radix-ui/themes';

export type SelectFieldProps = FieldWrapperPassThroughProps & {
  /**
   * The field name
   */
  name: string;
  /**
   * The options to display in the select
   */
  options: SelectItemProps[];
  /**
   * The placeholder text to display when the field is not valued
   */
  placeholder?: string;
  /**
   * Flag to indicate if the field is disabled
   */
  disabled?: boolean;
  /**
   * The default value of the field
   */
  defaultValue?: string;

  full?: boolean;

  fieldProps?: SelectProps['fieldProps'] & {
    root?: Omit<
      ThemeSelect.RootProps,
      'children' | 'disabled' | 'value' | 'name' | 'onValueChange'
    >;
  };
};

export const SelectField: React.FC<SelectFieldProps> = ({
  name,
  options,
  placeholder,
  dataTest,
  disabled = false,
  defaultValue,
  fieldProps,
  full,
  ...wrapperProps
}) => {
  const { control } = useFormContext();

  return (
    <Controller
      name={name}
      control={control}
      render={({ field, fieldState }) => (
        <FieldWrapper id={name} {...wrapperProps} error={fieldState.error}>
          <Select
            {...fieldProps?.root}
            {...field}
            full={full}
            placeholder={placeholder}
            defaultValue={defaultValue}
            options={options}
            invalid={fieldState.invalid}
            fieldProps={{
              content: fieldProps?.content,
              trigger: fieldProps?.trigger,
            }}
          />
        </FieldWrapper>
      )}
    />
  );
};
