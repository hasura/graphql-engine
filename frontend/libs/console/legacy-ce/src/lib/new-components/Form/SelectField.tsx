import React from 'react';
import get from 'lodash/get';
import { Controller, FieldError, useFormContext } from 'react-hook-form';
import { FieldWrapper, FieldWrapperPassThroughProps } from './FieldWrapper';
import {
  ReactSelect,
  ReactSelectProps,
  ReactSelectOptionType,
} from './ReactSelect';
import { Option } from 'react-select/src/filters';
import { GroupBase, PropsValue } from 'react-select';
import isEqual from 'lodash/isEqual';

const includesObject = (arr: any[], obj: any) => {
  return arr?.some && arr.some(item => isEqual(item, obj));
};

export type SelectFieldProps = FieldWrapperPassThroughProps & {
  /**
   * The field name
   */
  name: string;
  /**
   * The options to display in the select
   */
  options: ReactSelectProps<ReactSelectOptionType>['options'];
  /**
   * The placeholder text to display when the field is not valued
   */
  placeholder?: string;
  /**
   * Flag to indicate if the field is disabled
   */
  disabled?: boolean;
  /**
   * Flag to indicate if the field is multi-select
   */
  multi?: boolean;
  /**
   * The value of the field
   */
  value?: PropsValue<ReactSelectOptionType['value']> | undefined;
  /**
   * The default value of the field
   */
  defaultValue?: PropsValue<ReactSelectOptionType['value']> | undefined;
  /**
   * The wrapped react-select component props
   */
  selectProps?: Omit<
    ReactSelectProps<ReactSelectOptionType>,
    'options' | 'value' | 'name' | 'isDisabled' | 'isMulti' | 'defaultValue'
  >;
};

export const SelectField: React.VFC<SelectFieldProps> = ({
  name,
  options,
  placeholder,
  dataTest,
  disabled = false,
  multi = false,
  selectProps,
  value: propValue,
  ...props
}) => {
  const {
    control,
    formState: { errors },
  } = useFormContext();
  const maybeError = get(errors, name) as FieldError | undefined;

  return (
    <FieldWrapper id={name} {...props} error={maybeError}>
      <Controller
        name={name}
        control={control}
        render={({ field: { value, name: controllerName, onChange } }) => {
          const handleChange = (val: any) => {
            if (multi) {
              onChange(val.map((v: any) => v?.value));
            } else {
              onChange(val?.value);
            }
          };

          const extractValue = (
            rawValue: PropsValue<ReactSelectOptionType['value']> | undefined,
            optionsReference: typeof options
          ): PropsValue<ReactSelectOptionType['value']> | undefined => {
            return optionsReference
              ?.map(option => {
                if ((option as GroupBase<Option>)?.options) {
                  return (
                    (option as GroupBase<Option>).options as Option[]
                  ).filter(
                    innerOption =>
                      rawValue === innerOption?.value ||
                      isEqual(rawValue, innerOption?.value) ||
                      (rawValue?.includes &&
                        rawValue?.includes(innerOption.value)) ||
                      includesObject(rawValue, innerOption.value)
                  );
                } else {
                  return option;
                }
              })
              .flat()
              .filter(
                (
                  option:
                    | ReactSelectOptionType
                    | GroupBase<ReactSelectOptionType>
                ) => {
                  if ((option as GroupBase<ReactSelectOptionType>).options) {
                    return true;
                  } else {
                    const simpleOption = option as ReactSelectOptionType;
                    return (
                      rawValue === simpleOption.value ||
                      isEqual(rawValue, simpleOption.value) ||
                      (rawValue?.includes &&
                        rawValue?.includes(simpleOption.value)) ||
                      includesObject(rawValue, simpleOption.value)
                    );
                  }
                }
              );
          };

          return (
            <ReactSelect
              aria-labelledby={name}
              name={controllerName}
              placeholder={placeholder}
              isDisabled={disabled}
              isInvalid={!!maybeError}
              isMulti={multi}
              options={options}
              value={extractValue(value, options)}
              data-test={dataTest}
              data-testid={name}
              onChange={handleChange}
            />
          );
        }}
      />
    </FieldWrapper>
  );
};
