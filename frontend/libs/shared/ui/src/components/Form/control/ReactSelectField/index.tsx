import get from 'lodash/get';
import { Controller, FieldError, useFormContext } from 'react-hook-form';
import { FieldWrapper, FieldWrapperPassThroughProps } from '../FieldWrapper';
import { ReactSelectOptionType } from '../../base/react-select/types';
import ReactSelect, {
  GroupBase,
  PropsValue,
  Props as ReactSelectProps,
} from 'react-select';
import CreatableSelect from 'react-select/creatable';
import isEqual from 'lodash/isEqual';
import { defaultReactSelectClassNames } from '../../base/react-select/utils';
import FormatReactSelectOptionLabel from '../../base/react-select/FormatReactSelectOptionLabel';

const includesObject = (arr: any[], obj: any) => {
  return arr?.some && arr.some((item) => isEqual(item, obj));
};

// Only an array value (multi select) can match several options. A string
// value must match exactly: `'jsonb'.includes('json')` is a substring match.
const isOptionValueSelected = (rawValue: any, optionValue: any) =>
  rawValue === optionValue ||
  isEqual(rawValue, optionValue) ||
  (Array.isArray(rawValue) &&
    (rawValue.includes(optionValue) || includesObject(rawValue, optionValue)));

export type ReactSelectFieldProps<
  Option extends ReactSelectOptionType = ReactSelectOptionType,
  IsMulti extends boolean = false,
  Group extends GroupBase<Option> = GroupBase<Option>,
> = FieldWrapperPassThroughProps & {
  /**
   * The field name
   */
  name: string;
  /**
   * The options to display in the select
   */
  options: ReactSelectProps<Option, IsMulti, Group>['options'];
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
   * Flag to also accept a typed value that isn't one of the options
   */
  creatable?: boolean;
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

export function ReactSelectField<
  Option extends ReactSelectOptionType = ReactSelectOptionType,
>({
  name,
  options,
  placeholder,
  dataTest,
  disabled = false,
  multi = false,
  creatable = false,
  selectProps,
  value: propValue,
  ...props
}: ReactSelectFieldProps<Option>) {
  const {
    control,
    formState: { errors },
  } = useFormContext();
  const maybeError = get(errors, name) as FieldError | undefined;

  return (
    <FieldWrapper id={name} {...props} error={maybeError}>
      {() => (
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
              optionsReference: typeof options,
            ): PropsValue<ReactSelectOptionType['value']> | undefined => {
              return optionsReference
                ?.map((option) => {
                  if ((option as GroupBase<Option>)?.options) {
                    return (
                      (option as GroupBase<Option>).options as Option[]
                    ).filter((innerOption) =>
                      isOptionValueSelected(rawValue, innerOption?.value),
                    );
                  } else {
                    return option;
                  }
                })
                .flat()
                .filter(
                  (
                    option:
                      ReactSelectOptionType | GroupBase<ReactSelectOptionType>,
                  ) => {
                    if ((option as GroupBase<ReactSelectOptionType>).options) {
                      return true;
                    } else {
                      const simpleOption = option as ReactSelectOptionType;
                      return isOptionValueSelected(
                        rawValue,
                        simpleOption.value,
                      );
                    }
                  },
                );
            };

            // a typed value isn't one of the options: add it so it stays
            // selected
            const allOptions =
              creatable &&
              !multi &&
              typeof value === 'string' &&
              value &&
              !extractValue(value, options)?.length
                ? [...(options ?? []), { value, label: value } as Option]
                : options;

            const Select = creatable ? CreatableSelect : ReactSelect;

            return (
              <Select
                {...selectProps}
                aria-labelledby={name}
                name={controllerName}
                placeholder={placeholder}
                isDisabled={disabled}
                isMulti={multi}
                options={allOptions}
                value={extractValue(value, allOptions)}
                formatCreateLabel={(inputValue: string) =>
                  `Use "${inputValue}"`
                }
                data-test={dataTest}
                data-testid={name}
                onChange={handleChange}
                unstyled
                formatOptionLabel={
                  selectProps?.formatOptionLabel ?? FormatReactSelectOptionLabel
                }
                classNames={defaultReactSelectClassNames(Boolean(maybeError))}
                menuPortalTarget={
                  selectProps?.menuPortalTarget ??
                  window.document.getElementById('hasura-theme')
                }
                menuPosition={selectProps?.menuPosition ?? 'fixed'}
              />
            );
          }}
        />
      )}
    </FieldWrapper>
  );
}
