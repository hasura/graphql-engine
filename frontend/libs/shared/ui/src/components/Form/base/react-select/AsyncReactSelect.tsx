import AsyncSelect, { type AsyncProps } from 'react-select/async';
import type { GroupBase } from 'react-select';
import { ReactSelectOptionType } from './types';
import { createTextOption, defaultReactSelectClassNames } from './utils';
import FormatReactSelectOptionLabel from './FormatReactSelectOptionLabel';

export type AsyncReactSelectProps<
  Option,
  IsMulti extends boolean = false,
  Group extends GroupBase<Option> = GroupBase<Option>,
> = Omit<
  AsyncProps<Option, IsMulti, Group>,
  'options' | 'filterOption' | 'unstyle'
> & {
  invalid?: boolean;
};

export const createFilteredTextOptions =
  (
    options: string[],
  ): AsyncReactSelectProps<ReactSelectOptionType>['loadOptions'] =>
  (inputValue, callback) => {
    const lowerValue = inputValue ? inputValue.toLowerCase() : inputValue;
    const filteredOptions = inputValue
      ? options.filter((option) => option.toLowerCase().includes(lowerValue))
      : options;
    callback(filteredOptions.map(createTextOption));
  };

export function AsyncReactSelect<
  Option extends ReactSelectOptionType = ReactSelectOptionType,
  IsMulti extends boolean = false,
>({
  invalid,
  isDisabled,
  defaultOptions,
  formatOptionLabel,
  menuPortalTarget,
  ...props
}: AsyncReactSelectProps<Option, IsMulti, GroupBase<Option>>) {
  return (
    <AsyncSelect
      {...props}
      defaultOptions={defaultOptions ?? true}
      unstyled
      formatOptionLabel={formatOptionLabel ?? FormatReactSelectOptionLabel}
      classNames={defaultReactSelectClassNames(invalid)}
      menuPortalTarget={
        menuPortalTarget ?? window.document.getElementById('hasura-theme')
      }
    />
  );
}
