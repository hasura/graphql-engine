import {
  default as AsyncSelect,
  AsyncCreatableProps,
} from 'react-select/async-creatable';
import { GroupBase } from 'react-select';
import { defaultReactSelectClassNames, formatOptionLabel } from './utils';
import { ReactSelectOptionType } from './types';

export type ReactSelectCreatableProps<
  Option,
  IsMulti extends boolean = false,
  Group extends GroupBase<Option> = GroupBase<Option>,
> = Omit<
  AsyncCreatableProps<Option, IsMulti, Group>,
  'defaultOptions' | 'unstyled'
> & {
  isInvalid?: boolean;
};

export function ReactSelectCreatable<
  Option extends ReactSelectOptionType = ReactSelectOptionType,
  IsMulti extends boolean = false,
>({
  isInvalid,
  isDisabled,
  options,
  loadOptions,
  menuPortalTarget,
  ...props
}: ReactSelectCreatableProps<Option, IsMulti, GroupBase<Option>>) {
  return (
    <AsyncSelect
      {...props}
      defaultOptions={options ?? true}
      loadOptions={loadOptions}
      unstyled
      formatOptionLabel={formatOptionLabel}
      classNames={defaultReactSelectClassNames(isInvalid)}
      menuPortalTarget={
        menuPortalTarget ?? window.document.getElementById('hasura-theme')
      }
    />
  );
}
