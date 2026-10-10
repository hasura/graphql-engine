import Select, { GroupBase, Props } from 'react-select';
import { ReactSelectOptionType } from './types';
import { defaultReactSelectClassNames } from './utils';
import FormatReactSelectOptionLabel from './FormatReactSelectOptionLabel';

export type ReactSelectProps<
  Option,
  IsMulti extends boolean = false,
  Group extends GroupBase<Option> = GroupBase<Option>,
> = Omit<Props<Option, IsMulti, Group>, 'unstyle'> & {
  invalid?: boolean;
};

export const createMaybeTextOption = (value: string | null | undefined) =>
  value !== null && value !== undefined
    ? {
        label: value,
        value,
      }
    : undefined;

export function ReactSelect<
  Option extends ReactSelectOptionType = ReactSelectOptionType,
  IsMulti extends boolean = false,
>({
  invalid,
  isDisabled,
  formatOptionLabel,
  menuPortalTarget,
  ...props
}: ReactSelectProps<Option, IsMulti, GroupBase<Option>>) {
  return (
    <Select
      {...props}
      unstyled
      formatOptionLabel={formatOptionLabel ?? FormatReactSelectOptionLabel}
      classNames={defaultReactSelectClassNames(invalid)}
      menuPortalTarget={
        menuPortalTarget ?? window.document.getElementById('hasura-theme')
      }
    />
  );
}
