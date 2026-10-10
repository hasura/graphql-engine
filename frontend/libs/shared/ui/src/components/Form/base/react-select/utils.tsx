import { ReactSelectOptionType } from './types';
import React from 'react';
import { createFilter, FilterOptionOption, GroupBase } from 'react-select';
import clsx from 'clsx';
import type { AsyncProps } from 'react-select/async';

import './react-select.css';

export const REACT_SELECT_FILTER_PROPS = {
  isSearchable: true,
  filterOption: createFilter({
    ignoreCase: true,
    matchFrom: 'any',
  }),
} as const;

export const filterOptionByLabel = (
  option: FilterOptionOption<ReactSelectOptionType>,
  rawInput: string,
): boolean => {
  if (!rawInput) {
    return true;
  }

  return option.label.toLowerCase().includes(rawInput.toLowerCase());
};

export const createTextOption = <T extends string = string>(value: T) => ({
  label: value,
  value,
});

export function formatOptionLabel<V = any>({
  label,
  icon,
}: ReactSelectOptionType<V>): React.ReactNode {
  return (
    <div>
      {icon && (
        <span className="mr-2 -translate-y-0.5 inline-block">
          {React.createElement(icon)}
        </span>
      )}
      <span>{label}</span>
    </div>
  );
}

export function formatGroupLabel<
  Option = any,
  Group extends GroupBase<Option> = GroupBase<Option>,
>(data: Group) {
  return <div>{data.label}</div>;
}

/**
 * Mirrors the Radix Themes `Select` (size 2, surface trigger, solid content)
 * using only Radix tokens, so react-select follows the light/dark appearance
 * like every other Radix field. The menu is portaled into `#hasura-theme`, so
 * the tokens resolve there too.
 */
export const defaultReactSelectClassNames = (
  isInvalid: boolean | undefined,
): AsyncProps<any, any, any>['classNames'] => ({
  container: ({ isDisabled }) =>
    clsx(
      'block text-[length:var(--font-size-2)] leading-[var(--line-height-2)]',
      'rounded-[max(var(--radius-2),var(--radius-full))]',
      'focus-within:outline-2 focus-within:-outline-offset-1',
      isInvalid
        ? 'focus-within:outline-[var(--red-8)]'
        : 'focus-within:outline-[var(--focus-8)]',
      isDisabled
        ? clsx(
            'pointer-events-none bg-[var(--gray-a2)] text-[var(--gray-a11)]',
            'shadow-[inset_0_0_0_1px_var(--gray-a6)]',
          )
        : clsx(
            'bg-[var(--color-surface)] text-[var(--gray-12)]',
            isInvalid
              ? 'shadow-[inset_0_0_0_1px_var(--red-a8)]'
              : 'shadow-[inset_0_0_0_1px_var(--gray-a7)] hover:shadow-[inset_0_0_0_1px_var(--gray-a8)]',
          ),
    ),
  control: () => 'min-h-auto!',
  valueContainer: (state) =>
    clsx(
      'min-h-[var(--space-6)] pl-[var(--space-3)] gap-1',
      state.isDisabled ? 'pointer-events-none' : '',
    ),
  placeholder: () => 'text-[var(--gray-a10)]',
  multiValue: () =>
    clsx(
      'bg-[var(--gray-a3)] text-[var(--gray-12)]',
      'rounded-[var(--radius-1)] gap-1 pl-2',
    ),
  multiValueRemove: () =>
    clsx(
      'px-1 rounded-r-[var(--radius-1)] text-[var(--gray-a11)]',
      'hover:bg-[var(--red-a4)] hover:text-[var(--red-11)]',
    ),
  menu: () =>
    clsx(
      'my-1 p-[var(--space-2)] min-w-64 overflow-hidden',
      'rounded-[var(--radius-4)] bg-[var(--color-panel-solid)] shadow-[var(--shadow-5)]',
      'text-[length:var(--font-size-2)] text-[var(--gray-12)]',
      'divide-y divide-[var(--gray-a6)]',
      'focus:outline-none',
    ),
  groupHeading: () =>
    clsx(
      'flex items-center min-h-[var(--space-6)] px-[var(--space-5)]',
      'text-[var(--gray-a10)]',
    ),
  option: (state) =>
    clsx(
      'relative flex items-center whitespace-nowrap rounded-[var(--radius-2)]',
      'cursor-pointer py-2 px-6',
      state.isFocused
        ? 'bg-[var(--accent-9)] text-[var(--accent-contrast)]'
        : '',
      state.isDisabled ? 'text-[var(--gray-a8)] cursor-default' : '',
      // Radix-style check mark in the left indicator gutter (react-select.css).
      state.isSelected ? 'hasura-react-select-option--selected' : '',
    ),
  noOptionsMessage: () => 'text-[var(--gray-a10)] p-4',
  loadingMessage: () => 'text-[var(--gray-a10)] p-4',
  indicatorsContainer: () => 'text-[var(--gray-12)] opacity-90 px-2 gap-1',
  indicatorSeparator: () => 'hidden',
  clearIndicator: () => 'w-[14px] hover:text-[var(--red-11)]',
  dropdownIndicator: () => 'w-[14px]',
});

export const getDialogPortalTarget = () =>
  window.document.querySelector('.rt-DialogOverlay') as HTMLDivElement;
