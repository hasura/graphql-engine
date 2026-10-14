import React from 'react';
import { FaSearch } from 'react-icons/fa';
import { Input, InputProps } from './Input';
import { useDebouncedEffect } from '@hasura/shared/hooks';

export type SearchInputProps = Omit<
  InputProps,
  'onClear' | 'onChange' | 'onInput' | 'value'
> & {
  onSearch: (searchText: string) => void;
  value?: string;
  delay?: number;
};

export const SearchInput = ({
  onSearch,
  delay = 500,
  icon = FaSearch,
  iconPosition = 'start',
  clearable = true,
  value,
  ...rest
}: SearchInputProps) => {
  const [searchValue, setSearchValue] = React.useState<string>(value ?? '');

  useDebouncedEffect(
    () => {
      if (value !== searchValue) {
        onSearch(searchValue);
      }
    },
    delay,
    [searchValue],
  );

  // Sync the controlled `value` prop into local state during render rather than
  // in an effect, avoiding an extra commit.
  const [prevValue, setPrevValue] = React.useState(value);
  if (value !== prevValue) {
    setPrevValue(value);
    if (value !== searchValue) {
      setSearchValue(value ?? '');
    }
  }

  return (
    <Input
      {...rest}
      icon={icon}
      iconPosition={iconPosition}
      clearable={clearable}
      value={value}
      onChange={(e) => {
        setSearchValue(e.target.value);
      }}
      onClear={() => {
        setSearchValue('');
      }}
    />
  );
};
