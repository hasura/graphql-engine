import React from 'react';
import { FaSearch } from 'react-icons/fa';
import { Input } from '../../../../new-components/Form';

type SearchBarProps = {
  onSearch: (searchText: string) => void;
};

export const SearchBar = ({ onSearch }: SearchBarProps) => {
  const timer = React.useRef<ReturnType<typeof setTimeout> | null>(null);
  const [value, setValue] = React.useState('');
  return (
    <div className="flex gap-2">
      <Input
        name="searchText"
        icon={<FaSearch />}
        iconPosition="start"
        noErrorPlaceholder
        clearButton
        fieldProps={{ value: value }}
        onChange={e => {
          setValue(e.target.value);

          if (timer.current) clearTimeout(timer.current);

          timer.current = setTimeout(() => {
            onSearch(e.target.value);
          }, 20);
        }}
        onClearButtonClick={() => {
          setValue('');
          onSearch('');
        }}
      />
    </div>
  );
};
