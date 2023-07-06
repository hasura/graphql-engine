import React from 'react';
import { FaSearch } from 'react-icons/fa';
import { Input } from '../../../../new-components/Form';

type SearchBarProps = {
  onSearch: (searchText: string) => void;
  defaultValue?: string;
};

export const SearchBar = ({ onSearch, defaultValue }: SearchBarProps) => {
  const timer = React.useRef<ReturnType<typeof setTimeout> | null>(null);
  const [value, setValue] = React.useState(defaultValue ?? '');
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
