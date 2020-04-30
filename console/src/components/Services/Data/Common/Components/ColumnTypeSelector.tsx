import React, { useState } from 'react';

import SearchableSelect from '../../../../Common/SearchableSelect/SearchableSelect';

const createOpt = (prevValue: string, index: number) => ({
  value: prevValue,
  label: prevValue,
  colIdentifier: index,
});

type Option = {
  value: string;
  label: string;
  description?: string;
  key: number;
  colIdentifier: number;
};

type Props = {
  options: Array<{ label: string; options: Option[] }>;
  onChange: (option: any) => void;
  value: Option | string;
  bsClass: string;
  styleOverrides: Record<string, any>;
  colIdentifier: number;
};

export const ColumnTypeSelector: React.FC<Props> = ({
  options,
  onChange,
  value,
  bsClass,
  styleOverrides,
  colIdentifier,
}) => {
  const [searchValue, setSearchValue] = useState('');

  const onMenuClose = () => {
    if (searchValue !== '') onChange(createOpt(searchValue, colIdentifier));
    setSearchValue('');
  };

  // Creating new option based on input
  if (searchValue !== '') {
    options = [createOpt(searchValue, colIdentifier) as any, ...options]; // todo
  }

  return (
    <SearchableSelect
      onInputChange={(v: string) => setSearchValue(v)}
      // Treating last search value the same was as selected option,
      // so that user don't have to click in the dropdown, they can just leave the input
      onMenuClose={onMenuClose}
      options={options}
      onChange={onChange}
      value={value}
      bsClass={bsClass}
      styleOverrides={styleOverrides}
      filterOption="prefix"
      placeholder="column_type"
    />
  );
};
