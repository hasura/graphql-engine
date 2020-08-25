import React, { useCallback } from 'react';

import SearchableSelect from '../../../../Common/SearchableSelect/SearchableSelect';

type Option = {
  value: string;
  label: string;
  description?: string;
  key: number;
  colIdentifier: number;
};

export interface ColumnTypeSelectorProps {
  options: Array<{ label: string; options?: Option[] }>;
  onChange: (option: any) => void;
  value: Option | string;
  bsClass: string;
  styleOverrides: Record<string, any>;
  colIdentifier: number;
}

export const ColumnTypeSelector: React.FC<ColumnTypeSelectorProps> = ({
  options,
  onChange,
  value,
  bsClass,
  styleOverrides,
  colIdentifier,
}) => {
  const createOpt = useCallback(
    (prevValue: string) => ({
      value: prevValue,
      label: prevValue,
      colIdentifier,
    }),
    [colIdentifier]
  );

  return (
    <SearchableSelect
      isCreatable
      createNewOption={createOpt}
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
