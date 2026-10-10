import React from 'react';
import { unsupportedRawSQLDrivers } from './utils';
import { Select, SelectItemProps } from '@hasura/shared/ui';

type DropdownOption = {
  driver: string;
  name: string;
};

interface Props {
  options: DropdownOption[];
  defaultValue: string | '';
  onChange: (option: string) => void;
  className?: string;
  styles?: React.CSSProperties;
}

const DropDownSelector: React.FC<Props> = ({
  options,
  defaultValue,
  onChange,
  ...props
}) => {
  const handleValueChange = (value: string) => {
    onChange(value);
  };

  const optionProps: SelectItemProps[] = options.map(
    (item: DropdownOption, index: number): SelectItemProps =>
      unsupportedRawSQLDrivers.includes(item.driver)
        ? {
            label: `${item.name} (${item.driver} is not supported)`,
            value: item.name,
            disabled: true,
          }
        : {
            label: item.name,
            value: item.name,
          },
  );

  return (
    <div {...props}>
      <Select
        name="data-source"
        onChange={handleValueChange}
        defaultValue={defaultValue}
        options={optionProps}
        full
      />
    </div>
  );
};

export default DropDownSelector;
