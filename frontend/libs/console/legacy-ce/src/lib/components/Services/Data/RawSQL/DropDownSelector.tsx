import React from 'react';
import styles from '../../../Common/TableCommon/Table.module.scss';

import { unsupportedRawSQLDrivers } from './utils';

type DropdownOption = {
  driver: string;
  name: string;
};
interface Props {
  options: DropdownOption[];
  defaultValue: string | '';
  onChange: (option: string) => void;
  styles: React.CSSProperties;
}

const DropDownSelector: React.FC<Props> = ({
  options,
  defaultValue,
  onChange,
  ...props
}) => {
  const handleValueChange = (event: React.ChangeEvent<HTMLSelectElement>) => {
    event.persist();
    onChange(event.target.value);
  };

  const renderOptions = () => {
    if (
      options?.length === 1 &&
      unsupportedRawSQLDrivers?.includes(options[0].driver)
    ) {
      return (
        <option
          value={options[0].driver}
        >{`${options[0].name} (${options[0].driver} is not supported)`}</option>
      );
    }

    return options.map((item: DropdownOption, index: number) =>
      unsupportedRawSQLDrivers.includes(item.driver) ? (
        <option
          disabled
          key={index}
        >{`${item.name} (${item.driver} is not supported)`}</option>
      ) : (
        <option key={index}>{item.name}</option>
      )
    );
  };

  return (
    <div style={props.styles}>
      <select
        className={`${styles.select} ${styles.sample} form-control ${styles.add_pad_left}`}
        name="data-source"
        onChange={handleValueChange}
        defaultValue={defaultValue}
      >
        {renderOptions()}
      </select>
    </div>
  );
};

export default DropDownSelector;
