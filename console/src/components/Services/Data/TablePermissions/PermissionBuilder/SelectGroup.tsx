import React from 'react';

import styles from './PermissionBuilder.scss';
import { addToPrefix } from './utils';

interface SelectGroupProps {
  selectDispatchFunc: (value: string) => void;
  value: string;
  values: Record<string, string[]>;
  prefix?: string;
  disabledValues?: string[];
}

const SelectGroup: React.FC<SelectGroupProps> = ({
  selectDispatchFunc,
  value,
  values,
  prefix = '',
  disabledValues = [],
}) => {
  const dispatchSelect = (e: React.ChangeEvent<HTMLSelectElement>) => {
    selectDispatchFunc(e.target.value);
  };

  const selectOptions = [];

  selectOptions.push(<option value="">--</option>);

  Object.entries(values)
    .sort((a, b) => (a[0] > b[0] ? 1 : -1))
    .forEach(([key, val]) => {
      if (val?.length) {
        selectOptions.push(
          <optgroup label={key}>
            {val.map((v, i) => (
              <option
                value={addToPrefix(prefix, v)}
                key={i}
                disabled={disabledValues.includes(v)}
              >
                {v || '--'}
              </option>
            ))}
          </optgroup>
        );
      }
    });
  const selectedValue = addToPrefix(prefix, value || '--');

  return (
    <span>
      &quot;&nbsp;
      <select
        value={selectedValue}
        name={prefix}
        onChange={dispatchSelect}
        className={styles.qb_select}
        data-test="qb-select"
      >
        {selectOptions}
      </select>
      &nbsp;&quot;
    </span>
  );
};

export default SelectGroup;
